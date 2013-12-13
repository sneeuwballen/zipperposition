
(*
Zipperposition: a functional superposition prover for prototyping
Copyright (c) 2013, Simon Cruanes
All rights reserved.

Redistribution and use in source and binary forms, with or without
modification, are permitted provided that the following conditions are met:

Redistributions of source code must retain the above copyright notice, this
list of conditions and the following disclaimer.  Redistributions in binary
form must reproduce the above copyright notice, this list of conditions and the
following disclaimer in the documentation and/or other materials provided with
the distribution.

THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS "AS IS" AND
ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED
WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE ARE
DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT HOLDER OR CONTRIBUTORS BE LIABLE
FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL
DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR
SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER
CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY,
OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE
OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
*)

(** {1 Elimination of Arithmetic} *)

open Logtk

module S = Symbol
module T = FOTerm
module AT = ArithTerm
module F = FOFormula
module C = Clause
module I = ProofState.TermIndex
module M = Monome
module PF = PFormula
module TO = Theories.TotalOrder
module Lit = Literal
module Lits = Literal.Arr

let prof_case_switch = Util.mk_profiler "arith.case_switch"
let prof_inner_case_switch = Util.mk_profiler "arith.inner_case_switch"
let prof_factor_bounds = Util.mk_profiler "arith.factor_bounds"

let stat_case_switch = Util.mk_stat "arith.case_switch"
let stat_inner_case_switch = Util.mk_stat "arith.inner_case_switch"
let stat_factor_bounds = Util.mk_stat "arith.factor_bounds"
let stat_bound_tauto = Util.mk_stat "arith.bound_tautology"
let stat_simplify_remainder = Util.mk_stat "arith.simplify_remainder"
let stat_infer_remainder_of_divisors = Util.mk_stat "arith.remainder_of_divisors"
let stat_enum_remainder_cases = Util.mk_stat "arith.enum_remainder_cases"
let stat_remainder_of_equality = Util.mk_stat "arith.remainder_of_eq"
let stat_remainder_of_var_ineq = Util.mk_stat "arith.remainder_of_var_ineq"

(** {2 Inference Rules} *)

let rewrite_lit ~ctx lit =
  let ord = Ctx.ord ~ctx in
  let lit' = ArithLit.simplify ~ord lit in
  if ArithLit.is_trivial lit'
    then Lit.mk_tauto
  else if ArithLit.has_instances lit'
    then lit'
    else Lit.mk_absurd

let eliminate_arith c =
  let ctx = c.C.hcctx in
  let ord = Ctx.ord ctx in
  let eligible = C.Eligible.max c in
  (* find new arrays of literals, after substitution *)
  let lits'_list = ArithLit.Arr.eliminate ~ord ~eligible c.C.hclits in
  (* turn new arrays into clauses, if needed *)
  List.fold_left
    (fun acc (subst,lits') ->
      if Lits.eq_com c.C.hclits lits'
        then acc
        else begin
          (* build new clause *)
          let info = [Substs.FO.to_string subst] in
          let proof cc = Proof.mk_c_inference
            ~theories:["arith";"equality"] ~info
            ~rule:"arith_instantiate" cc [c.C.hcproof] in
          let new_c = C.create_a ~parents:[c] ~ctx lits' proof in
          Util.debug 3 "arith instantiate %a into %a" C.pp c C.pp new_c;
          new_c :: acc
        end)
    [] lits'_list

let factor_arith c =
  let ctx = c.C.hcctx in
  let ord = Ctx.ord ctx in
  let eligible = C.Eligible.max c in
  (* instantiate the clause with subst *)
  let mk_instance subst =
    let renaming = Ctx.renaming_clear ~ctx in
    let lits' = Lits.apply_subst ~ord ~renaming subst c.C.hclits 0 in
    let proof cc = Proof.mk_c_inference ~theories:["arith"; "equality"]
      ~info:[Substs.FO.to_string subst] ~rule:"factor" cc [c.C.hcproof] in
    let new_c = C.create_a ~parents:[c] ~ctx lits' proof in
    Util.debug 3 "factor %a (with %a) into %a" C.pp c Substs.FO.pp subst C.pp new_c;
    new_c
  in
  (* try to factor arith literals *)
  Lits.fold_lits ~eligible c.C.hclits []
    (fun acc lit i ->
      try
        let elit = ArithLit.Canonical.extract lit in
        let substs = ArithLit.Canonical.factor elit in
        List.fold_left
          (fun acc subst -> mk_instance subst :: acc)
          acc substs
      with M.NotLinear -> acc)

let purify_arith c =
  let ctx = c.C.hcctx in
  let eligible = C.Eligible.(max c) in
  let lits' = ArithLit.Arr.purify ~ord:(Ctx.ord ctx) ~eligible c.C.hclits
  in
  if Lits.eq_com c.C.hclits lits'
    then []
    else begin
      let proof cc = Proof.mk_c_inference ~rule:"purify" cc [c.C.hcproof] in
      let new_c = C.create_a ~ctx ~parents:[c] lits' proof in
      [new_c]
    end

(* if a < t1, a < t2, .... a < tn occurs in clause, where t_i are all
   arithmetic constants, replace by  a < max_i(t_i) *)
let factor_bounds c =
  Util.enter_prof prof_factor_bounds;
  let ctx = c.C.hcctx in
  (* [bv] stores which literals should survive *)
  let bv = BV.create ~size:(Array.length c.C.hclits) true in
  (* literals seen as comparisons with monomes *)
  let lits = ArithLit.Arr.view_bounds c.C.hclits in
  assert (Array.length lits = Array.length c.C.hclits);
  (* remove literals that are redundant *)
  Array.iteri
    (fun i lit -> match lit with
    | `Ignore _ -> ()
    | `LowerBound (_, m, r) ->
      if Util.array_exists
        (function
          | `LowerBound (_, m', r') ->
            (* if m < r and m' < r and m > m', then m' < r is enough *)
            T.eq r r' && S.Arith.Op.greater m m'
          | _ -> false)
        lits
        then BV.reset bv i
    | `HigherBound (_, l, m) ->
      if Util.array_exists
        (function
          | `HigherBound (_, l', m') ->
            (* if l < m and l < m', then l < max(m,m') is enough *)
            T.eq l l' && S.Arith.Op.less m m'
          | _ -> false)
        lits
        then BV.reset bv i
    )
    lits;
  (* see whether some lits were removed *)
  if BV.cardinal bv < Array.length c.C.hclits then begin
    let lits' = BV.select bv c.C.hclits in
    let proof cc = Proof.mk_c_simp ~theories:["equality";"arith"]
      ~rule:"arith_factor_bounds" cc [c.C.hcproof] in
    let new_c = C.create ~parents:[c] ~ctx lits' proof in
    Util.debug 3 "arith_factor_bounds of %a gives %a" C.pp c C.pp new_c;
    assert (List.length lits' < Array.length c.C.hclits);
    Util.exit_prof prof_factor_bounds;
    Util.incr_stat stat_factor_bounds;
    new_c
  end else
    let _ = Util.exit_prof prof_factor_bounds in
    c  (* no change *)

(* enumerate integers from 0 to range. Bounds are excluded or included
    depending on params [strict_low] and [strict_high] (if true, bound is
    excluded). Returns an empty list is the range is empty. *)
let _int_range ~strict_low ~strict_high range =
  let rec enum acc i = match Big_int.compare_big_int i range with
    | 0 when not strict_high -> i :: acc (* include range *)
    | n when n >= 0 -> acc  (* gone too high, remember that range is excluded *)
    | _ -> enum (i::acc) (Big_int.succ_big_int i)
  in
  (* include lower bound? *)
  let start = Big_int.zero_big_int in
  let start = if strict_low then Big_int.succ_big_int start else start in
  enum [] start

(* new inference, kind of the dual of inequality chaining for integer
   inequalities. See the .mli file for more explanations. *)
let case_switch (active_set: ProofState.ActiveSet.t) c = []  (* TODO *)
  (*
  Util.enter_prof prof_case_switch;
  let ctx = active_set#ctx in
  let ord = Ctx.ord ctx in
  let spec = Ctx.total_order ctx in
  let instance = TO.tstp_instance ~spec in  (* $less, $lesseq *)
  (* do the case switch inference. c contains lower <= t,
      c' contains t <= lower+range. [s_foo] is the scope of [foo].
      Enumerates t = lower + i for i in 0 ... range *)
  let _do_case_switch c s_c i c' s_c' i' t s_t lower s_lower strict_low strict_high range subst acc =
    Util.debug 5 "case_switch between %a at %d and %a at %d" C.pp c i C.pp c' i';
    let renaming = Ctx.renaming_clear ~ctx in
    let lits_left = Util.array_except_idx c.C.hclits i in
    let lits_left = Lit.apply_subst_list ~renaming ~ord subst lits_left s_c in
    let lits_right = Util.array_except_idx c'.C.hclits i' in
    let lits_right = Lit.apply_subst_list ~renaming ~ord subst lits_right s_c' in
    let t' = Substs.FO.apply ~renaming subst t s_t in
    (* the case switch on t: for n=0...range, compute monome=lower+n, apply
      substitution to it, and add literal  t=monome *)
    let lits_case = List.map
      (fun n ->
        let monome = M.add_const lower (S.mk_bigint n) in
        let monome = M.apply_subst ~renaming subst monome s_lower in
        Lit.mk_eq ~ord t' (M.to_term monome))
      (_int_range ~strict_low ~strict_high range)
    in
    let new_lits = lits_left @ lits_right @ lits_case in
    let proof cc = Proof.mk_c_step cc ~rule:"arith_case_switch" [c.C.hcproof; c'.C.hcproof] in
    let parents = [c; c'] in
    let new_c = C.create ~parents ~ctx new_lits proof in
    Util.debug 5 "  --> case switch gives clause %a" C.pp new_c;
    Util.incr_stat stat_case_switch;
    new_c :: acc
  in
  (* view literals as arithmetic bounds when possible *)
  let lits = ArithLit.Arr.view_bounds c.C.hclits in
  (* fold on literals *)
  let new_clauses = Util.array_foldi
    (fun acc i lit -> match lit with
      | `Ignore _ -> acc
      | `LowerBound (strict_low, low, t) when Type.eq Type.int (M.ty low) ->
        (* low < r, see whether we can find high with r < high and
           high-low = constant *)
        I.retrieve_unifiables active_set#idx_ord_left 1 t 0 acc
          (fun acc _ with_pos subst ->
            try
              let c' = with_pos.C.WithPos.clause in
              let pos' = with_pos.C.WithPos.pos in
              let i' = List.hd pos' in
              let olit' = Lit.ineq_lit_of ~instance c'.C.hclits.(i') in
              let strict_high = olit'.TO.strict in
              (* see whether the right term is a monome *)
              let high = M.of_term olit'.TO.right in
              assert (Type.eq Type.int (M.ty high));
              (* compute range= subst(high)-subst(low) *)
              let renaming = Ctx.renaming_clear ~ctx in
              let low' = M.apply_subst ~renaming subst low 0 in
              let high' = M.apply_subst ~renaming subst high 1 in
              let range = M.difference high' low' in
              (* see whether range is a constant *)
              if M.is_const range
              then match range.M.const with
                | S.Int range ->
                  (* ok, found a high bound, such that high-low is a constant,
                      so t ranges from low to high. Typing ensures that
                      high is also an integer monome. *)
                  _do_case_switch c 0 i c' 1 i' t 0 low 0 strict_low strict_high range subst acc
                | _ -> assert false
              else acc
            with Exit | Not_found | M.NotLinear ->
              acc)
      | `LowerBound _ ->
        acc  (* other lower bounds are ignored *)
      | `HigherBound (strict_high, t, high) when Type.eq Type.int (M.ty high) ->
        (* converse case (symmetric of the `LowerBound case) *)
        I.retrieve_unifiables active_set#idx_ord_right 1 t 0 acc
          (fun acc _ with_pos subst ->
            try
              let c' = with_pos.C.WithPos.clause in
              let pos' = with_pos.C.WithPos.pos in
              let i' = List.hd pos' in
              let olit' = Lit.ineq_lit_of ~instance c'.C.hclits.(i') in
              let strict_low = olit'.TO.strict in
              (* see whether the left term is a monome *)
              let low = M.of_term olit'.TO.left in
              assert (Type.eq Type.int (M.ty low));
              (* compute range= subst(high)-subst(low) *)
              let renaming = Ctx.renaming_clear ~ctx in
              let low' = M.apply_subst ~renaming subst low 1 in
              let high' = M.apply_subst ~renaming subst high 0 in
              let range = M.difference high' low' in
              (* see whether range is a constant *)
              if M.is_const range
              then match range.M.const with
                | S.Int range ->
                  (* ok, found a high bound, such that high-low is a constant,
                      so t ranges from low to high. Typing ensures that
                      high is also an integer monome. *)
                _do_case_switch c' 1 i' c 0 i t 0 low 1 strict_low strict_high range subst acc
                | _ -> assert false
              else acc
            with Exit | Not_found | M.NotLinear ->
              acc)
      | `HigherBound _ ->
        acc (* other higher bounds are ignored *)
      )
    [] lits
  in
  Util.exit_prof prof_case_switch;
  new_clauses
  *)

let inner_case_switch c =
  let ctx = c.C.hcctx in
  let ord = Ctx.ord ctx in
  (* do the case switch, removing lits i and j, adding t=low+k
     where k in [0... range]
     FIXME: need a coefficient in front of [t]? *)
  let _do_case_switch i j t low range strict_low strict_high subst acc =
    Util.debug 5 "inner_case_switch in %a: %a in (%a,%a+%s) subst is %a"
      C.pp c T.pp t S.pp low S.pp low
      (Big_int.string_of_big_int range) Substs.FO.pp subst;
    (* remove lits i and j *)
    let lits = Util.array_foldi
      (fun acc i' lit -> if i' <> i && i' <> j then lit :: acc else acc)
      [] c.C.hclits
    in
    List.fold_left
      (fun acc k ->
        let renaming = Ctx.renaming_clear ~ctx in
        let t' = Substs.FO.apply ~renaming subst t 0 in
        let lit =
          Literal.mk_neq ~ord t'
            (T.mk_const (S.Arith.Op.sum low (S.mk_bigint k)))
        in
        let lits' = Literal.apply_subst_list ~ord ~renaming subst lits 0 in
        let new_lits = lit :: lits' in
        let proof cc = Proof.mk_c_inference ~theories:["arith";"equality"]
          ~rule:"arith_inner_case_switch" cc [c.C.hcproof] in
        let parents = [c] in
        let new_clause = C.create ~parents ~ctx new_lits proof in
        Util.debug 5 "  --> inner case switch gives clause %a" C.pp new_clause;
        new_clause :: acc)
      acc (_int_range ~strict_low ~strict_high range)
  in
  (* view literals as arithmetic bounds when possible *)
  let lits = ArithLit.Arr.view_bounds c.C.hclits in
  (* fold on literals *)
  let new_clauses = Util.array_foldi
    (fun acc i lit -> match lit with
      | `Ignore _ -> acc
      | `LowerBound (strict_high, high, t) when Type.eq Type.int (T.ty t) ->
        (* take the negation: high < t is the constraint  t <= high *)
        let strict_high = not strict_high in
        Util.array_foldi
          (fun acc j lit' ->
            if j = i then acc else match lit' with
            | `HigherBound (strict_low, t', low) when Type.eq Type.int (T.ty t') ->
              let strict_low = not strict_low in
              begin try
                (* unify t and t', see if it makes for a proper framing of t *)
                let subst = FOUnif.unification t 0 t' 0 in
                let range = S.Arith.Op.difference high low in
                match range with
                | S.Int range ->
                  _do_case_switch i j t low range strict_low strict_high subst acc
                | _ -> assert false
              with FOUnif.Fail -> acc
              end
            | _ -> acc
          )
          acc lits
      | `LowerBound _
      | `HigherBound _ -> acc
      )
    [] lits
  in
  Util.exit_prof prof_case_switch;
  new_clauses

(* if c =  a < t or t < b or c'  with b > a, it's a tautology *)
let bounds_are_tautology c = false (* TODO *)
  (*
  let ctx = c.C.hcctx in
  let lits = ArithLit.Arr.view_bounds c.C.hclits in
  let res = Util.array_exists
    (function
      | `LowerBound (strict_low, l, t) ->
        Util.array_exists
          (function
            | `HigherBound (strict_high, t', r) when T.eq t t' ->
              let both_strict = strict_low && strict_high in
              begin match M.comparison l r with
              | Comparison.Incomparable -> false
              | Comparison.Eq ->
                not both_strict (* n < t | t <= n is redundant *)
              | Comparison.Lt -> true
              | Comparison.Gt ->
                (not both_strict) && Type.eq (T.ty t) Type.int && M.(eq (succ l) r)
                  (* l <= t | t <= l+1 is redundant in integers *)
              end
            | _ -> false)
          lits
      | _ -> false)
    lits
  in
  if res then begin
    Util.debug 2 "redundant clause %a: bounds are tautology" C.pp c;
    Util.incr_stat stat_bound_tauto;
    end;
  res
  *)

(** {2 Modular Integer Arithmetic} *)

let _view_lit_as_remainder lit = match lit with
  | Literal.Equation ({T.term=T.Node(s, _, [t;n])}, c, sign, _)
  | Literal.Equation (c, {T.term=T.Node(s, _, [t;n])}, sign, _)
    when S.eq s S.Arith.remainder_e
    && AT.is_arith_const n
    && AT.is_arith_const c
    && Type.eq Type.int (T.ty t) ->
    let n, c = T.head n, T.head c in
    (* last check: must have a positive int *)
    if S.Arith.sign n <= 0 then `Ignore lit else `EqMod (t, n, c, sign)
  | _ -> `Ignore lit

let _view_lits_as_remainder lits =
  Array.map _view_lit_as_remainder lits

(* convert view back into a proper literal *)
let _mk_remainder ~ord lit = match lit with
  | `EqMod (t, n, c, sign) ->
    Lit.mk_lit ~ord (AT.mk_remainder_e t n) (T.mk_const c) sign
  | `Ignore lit' -> lit'  (* keep same lit *)
  | `True -> Lit.mk_tauto
  | `False -> Lit.mk_absurd

let simplify_remainder_term ~tyargs f l =
  match l with
  | [t; n] when S.eq f S.Arith.remainder_e && AT.is_arith_const n ->
    let n = T.head n in
    (* remainder(t,n) where n is a const *)
    begin try
      let e = M.of_term t in
      begin match M.factorize e with
        | None -> None
        | Some (e', coeff) ->
          (* e = e' * coeff, apply the distributivity rule that states
            that  (a*b) mod n = ((a mod n) * (b mod n)) mod n *)
          let coeff' = S.Arith.Op.remainder_e coeff n in
          let e' = M.product e' coeff' in
          let t' = M.to_term e' in
          if T.eq t t'
            then None
            else Some (AT.mk_remainder_e t' n)
      end
    with M.NotLinear -> None
    end
  | _ -> None

(* basic simplifications on equational literals of shape "a mod b ?= c" *)
let simplify_remainder_lit ~ctx lit =
  let ord = Ctx.ord ctx in
  let lit' = _view_lit_as_remainder lit in
  let lit' = match _view_lit_as_remainder lit with
  | `Ignore _ -> lit'
  | `EqMod (_, n, c, sign) when S.Arith.is_one n ->
    if S.Arith.is_zero c = sign
      then `True   (* t mod 1 = 0, or t mod 1 != c *)
      else `False  (* opposite case, absurd *)
  | `EqMod (t, n, c, sign) ->
    try
      let e = M.of_term t in
      (* first move [c] into [e] *)
      let e' = M.add_const e (S.Arith.Op.uminus c) in
      (* see whether [t] is [n * t'] *)
      if M.divisible e' n
        (* if [MA.Expr.divides e n] then it's tautology/absurd
          depending on sign  (e.g. 3a mod 3 = 0 is true). *)
        then if sign
          then `True
          else `False
        else
          let e' = match M.factorize e' with
          | None ->  e'
          | Some (e'', s) ->
            (* [e = e' × s], decompose [s] into prime factors and
              remove the factors of [s] that are prime with [n]. To do
              this, just compute gcd(s,n), because s = s' . gcd(s,n) with
              s' prime with n.
              Ex: [6t+4 mod 2 ---> (3t+2 mod 2]. *)
            let gcd = S.Arith.Op.gcd s n in
            assert (S.Arith.sign gcd > 0);
            M.product e'' gcd
          in
          (* move constant back to rhs *)
          let c' = M.Modulo.modulo ~n (S.Arith.Op.uminus e'.M.const) in
          let e' = M.remove_const e' in
          `EqMod (M.to_term e', n, c', sign)
      with M.NotLinear ->
        `Ignore lit
  in
  _mk_remainder ~ord lit'

(* infer remainders of divisors of [n], given remainders modulo [n].
  in other words, a mod 6 = 0 ===> a mod 2 = 0  and a mod 3 = 0 *)
let infer_remainder_of_divisors c =
  let ctx = c.C.hcctx in
  let ord = Ctx.ord ctx in
  let lits = _view_lits_as_remainder c.C.hclits in
  Util.array_foldi
    (fun acc i lit -> match lit with
      | `EqMod (t, n, const, true) when Type.eq Type.int (T.ty t) ->
        (* compute set of divisors of [n], then for each such [d]
           replace by [t-c mod d = 0] *)
        let divisors = match n with
          | S.Int n -> S.Arith.Op.divisors n
          | _ -> assert false
        in
        (* put const on lhs *)
        let t' = AT.mk_difference t (T.mk_const const) in
        List.fold_left
          (fun acc n' ->
            let lits' = Util.array_except_idx c.C.hclits i in
            let lits' = Lit.mk_eq ~ord
              (AT.mk_remainder_e t' (Symbol.mk_bigint n'))
              (T.mk_const S.Arith.zero_i) :: lits'
            in
            let parents = [c] in
            let proof cc = Proof.mk_c_inference
              ~theories:["arith"; "equality"]
              ~rule:"remainder_divisor" cc [c.C.hcproof] in
            let new_c = C.create ~ctx ~parents lits' proof in
            Util.debug 5 "remainder_of_divisors: from %a  deduce %a" C.pp c C.pp new_c;
            Util.incr_stat stat_infer_remainder_of_divisors;
            new_c :: acc
          ) acc divisors
      | `EqMod (t, n, const, false) when Type.eq Type.int (T.ty t) ->
        (* replace [t - c mod n != 0] by [Or_d (t - c) mod d != 0] for [d]
            ranging in divisors of [n] *)
        let divisors = match n with
          | S.Int n -> S.Arith.Op.divisors n
          | _ -> assert false
        in
        if divisors <> []
          then
            let lits' = Util.array_except_idx c.C.hclits i in
            let t' = AT.mk_difference t (T.mk_const const) in
            let new_lits = List.map
              (fun d ->
                let new_lit = Lit.mk_neq ~ord
                  (AT.mk_remainder_e t' (Symbol.mk_bigint d))
                  (T.mk_const S.Arith.zero_i)
                in new_lit)
              divisors
            in
            let all_lits = new_lits @ lits' in
            let parents = [c] in
            let proof cc = Proof.mk_c_inference
              ~theories:["arith"; "equality"]
              ~rule:"remainder_divisor" cc [c.C.hcproof] in
            let new_c = C.create ~ctx ~parents all_lits proof in
            Util.debug 5 "remainder_of_divisors: from %a  deduce %a" C.pp c C.pp new_c;
            Util.incr_stat stat_infer_remainder_of_divisors;
            new_c :: acc
          else acc
      | _ -> acc)
    [] lits

(* when [a mod n] occurs in a clause, instantiate [Or_{i=0..n-1} a mod n = i] *)
let enum_remainder_cases c =
  let ctx = c.C.hcctx in
  let ord = Ctx.ord ctx in
  let lits = _view_lits_as_remainder c.C.hclits in
  Array.fold_left
    (fun acc lit -> match lit with
      | `EqMod (t, n, c, sign) ->
        (* TODO: if n divides t-c, do not enumerate but assert  t-c mod n = 0 *)
        begin try
          (* ignore constant *)
          let e = M.of_term t in
          let e' = M.remove_const e in
          let t' = M.to_term e' in
          (* range: 0 .. n-1 *)
          let range = match n with
            | S.Int n' -> _int_range ~strict_low:false ~strict_high:true n'
            | _ -> assert false
          in
          (* make clause *)
          let lits' = List.map
            (fun i ->
              Lit.mk_eq ~ord (AT.mk_remainder_e t' n) (T.mk_const (S.mk_bigint i)))
            range
          in
          let proof cc = Proof.mk_c_trivial ~theories:["arith"; "equality"] cc in
          let new_c = C.create ~ctx lits' proof in
          Util.debug 5 "instantiate enum of remainder for %a: clause %a" S.pp n C.pp new_c;
          Util.incr_stat stat_enum_remainder_cases;
          new_c :: acc
        with M.NotLinear -> acc
        end
      | _ -> acc)
    [] lits

(* when a lit of the form [n a = b] with a, b ground occurs, infer that
    [b mod n = 0] *)
let remainder_of_equality c =
  let ctx = c.C.hcctx in
  let ord = Ctx.ord ctx in
  Util.array_foldi
    (fun acc i lit -> match lit with
      | Lit.Equation (l, r, true, _)
        when T.is_ground l && T.is_ground r && Type.eq Type.int (T.ty l) ->
        begin match ArithLit.Canonical.extract_opt lit with
        | Some (ArithLit.Canonical.Compare (ArithLit.Eq, m1, m2)) ->
          (* m1 = m2, where m ground and integer *)
          let m = M.difference m1 m2 in
          assert (M.is_ground m);
          let terms = M.to_list m in
          List.fold_left
            (fun acc (coeff, t) ->
              if not (S.Arith.is_one coeff)
                then begin
                  (* m' + coeff*t = 0, so m' mod coeff = 0 *) 
                  let m' = M.remove m t in
                  let t' = M.to_term m' in
                  let lit = Lit.mk_eq ~ord
                    (AT.mk_remainder_e t' (S.Arith.Op.abs coeff))
                    (T.mk_const S.Arith.zero_i)
                  in
                  let lits' = lit :: Util.array_except_idx c.C.hclits i in
                  let proof cc = Proof.mk_c_inference
                    ~theories:["arith"; "equality"] ~rule:"remainder_of_eq"
                    cc [c.C.hcproof] in
                  let new_c = C.create ~ctx lits' proof in
                  Util.debug 5 "deduce remainders from %a: clause %a" C.pp c C.pp new_c;
                  Util.incr_stat stat_remainder_of_equality;
                  new_c :: acc
                end
              else acc)
            acc terms
        | _ -> acc
        end
      | _ -> acc)
    [] c.C.hclits

exception FoundSingleVar of [`Left|`Right] * Symbol.t * FOTerm.t

let _var_occurs_exactly_once_in_lits var lits =
  assert (T.is_var var);
  let seq = Sequence.of_array lits in
  let seq = Sequence.flatMap Literal.Seq.vars seq in
  let seq = Sequence.filter (fun v' -> T.eq v' var) seq in
  Sequence.length seq = 1

(* [t != n.X or C] becomes [t mod n != 0 or C] if [X] does not occur in [C]. *)
let remainder_of_var_ineq c =
  let ctx = c.C.hcctx in
  let ord = Ctx.ord ctx in
  let changed = ref false in
  let lits' = Array.mapi
    (fun i lit -> match lit with
      | Lit.Equation (l, r, false, _)
        when not (T.is_ground l && T.is_ground r) && Type.eq Type.int (T.ty l) ->
        begin match ArithLit.Canonical.extract_opt lit with
        | Some (ArithLit.Canonical.Compare (ArithLit.Neq, m1, m2)) ->
          (* m1 != m2, integer monomes that are not ground *)
          let terms =
            List.map (fun (c,t) -> `Left, c, t) (M.to_list m1) @
            List.map (fun (c,t) -> `Right, c, t) (M.to_list m2)
          in
          begin try
            List.iter
              (fun (side, n, t) ->
                if T.is_var t && _var_occurs_exactly_once_in_lits t c.C.hclits
                  then raise (FoundSingleVar(side, n, t)))
              terms;
            (* did not found a single var *)
            lit
          with FoundSingleVar(side, n, var) ->
            (* build [m] such that [lit === m != n*X]. *)
            let m = match side with
              | `Left -> M.difference m2 (M.remove m1 var)
              | `Right -> M.difference m1 (M.remove m2 var)
            in
            assert (not (M.var_occurs ~var m));
            let lit' = Literal.mk_neq ~ord
              (AT.mk_remainder_e (M.to_term m) n)
              (T.mk_const S.Arith.zero_i)
            in
            changed := true;
            lit'
          end
        | _ -> lit
        end
      | _ -> lit)
    c.C.hclits
  in
  if !changed
    then begin
      let proof cc = Proof.mk_c_simp
        ~theories:["arith"; "equality"] ~rule:"remainder_of_var_ineq"
        cc [c.C.hcproof] in
      let new_c = C.create_a ~ctx lits' proof in
      Util.debug 5 "simplified remainder from var ineq of %a: clause %a"
        C.pp c C.pp new_c;
      Util.incr_stat stat_remainder_of_var_ineq;
      new_c
    end else
      c

(** {2 Setup} *)

(* TODO: some simplification stuff? Or distributivity?
   TODO: axiomatize quotient_e using remainder_e? *)

let axioms =
  (* parse a pformula
  let pform ~name s =
    let f = Parse_tptp.parse_formula Lex_tptp.token (Lexing.from_string s) in
    let proof = Proof.mk_f_axiom f ~file:"/dev/arith" ~name in
    let pf = PF.create f proof in
    pf
  in
  *)
  []

let setup_penv ~penv =
  (* rule for formula simplification *)
  let simplify_rule set pf =
    let f' = AT.Form.simplify pf.PF.form in
    if F.eq pf.PF.form f'
      then []
      else
        let proof = Proof.mk_f_simp ~theories:["arith";"equality"]
          ~rule:"arith_simplify" f' [pf.PF.proof] in
        let pf' = PF.create f' proof in
        [PEnv.SimplifyInto pf']
  in
  (* signature of arith symbols *)
  let base = Signature.Arith.signature in
  PEnv.add_base_sig ~penv base;
  PEnv.add_operation ~penv ~prio:2 simplify_rule;
  PEnv.add_constr ~penv (Precedence.min_constraint (Signature.to_symbols base));
  ()

let setup_env ?(ac=false) ~env =
  (* basics *)
  Env.interpret_symbols ~env Evaluator.FO.arith;
  (* rules *)
  Env.add_lit_rule ~env "arith_rw" rewrite_lit;
  Env.add_unary_inf ~env "arith_factor" factor_arith;
  Env.add_unary_inf ~env "arith_purify" purify_arith;
  Env.add_unary_inf ~env "arith_elim" eliminate_arith;
  Env.add_unary_inf ~env "arith_inner_case_switch" inner_case_switch;
  Env.add_binary_inf ~env "arith_case_switch" case_switch;
  Env.add_simplify ~env factor_bounds;
  Env.add_is_trivial ~env bounds_are_tautology;
  (* modular arith *)
  Env.interpret_symbol ~env S.Arith.remainder_e simplify_remainder_term;
  Env.add_lit_rule ~env "arith_simplify_remainder" simplify_remainder_lit;
  Env.add_unary_inf ~env "arith_remainder_divisors" infer_remainder_of_divisors;
  Env.add_unary_inf ~env "arith_enum_remainder" enum_remainder_cases;
  Env.add_unary_inf ~env "arith_remainder_of_eq" remainder_of_equality;
  Env.add_simplify ~env remainder_of_var_ineq;
  (* declare some AC symbols *)
  if ac then begin
    AC.add_ac ~env S.Arith.sum;
    AC.add_ac ~env S.Arith.product;
    end;
  (* enable cancellative inferences *)
  CancellativeInference.setup_env ~env;
  (* we are (until proved otherwise) incomplete *)
  Ctx.lost_completeness ~ctx:(Env.ctx env);
  ()

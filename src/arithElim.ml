
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
module F = FOFormula
module C = Clause
module I = ProofState.TermIndex
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

(** {2 Inference Rules} *)

let rewrite_lit ~ctx lit =
  let ord = Ctx.ord ~ctx in
  let lit' = Arith.Lit.simplify ~ord lit in
  if Arith.Lit.is_trivial lit'
    then Lit.mk_tauto
  else if Arith.Lit.has_instances lit'
    then lit'
    else Lit.mk_absurd

let eliminate_arith c =
  let ctx = c.C.hcctx in
  let ord = Ctx.ord ctx in
  let eligible = C.Eligible.max c in
  (* find new arrays of literals, after substitution *)
  let lits'_list = Arith.Lits.eliminate ~ord ~eligible c.C.hclits in
  (* turn new arrays into clauses, if needed *)
  List.fold_left
    (fun acc lits' ->
      if Lits.eq_com c.C.hclits lits'
        then acc
        else begin
          (* build new clause *)
          let proof cc = Proof.mk_c_step ~theories:["arith";"equality"]
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
    let proof cc = Proof.mk_c_step ~theories:["arith";"equality"]
      ~rule:"factor" cc [c.C.hcproof] in
    let new_c = C.create_a ~parents:[c] ~ctx lits' proof in
    Util.debug 3 "factor %a (with %a) into %a" C.pp c Substs.FO.pp subst C.pp new_c;
    new_c
  in
  (* try to factor arith literals *)
  Lits.fold_lits ~eligible c.C.hclits []
    (fun acc lit i ->
      try
        let elit = Arith.Lit.Extracted.extract lit in
        let substs = Arith.Lit.Extracted.factor elit in
        List.fold_left
          (fun acc subst -> mk_instance subst :: acc)
          acc substs
      with Monome.NotLinear _ -> acc)

let pivot_arith c =
  let ctx = c.C.hcctx in
  let eligible = C.Eligible.always in
  let lits'_list = Arith.Lits.pivot ~ord:(Ctx.ord ctx) ~eligible c.C.hclits
  in
  Util.list_fmap
    (fun lits' ->
      if Lits.eq_com c.C.hclits lits'
        then None
        else begin
          (* build new clause *)
          let proof cc = Proof.mk_c_step ~theories:["equality";"arith"]
            ~rule:"arith_pivot" cc [c.C.hcproof] in
          let new_c = C.create_a ~parents:[c] ~ctx lits' proof in
          Util.debug 3 "arith_pivot of %a: %a" C.pp c C.pp new_c;
          Some new_c
        end)
    lits'_list

let purify_arith c =
  let ctx = c.C.hcctx in
  let eligible = C.Eligible.(max c) in
  let lits' = Arith.Lits.purify ~ord:(Ctx.ord ctx) ~eligible c.C.hclits
  in
  if Lits.eq_com c.C.hclits lits'
    then []
    else begin
      let proof cc = Proof.mk_c_step ~rule:"purify" cc [c.C.hcproof] in
      let new_c = C.create_a ~ctx ~parents:[c] lits' proof in
      [new_c]
    end

(* view array of literals as either lower bound for a term,
    or higher bound for a term, or something else *)
let _view_lits_as_bounds ~ctx lits =
  (* only for TPTP ordering *)
  let instance = TO.tstp_instance ~spec:(Ctx.total_order ctx) in
  Array.map
    (fun lit ->
      try
        let olit = Lit.ineq_lit_of ~instance lit in
        let l = olit.TO.left in
        let r = olit.TO.right in
        let strict = olit.TO.strict in
        match Arith.T.arith_kind l, Arith.T.arith_kind r with
        | (`Var | `Not), (`Const | `Expr) ->
          let m = Monome.of_term r in
          `HigherBound (strict, l, m)
        | (`Const | `Expr), (`Var | `Not) ->
          let m = Monome.of_term l in
          `LowerBound (strict, m, r)
        | _ -> `Ignore
      with Not_found | Monome.NotLinear _ -> `Ignore)
    lits

(* if a < t1, a < t2, .... a < tn occurs in clause, where t_i are all
   arithmetic constants, replace by  a < max_i(t_i) *)
let factor_bounds c =
  Util.enter_prof prof_factor_bounds;
  let ctx = c.C.hcctx in
  (* [bv] stores which literals should survive *)
  let bv = BV.create ~size:(Array.length c.C.hclits) true in
  (* literals seen as comparisons with monomes *)
  let lits = _view_lits_as_bounds ~ctx c.C.hclits in
  assert (Array.length lits = Array.length c.C.hclits);
  (* remove literals that are redundant *)
  Array.iteri
    (fun i lit -> match lit with
    | `Ignore -> ()
    | `LowerBound (_, m, r) ->
      if Util.array_exists
        (function
          | `LowerBound (_, m', r') ->
            (* if m < r and m' < r and m > m', then m' < r is enough *)
            T.eq r r' && Monome.comparison m m' = Comparison.Gt
          | _ -> false)
        lits
        then BV.reset bv i
    | `HigherBound (_, l, m) ->
      if Util.array_exists
        (function
          | `HigherBound (_, l', m') ->
            (* if l < m and l < m', then l < max(m,m') is enough *)
            T.eq l l' && Monome.comparison m m' = Comparison.Lt
          | _ -> false)
        lits
        then BV.reset bv i
    )
    lits;
  (* see whether some lits were removed *)
  if BV.cardinal bv < Array.length c.C.hclits then begin
    let lits' = BV.select bv c.C.hclits in
    let proof cc = Proof.mk_c_step ~theories:["equality";"arith"]
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
let case_switch active_set c =
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
        let monome = Monome.add_const lower (S.mk_bigint n) in
        let monome = Monome.apply_subst ~renaming subst monome s_lower in
        Lit.mk_eq ~ord t' (Monome.to_term monome))
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
  let lits = _view_lits_as_bounds ~ctx c.C.hclits in
  (* fold on literals *)
  let new_clauses = Util.array_foldi
    (fun acc i lit -> match lit with
      | `Ignore -> acc
      | `LowerBound (strict_low, low, t) when Type.eq Type.int (Monome.type_of low) ->
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
              let high = Monome.of_term olit'.TO.right in
              assert (Type.eq Type.int (Monome.type_of high));
              (* compute range= subst(high)-subst(low) *)
              let renaming = Ctx.renaming_clear ~ctx in
              let low' = Monome.apply_subst ~renaming subst low 0 in
              let high' = Monome.apply_subst ~renaming subst high 1 in
              let range = Monome.difference high' low' in
              (* see whether range is a constant *)
              if Monome.is_constant range
              then match range.Monome.constant with
                | S.Int range ->
                  (* ok, found a high bound, such that high-low is a constant,
                      so t ranges from low to high. Typing ensures that
                      high is also an integer monome. *)
                  _do_case_switch c 0 i c' 1 i' t 0 low 0 strict_low strict_high range subst acc
                | _ -> assert false
              else acc
            with Exit | Not_found | Monome.NotLinear _ ->
              acc)
      | `LowerBound _ ->
        acc  (* other lower bounds are ignored *)
      | `HigherBound (strict_high, t, high) when Type.eq Type.int (Monome.type_of high) ->
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
              let low = Monome.of_term olit'.TO.left in
              assert (Type.eq Type.int (Monome.type_of low));
              (* compute range= subst(high)-subst(low) *)
              let renaming = Ctx.renaming_clear ~ctx in
              let low' = Monome.apply_subst ~renaming subst low 1 in
              let high' = Monome.apply_subst ~renaming subst high 0 in
              let range = Monome.difference high' low' in
              (* see whether range is a constant *)
              if Monome.is_constant range
              then match range.Monome.constant with
                | S.Int range ->
                  (* ok, found a high bound, such that high-low is a constant,
                      so t ranges from low to high. Typing ensures that
                      high is also an integer monome. *)
                _do_case_switch c' 1 i' c 0 i t 0 low 1 strict_low strict_high range subst acc
                | _ -> assert false
              else acc
            with Exit | Not_found | Monome.NotLinear _ ->
              acc)
      | `HigherBound _ ->
        acc (* other higher bounds are ignored *)
      )
    [] lits
  in
  Util.exit_prof prof_case_switch;
  new_clauses

let inner_case_switch c =
  let ctx = c.C.hcctx in
  let ord = Ctx.ord ctx in
  let spec = Ctx.total_order ctx in
  let instance = TO.tstp_instance ~spec in  (* $less, $lesseq *)
  (* do the case switch, removing lits i and j, adding t=low+k
     where k in [0... range] *)
  let _do_case_switch i j t low range strict_low strict_high subst acc =
    Util.debug 5 "inner_case_switch in %a: %a in (%a,%a+%s) subst is %a"
      C.pp c T.pp t Monome.pp low Monome.pp low
      (Big_int.string_of_big_int range) Substs.FO.pp subst;
    (* remove lits i and j *)
    let lits = Util.array_foldi
      (fun acc i' lit -> if i' <> i && i' <> j then lit :: acc else acc)
      [] c.C.hclits
    in
    List.fold_left
      (fun acc k ->
        let renaming = Ctx.renaming_clear ~ctx in
        let low' = Monome.apply_subst ~renaming subst low 0 in
        let t' = Substs.FO.apply ~renaming subst t 0 in
        let lit =
          Literal.mk_neq ~ord t'
            (Arith.T.mk_sum (Monome.to_term low') (T.mk_const (S.mk_bigint k)))
        in
        let lits' = Literal.apply_subst_list ~ord ~renaming subst lits 0 in
        let new_lits = lit :: lits' in
        let proof cc = Proof.mk_c_step cc ~rule:"arith_inner_case_switch" [c.C.hcproof] in
        let parents = [c] in
        let new_clause = C.create ~parents ~ctx new_lits proof in
        Util.debug 5 "  --> inner case switch gives clause %a" C.pp new_clause;
        new_clause :: acc)
      acc (_int_range ~strict_low ~strict_high range)
  in
  (* view literals as arithmetic bounds when possible *)
  let lits = _view_lits_as_bounds ~ctx c.C.hclits in
  (* fold on literals *)
  let new_clauses = Util.array_foldi
    (fun acc i lit -> match lit with
      | `Ignore -> acc
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
                let range = Monome.difference high low in
                if Monome.is_constant range
                  then match range.Monome.constant with
                  | S.Int range ->
                    _do_case_switch i j t low range strict_low strict_high subst acc
                  | _ -> assert false
                  else acc
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
let bounds_are_tautology c =
  let ctx = c.C.hcctx in
  let lits = _view_lits_as_bounds ~ctx c.C.hclits in
  let res = Util.array_exists
    (function
      | `LowerBound (strict_low, l, t) ->
        Util.array_exists
          (function
            | `HigherBound (strict_high, t', r) when T.eq t t' ->
              let both_strict = strict_low && strict_high in
              begin match Monome.comparison l r with
              | Comparison.Incomparable -> false
              | Comparison.Eq ->
                not both_strict (* n < t | t <= n is redundant *)
              | Comparison.Lt -> true
              | Comparison.Gt ->
                (not both_strict) && Type.eq (T.ty t) Type.int && Monome.(eq (succ l) r)
                  (* l <= t | t <= l+1 is redundant in integers *)
              end
            | _ -> false)
          lits
      | _ -> false)
    lits
  in
  if res then Util.debug 2 "redundant clause %a: bounds are tautology" C.pp c;
  res

let axioms =
  (* parse a pformula
  let pform ~name s =
    let f = Parse_tptp.parse_formula Lex_tptp.token (Lexing.from_string s) in
    let proof = Proof.mk_f_axiom f ~file:"/dev/arith" ~name in
    let pf = PF.create f proof in
    pf
  in
  *)
  []  (* TODO: some simplification stuff? Or distributivity? *)

(** {2 Setup} *)

let setup_penv ~penv =
  (* rule for formula simplification *)
  let simplify_rule set pf =
    let f' = Arith.F.simplify pf.PF.form in
    if F.eq pf.PF.form f'
      then []
      else
        let proof = Proof.mk_f_step f' ~rule:"arith_simplify" [pf.PF.proof] in
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
  Env.add_lit_rule ~env "arith_rw" rewrite_lit;
  Env.add_unary_inf ~env "arith_factor" factor_arith;
  Env.add_unary_inf ~env "arith_pivot" pivot_arith;
  Env.add_unary_inf ~env "arith_purify" purify_arith;
  Env.add_unary_inf ~env "arith_elim" eliminate_arith;
  Env.add_unary_inf ~env "arith_inner_case_switch" inner_case_switch;
  Env.add_binary_inf ~env "arith_case_switch" case_switch;
  Env.add_simplify ~env factor_bounds;
  Env.add_is_trivial ~env bounds_are_tautology;
  (* declare some AC symbols *)
  if ac then begin
    AC.add_ac ~env S.Arith.sum;
    AC.add_ac ~env S.Arith.product;
    end;
  (* be sure that the ordering is present in the context *)
  Chaining.add_tstp_order ~env;
  (* we are (until proved otherwise) incomplete *)
  Ctx.lost_completeness ~ctx:(Env.ctx env);
  ()

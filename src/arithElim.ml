
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
let prof_factor_bounds = Util.mk_profiler "arith.factor_bounds"

let stat_case_switch = Util.mk_stat "arith.case_switch"
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

(* if a < t1, a < t2, .... a < tn occurs in clause, where t_i are all
   arithmetic constants, replace by  a < max_i(t_i) *)
let factor_bounds c = c (* TODO *)
(*
  Util.enter_prof prof_factor_bounds;
  let ctx = c.C.hcctx in
  let instance = TO.tstp_instance ~spec:(Ctx.total_order ctx) in
  (* [bv] stores which literals should survive *)
  let bv = BV.create ~size:(Array.length c.C.hclits) true in
  let left = T.Tbl.create 5 in   (* terms t such that  t < constant *)
  let right = T.Tbl.create 5 in  (* terms t such that constant < t *)
  Lits.fold_lits ~eligible:C.Eligible.pos c.C.hclits ()
    (fun () lit i ->
      try
        let olit = Lit.ineq_lit_of ~instance lit in
        let l = olit.TO.left in
        let r = olit.TO.right in
        let strict = olit.TO.strict in
        match l.T.term, r.T.term with
        | T.Node ((S.Int _ | S.Rat _ | S.Real _) as s, []), _
          when not (Arith.T.is_arith_const r) ->
          begin try
            (* find whether [r] has another bound *)
            let (i', s', strict') = T.Tbl.find right r in
            if S.eq s s' && (strict <> strict')
              then begin
                (* if s=s' then  s' < r  or s <= r ----> s <= r *)
                T.Tbl.replace right r (i, s, false);
                BV.reset i;
                BV.reset i';
              end
            else if S.Arith.Op.less s s'
              (* if s < s' then  s' <| r  or  s <| r -----> s <| r *)
              then begin
                T.Tbl.replace right r (i, s, strict);
                BV.reset bv i;
                BV.reset bv i;
              end
          with Not_found ->
            T.Tbl.add right r (i, s, strict) (* remember bound for r *)
          end
        | _, T.Node ((S.Int _ | S.Rat _ | S.Real _) as s, []
          when not (Arith.T.is_arith_const l) ->
      with Not_found -> ());
  if BV.length bv < Array.length c.C.hclits then begin
    assert false (* TODO *)


    end
  else c  (* no change *)
    *)

(* enumerate integers from lower to higher, included. Returns an empty list
    if lower > higher *)
let _int_range lower higher =
  let rec enum acc l r = match Big_int.compare_big_int l r with
    | 0 -> l :: acc (* done *)
    | n when n > 0 -> [] (* l must be smaller *)
    | _ -> enum (l :: acc) (Big_int.succ_big_int l) r
  in
  enum [] lower higher

(* new inference, kind of the dual of inequality chaining for integer
   inequalities. See the .mli file for more explanations. *)
let case_switch active_set c =
  Util.enter_prof prof_case_switch;
  let ctx = active_set#ctx in
  let ord = Ctx.ord ctx in
  let spec = Ctx.total_order ctx in
  let instance = TO.tstp_instance ~spec in  (* $less, $lesseq *)
  let eligible = C.Eligible.chaining c in
  (* do the case switch inference. c contains lower <= t,
      c' contains t <= higher. Enumerates t = i for i in lower .... higher *)
  let _do_case_switch c s_c i c' s_c' i' t lower higher subst acc =
    Util.debug 5 "case_switch between %a at %d and %a at %d" C.pp c i C.pp c' i';
    let renaming = Ctx.renaming_clear ~ctx in
    let lits_left = Util.array_except_idx c.C.hclits i in
    let lits_left = Lit.apply_subst_list ~renaming ~ord subst lits_left s_c in
    let lits_right = Util.array_except_idx c'.C.hclits i' in
    let lits_right = Lit.apply_subst_list ~renaming ~ord subst lits_right s_c' in
    let t' = Substs.FO.apply ~renaming subst t s_c in
    (* the case switch on t *)
    let lits_case = List.map
      (fun n -> Lit.mk_eq ~ord t' (T.mk_const (S.mk_bigint n)))
      (_int_range lower higher)
    in
    let new_lits = lits_left @ lits_right @ lits_case in
    let proof cc = Proof.mk_c_step cc ~rule:"arith_case_switch" [c.C.hcproof; c'.C.hcproof] in
    let parents = [c; c'] in
    let new_c = C.create ~parents ~ctx new_lits proof in
    Util.debug 5 "  --> case switch gives clause %a" C.pp new_c;
    Util.incr_stat stat_case_switch;
    new_c :: acc
  in
  (* fold on literals *)
  let new_clauses = Lits.fold_lits ~eligible c.C.hclits []
    (fun acc lit i ->
      try
        let olit = Lit.ineq_lit_of ~instance lit in
        let l = olit.TO.left in
        let r = olit.TO.right in
        if olit.TO.strict
          then acc  (* on integers, we should only have <=, not < *)
        else begin match l.T.term, r.T.term with
          | T.Node (S.Int lower, _, []), _ when not (Arith.T.is_arith_const r) ->
            (* const <= r, look for r <= const' in index *)
            I.retrieve_unifiables active_set#idx_ord_left 1 r 0 acc
              (fun acc _ with_pos subst ->
                try
                  let c' = with_pos.C.WithPos.clause in
                  let pos' = with_pos.C.WithPos.pos in
                  let i' = List.hd pos' in
                  let olit' = Lit.ineq_lit_of ~instance c'.C.hclits.(i') in
                  begin match olit'.TO.right.T.term with
                  | T.Node (S.Int higher, _, []) ->
                    (* found higher bound, now we can do the case switch *)
                    _do_case_switch c 0 i c' 1 i' r lower higher subst acc
                  | _ -> acc
                  end
                with Not_found ->
                  acc)
          | _, T.Node (S.Int higher, _, []) when not (Arith.T.is_arith_const l) ->
            (* l <= const, look for const' <= l in index *)
            I.retrieve_unifiables active_set#idx_ord_right 1 l 0 acc
              (fun acc _ with_pos subst ->
                try
                  let c' = with_pos.C.WithPos.clause in
                  let pos' = with_pos.C.WithPos.pos in
                  let i' = List.hd pos' in
                  let olit' = Lit.ineq_lit_of ~instance c'.C.hclits.(i') in
                  begin match olit'.TO.left.T.term with
                  | T.Node (S.Int lower, _, []) ->
                    (* found lower bound *)
                    _do_case_switch c' 1 i' c 0 i l lower higher subst acc
                  | _ -> acc
                  end
                with Not_found ->
                  acc)
          | _ -> acc
        end
      with Not_found ->
        acc)
  in
  Util.exit_prof prof_case_switch;
  new_clauses

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
  Env.add_binary_inf ~env "arith_case_switch" case_switch;
  Env.add_simplify ~env factor_bounds;
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

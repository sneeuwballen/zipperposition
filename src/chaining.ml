
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

(** {6 Chaining Inferences} *)

open Logtk

module T = Term
module C = Clause
module I = ProofState.TermIndex
module Lit = Literal
module Lits = Literal.Arr
module TO = Theories.TotalOrder
module Ord = Ordering

let stat_eq_chaining = Util.mk_stat "eq_chaining"
let stat_ineq_chaining = Util.mk_stat "ineq_chaining"

let prof_eq_chaining_active = Util.mk_profiler "chaining.eq_active"
let prof_eq_chaining_passive = Util.mk_profiler "chaining.eq_passive"
let prof_ineq_chaining_active = Util.mk_profiler "chaining.ineq_active"
let prof_ineq_chaining_passive = Util.mk_profiler "chaining.ineq_passive"

(* finds all positions, in lits that are [eligible], that unify with the
  subterm at the given [pos], and return the list of such positions
  plus the new substitution *)
let _gather_positions ~eligible lits scope pos subst =
  let t = Lits.at_pos lits pos in
  let pos' = List.tl pos in  (* position within the literal *)
  Lits.fold_lits ~eligible lits ([], subst)
    (fun (pos_list, subst) lit i ->
      try
        let t' = Lit.at_pos lit pos' in
        let subst = Unif.unification ~subst t scope t' scope in
        let pos_list = (i::pos') :: pos_list in
        pos_list, subst
      with Not_found | Unif.Fail ->
        (pos_list, subst))

(* check ordering conditions for the active clause in equality chaining *)
let _check_eq_chaining_active ~ctx active s_a active_pos subst =
  let s, t, sign = Lits.get_eqn active.C.hclits active_pos in
  sign &&
  T.db_closed t &&
  begin (* s' not < t' *)
    let ord = Ctx.ord ~ctx in
    let renaming = Ctx.renaming_clear ~ctx in
    let s' = Substs.apply ~renaming subst s s_a in
    let t' = Substs.apply ~renaming subst t s_a in
    match Ord.compare ord s' t' with
    | Comparison.Lt -> false
    | _ -> true
  end &&
  begin (* eligible for paramodulation *)
    let i = List.hd active_pos in
    let bv = C.eligible_param active i subst in
    BV.get bv i
  end

(* check ordering conditions for passive clause in equality chaining left *)
let _check_eq_chaining_left_passive ~ctx passive s_p positions subst =
  assert (positions <> []);
  let ord = Ctx.ord ~ctx in
  let spec = Ctx.total_order ~ctx in
  let ord_lit = Lits.get_ineq ~spec passive.C.hclits (List.hd positions) in
  let instance = ord_lit.TO.instance in
  let renaming = Ctx.renaming_clear ~ctx in
  let t1' = Substs.apply ~renaming subst ord_lit.TO.left s_p in
  (* subst(t1) must be a maximal term *)
  Sequence.for_all
    (fun v ->
      let v' = Substs.apply ~renaming subst v s_p in
      (* all other terms [v'] must not be bigger than [t1'] *)
      match Ord.compare ord t1' v' with
      | Comparison.Lt -> false
      | _ -> true)
    (Lits.terms_under_ineq ~instance passive.C.hclits)
  &&
  (* each ti <| vi must verify that subst(ti) not < subst(vi) in
    the term ordering *)
  List.for_all
    begin fun pos ->
      let ord_lit = Lits.get_ineq ~spec passive.C.hclits pos in
      let renaming = Ctx.renaming_clear ~ctx in
      let ti = Substs.apply ~renaming subst ord_lit.TO.left s_p in
      let vi = Substs.apply ~renaming subst ord_lit.TO.right s_p in
      match Ord.compare ord ti vi with
      | Comparison.Lt -> false
      | _ -> true
    end
    positions
  
(* equality chaining left *)
let do_eq_chaining_left ~ctx active s_a active_pos passive s_p passive_pos subst acc =
  let ord = Ctx.ord ~ctx in
  let spec = Ctx.total_order ~ctx in
  (* rewrite s into t *)
  let s, t, _ = Lits.get_eqn active.C.hclits active_pos in
  assert (not (T.is_var s));
  (* get all inequalities that can be factored with passive_pos: if 
      the passive lit is t1[s1]_p <| v1, we want all the ti[si]_p < vi
      such that si is unifiable with s1 (and thus with [s]).
      We only consider literals that are inequations of the same instance
      as [t1 <| v1]. *)
  let ord_lit = Lits.get_ineq ~spec passive.C.hclits passive_pos in
  let eligible = C.Eligible.ineq_of passive ord_lit.TO.instance in
  let positions, subst =
    _gather_positions ~eligible passive.C.hclits s_p passive_pos subst in
  (* check ordering conditions *)
  if _check_eq_chaining_active ~ctx active s_a active_pos subst
  && _check_eq_chaining_left_passive ~ctx passive s_p positions subst
    then begin
      (* now we can combine the two clauses *)
      let renaming = Ctx.renaming_clear ~ctx in
      (* literals of new clause *)
      let lits_a = Util.array_except_idx active.C.hclits (List.hd active_pos) in
      let lits_a = Lit.apply_subst_list ~ord ~renaming subst lits_a s_a in
      let lits_p = Array.copy passive.C.hclits in
      List.iter
        (fun pos -> Lits.replace_pos ~ord lits_p ~at:pos ~by:t)
        positions;
      let lits_p = Array.to_list (Lits.apply_subst ~renaming ~ord subst lits_p s_p) in
      let new_lits = lits_a @ lits_p in
      (* proof *)
      let proof cc = Proof.mk_c_step cc "eq_chain_left"
        [active.C.hcproof; passive.C.hcproof] in
      let parents = [active; passive] in
      let new_clause = C.create ~parents ~ctx new_lits proof in
      Util.debug 3 "eq chaining left --> %a" C.pp new_clause;
      new_clause :: acc
    end else begin
      Util.debug 3 "eq chaining between %a at %a, and %a at %a redundant"
        C.pp active Position.pp active_pos C.pp passive Position.pp passive_pos;
      acc
    end

let do_eq_chaining_right ~ctx active s_a active_pos passive s_p passive_pos subst acc =
  acc (* TODO *)

(* perform equality chaining *)
let do_eq_chaining ~ctx active s_a active_pos passive s_p passive_pos subst acc =
  match passive_pos with
  | _::_::pos::_ when pos = Position.left_pos ->
    do_eq_chaining_left ~ctx active s_a active_pos passive s_p passive_pos subst acc
  | _::_::pos::_ when pos = Position.right_pos ->
    do_eq_chaining_right ~ctx active s_a active_pos passive s_p passive_pos subst acc
  | _ ->
    invalid_arg ("equality chaining: wrong pos " ^ (Position.to_string passive_pos))

(* use [s = t] to rewrite subterms of active clauses that unify with [s]
  into [t] *)
let eq_chaining_active active_set c =
  Util.enter_prof prof_eq_chaining_active;
  let ctx = active_set#ctx in
  let scope = T.max_var c.C.hcvars + 1 in
  let eligible = C.Eligible.param c in
  (* fold on eq lits *)
  let new_clauses = Lits.fold_eqn ~both:true ~eligible c.C.hclits []
    (fun acc s t _ s_pos ->
      I.retrieve_unifiables active_set#idx_ord_subterm scope s 0 acc
        (fun acc u_p with_pos subst ->
          let passive = with_pos.C.WithPos.clause in
          let u_pos = with_pos.C.WithPos.pos in
          do_eq_chaining ~ctx c 0 s_pos passive scope u_pos subst acc))
  in
  Util.exit_prof prof_eq_chaining_active;
  new_clauses

let eq_chaining_passive active_set c =

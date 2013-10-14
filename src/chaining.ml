
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
open Logtk_meta

module T = FOTerm
module HOT = HOTerm
module C = Clause
module I = ProofState.TermIndex
module Lit = Literal
module Lits = Literal.Arr
module TO = Theories.TotalOrder
module Ord = Ordering
module S = Substs.FO

let stat_eq_chaining = Util.mk_stat "eq_chaining"
let stat_ineq_chaining = Util.mk_stat "ineq_chaining"

let prof_eq_chaining_active = Util.mk_profiler "chaining.eq_active"
let prof_eq_chaining_passive = Util.mk_profiler "chaining.eq_passive"
let prof_ineq_chaining_left = Util.mk_profiler "chaining.ineq_left"
let prof_ineq_chaining_right = Util.mk_profiler "chaining.ineq_right"

(* finds all positions, in lits that are [eligible], that unify with the
  subterm at the given [pos], and return the list of such positions
  plus the new substitution *)
let _gather_positions ~eligible ~signature lits scope pos subst =
  let t = Lits.at_pos lits pos in
  let pos' = List.tl pos in  (* position within the literal *)
  Lits.fold_lits ~eligible lits ([pos], subst)
    (fun (pos_list, subst) lit i ->
      try
        let t' = Lit.at_pos lit pos' in
        if T.is_var t' || not (TypeInference.FO.check_term_term_sig signature t t')
          then raise Exit  (* variables are not eligible *)
          else
            let subst = FOUnif.unification ~subst t scope t' scope in
            let pos_list = (i::pos') :: pos_list in
            pos_list, subst
      with Not_found | FOUnif.Fail | Exit ->
        pos_list, subst)

(* check ordering conditions for the active clause in equality chaining *)
let _check_eq_chaining_active ~ctx active s_a active_pos subst =
  let s, t, sign = Lits.get_eqn active.C.hclits active_pos in
  sign &&
  T.db_closed t &&
  begin (* s' not < t' *)
    let ord = Ctx.ord ~ctx in
    let renaming = Ctx.renaming_clear ~ctx in
    let s' = S.apply ~renaming subst s s_a in
    let t' = S.apply ~renaming subst t s_a in
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
  let renaming = S.Renaming.create 5 in
  let t1' = S.apply ~renaming subst ord_lit.TO.left s_p in
  (* subst(t1) must be a maximal term *)
  Sequence.for_all
    (fun v ->
      let v' = S.apply ~renaming subst v s_p in
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
      let ti = S.apply ~renaming subst ord_lit.TO.left s_p in
      let vi = S.apply ~renaming subst ord_lit.TO.right s_p in
      match Ord.compare ord ti vi with
      | Comparison.Lt -> false
      | _ -> true
    end
    positions

(* check ordering conditions for passive clause in equality chaining right *)
let _check_eq_chaining_right_passive ~ctx passive s_p positions subst =
  assert (positions <> []);
  let ord = Ctx.ord ~ctx in
  let spec = Ctx.total_order ~ctx in
  let ord_lit = Lits.get_ineq ~spec passive.C.hclits (List.hd positions) in
  let instance = ord_lit.TO.instance in
  let renaming = Ctx.renaming_clear ~ctx in
  let t1' = S.apply ~renaming subst ord_lit.TO.right s_p in
  (* subst(t1) must be a maximal term *)
  Sequence.for_all
    (fun v ->
      let v' = S.apply ~renaming subst v s_p in
      (* all other terms [v'] must not be bigger than [t1'] *)
      match Ord.compare ord t1' v' with
      | Comparison.Lt -> false
      | _ -> true)
    (Lits.terms_under_ineq ~instance passive.C.hclits)
  &&
  (* each vi <| ti must verify that subst(ti) not < subst(vi) in
    the term ordering *)
  List.for_all
    begin fun pos ->
      let ord_lit = Lits.get_ineq ~spec passive.C.hclits pos in
      let renaming = Ctx.renaming_clear ~ctx in
      let ti = S.apply ~renaming subst ord_lit.TO.right s_p in
      let vi = S.apply ~renaming subst ord_lit.TO.left s_p in
      match Ord.compare ord ti vi with
      | Comparison.Lt -> false
      | _ -> true
    end
    positions
  
(* equality chaining left *)
let do_eq_chaining_left ~ctx active s_a active_pos passive s_p passive_pos subst acc =
  let ord = Ctx.ord ~ctx in
  let signature = Ctx.signature ctx in
  let spec = Ctx.total_order ~ctx in
  let instance = (Lits.get_ineq ~spec passive.C.hclits passive_pos).TO.instance in
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
    _gather_positions ~eligible ~signature passive.C.hclits s_p passive_pos subst in
  (* check ordering conditions, and well-typedness *)
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
      let premises = active.C.hcproof :: passive.C.hcproof :: instance.TO.proof in
      let theories = ["total_order"] in
      let proof cc = Proof.mk_c_step ~theories ~rule:"eq_chain_left" cc premises in
      let parents = [active; passive] in
      let new_clause = C.create ~parents ~ctx new_lits proof in
      Util.debug 3 "eq chaining left --> %a" C.pp new_clause;
      new_clause :: acc
    end else begin
      Util.debug 3 "eq chaining left between %a at %a, and %a at %a redundant"
        C.pp active Position.pp active_pos C.pp passive Position.pp passive_pos;
      acc
    end

let do_eq_chaining_right ~ctx active s_a active_pos passive s_p passive_pos subst acc =
  let ord = Ctx.ord ~ctx in
  let signature = Ctx.signature ctx in
  let spec = Ctx.total_order ~ctx in
  let instance = (Lits.get_ineq ~spec passive.C.hclits passive_pos).TO.instance in
  (* rewrite s into t *)
  let s, t, _ = Lits.get_eqn active.C.hclits active_pos in
  assert (not (T.is_var s));
  (* get all inequalities that can be factored with passive_pos: if 
      the passive lit is v1 <| t1[s1]_p, we want all the vi <| ti[si]_p
      such that si is unifiable with s1 (and thus with [s]).
      We only consider literals that are inequations of the same instance
      as [v1 <| t1]. *)
  let ord_lit = Lits.get_ineq ~spec passive.C.hclits passive_pos in
  let eligible = C.Eligible.ineq_of passive ord_lit.TO.instance in
  let positions, subst =
    _gather_positions ~eligible ~signature passive.C.hclits s_p passive_pos subst in
  (* check ordering conditions, and well-typedness *)
  if _check_eq_chaining_active ~ctx active s_a active_pos subst
  && _check_eq_chaining_right_passive ~ctx passive s_p positions subst
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
      let premises = active.C.hcproof :: passive.C.hcproof :: instance.TO.proof in
      let theories = ["total_order"] in
      let proof cc = Proof.mk_c_step ~theories ~rule:"eq_chain_right" cc premises in
      let parents = [active; passive] in
      let new_clause = C.create ~parents ~ctx new_lits proof in
      Util.debug 3 "eq chaining right --> %a" C.pp new_clause;
      new_clause :: acc
    end else begin
      Util.debug 3 "eq chaining right between %a at %a, and %a at %a redundant"
        C.pp active Position.pp active_pos C.pp passive Position.pp passive_pos;
      acc
    end

(* perform equality chaining *)
let do_eq_chaining ~ctx active s_a active_pos passive s_p passive_pos subst acc =
  Util.debug 5 "try eq chaining with %a at %a and %a at %a" C.pp active
    Position.pp active_pos C.pp passive Position.pp passive_pos;
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

(* rewrite subterms in inequalities of the given clause [c] using equations
    of active clauses *)
let eq_chaining_passive active_set c =
  Util.enter_prof prof_eq_chaining_passive;
  let ctx = active_set#ctx in
  let passive = c in
  let scope = 1 in
  let spec = Ctx.total_order ~ctx in
  let eligible = C.Eligible.chaining c in
  (* fold on ineq lits *)
  let new_clauses = Lits.fold_ineq ~spec ~eligible c.C.hclits []
    (fun acc ord_lit lit_pos ->
      (* factorize code for left and right terms *)
      let explore acc t pos =
        T.all_positions ~pos t acc
        (fun acc t_p passive_pos ->
          (* at this point, [t_p] is a subterm of a side of the inequation *)
          I.retrieve_unifiables active_set#idx_sup_from scope t_p 0 acc
            (fun acc s with_pos subst ->
              (* [s] is the lhs of an equation in some clause, that can
                  potentially rewrite [t_p] into some smaller term *)
              let active = with_pos.C.WithPos.clause in
              let active_pos = with_pos.C.WithPos.pos in
              do_eq_chaining ~ctx active scope active_pos passive 0 passive_pos subst acc))
      in
      let l = ord_lit.TO.left in
      let r = ord_lit.TO.right in
      let acc = explore acc l (lit_pos @ [Position.left_pos]) in
      let acc = explore acc r (lit_pos @ [Position.right_pos]) in
      acc)
  in
  Util.exit_prof prof_eq_chaining_passive;
  new_clauses

(* literals whose position does not occur in [positions] *)
let _all_lits_but_positions lits positions =
  let bv = BV.create ~size:(Array.length lits) true in
  List.iter
    (fun pos -> match pos with
      | i::_ -> BV.reset bv i
      | [] -> assert false)
    positions;
  List.map fst (BV.select bv lits)

(* inequality chaining between two clauses *)
let do_ineq_chaining ~ctx left s_left left_pos right s_right right_pos subst acc =
  let ord = Ctx.ord ctx in
  let spec = Ctx.total_order ctx in
  Util.debug 5 "ineq_chaining between %a at %a and %a at %a" C.pp left
    Position.pp left_pos C.pp right Position.pp right_pos;
  let signature = Ctx.signature ctx in
  let instance = (Lits.get_ineq ~spec left.C.hclits left_pos).TO.instance in
  let mk_less t1 t2 = Lit.mk_true (T.mk_node instance.TO.less [t1; t2]) in
  let t1 = (Lits.get_ineq ~spec right.C.hclits right_pos).TO.left in
  (* find other literals that can be chained on *)
  let eligible = C.Eligible.pos in
  let left_pos_list, subst =
    _gather_positions ~eligible ~signature left.C.hclits s_left left_pos subst in
  let right_pos_list, subst =
    _gather_positions ~eligible ~signature right.C.hclits s_right right_pos subst in
  Util.debug 5 "positions for left: %a" (Util.pp_list Position.pp) left_pos_list;
  Util.debug 5 "positions for right: %a" (Util.pp_list Position.pp) right_pos_list;
  (* check ordering conditions. Note that the conditions are inversed w.r.t
    equality chaining (ie, in left clause, right hand side terms must
    be maximal, and conversely *)
  if _check_eq_chaining_right_passive ~ctx left s_left left_pos_list subst
  && _check_eq_chaining_left_passive ~ctx right s_right right_pos_list subst
    then begin
      let renaming = Ctx.renaming_clear ~ctx in
      (* build literal list *)
      let lits_left = _all_lits_but_positions left.C.hclits left_pos_list in
      let lits_left = Lit.apply_subst_list ~ord ~renaming subst lits_left s_left in
      let lits_right = _all_lits_but_positions right.C.hclits right_pos_list in
      let lits_right = Lit.apply_subst_list ~ord ~renaming subst lits_right s_right in
      let product = List.fold_left
        (fun acc left_pos ->
          let lit_left = Lits.get_ineq ~spec left.C.hclits left_pos in
          let ui = lit_left.TO.left in
          List.fold_left
            (fun acc right_pos ->
              let lit_right = Lits.get_ineq ~spec right.C.hclits right_pos in
              let vj = lit_right.TO.right in
              (* now to compute INEQ(ui, vj) *)
              let ui' = S.apply ~renaming subst ui s_left in
              let vj' = S.apply ~renaming subst vj s_right in
              let t1' = S.apply ~renaming subst t1 s_right in
              if lit_left.TO.strict || lit_right.TO.strict
                then (* ui < vj *)
                  mk_less ui' vj' :: acc
                else (* ui < vj OR ui = t1 *)
                  mk_less ui' vj' :: Lit.mk_eq ~ord ui' t1' :: acc        
              )
            acc right_pos_list)
        [] left_pos_list
      in
      let lits = lits_left @ lits_right @ product in
      (* build clause *)
      let theories = ["total_order"] in
      let rule = "inequality_chaining" in
      let premises = left.C.hcproof :: right.C.hcproof :: instance.TO.proof in
      let proof cc = Proof.mk_c_step ~theories ~rule cc premises in
      let new_clause = C.create ~parents:[left;right] ~ctx lits proof in
      Util.debug 3 "ineq_chaining of %a and %a gives %a"
        C.pp left C.pp right C.pp new_clause;
      new_clause :: acc
    end else
      let () = Util.debug 5 "ordering conditions won't do." in
      acc

(* inequality chaining, where [c] is on the left (which means terms on
    the RHS of inequalities will be chained) *)
let ineq_chaining_left active_set c =
  Util.enter_prof prof_ineq_chaining_left;
  let ctx = active_set#ctx in
  let spec = Ctx.total_order ~ctx in
  let eligible = C.Eligible.chaining c in
  (* fold on ineq lits *)
  let new_clauses = Lits.fold_ineq ~spec ~eligible c.C.hclits []
    (fun acc ord_lit left_pos ->
      let s1 = ord_lit.TO.right in
      let left_pos = left_pos @ [Position.right_pos] in
      Util.debug 5 "try left ineq chaining with %a at %a" C.pp c Position.pp left_pos;
      I.retrieve_unifiables active_set#idx_ord_side 1 s1 0 acc
        (fun acc t1 with_pos subst ->
          let right = with_pos.C.WithPos.clause in
          let right_pos = with_pos.C.WithPos.pos in
          do_ineq_chaining ~ctx c 0 left_pos right 1 right_pos subst acc))
  in
  Util.exit_prof prof_ineq_chaining_left;
  new_clauses

(* inequality chaining where [c] is on the right *)
let ineq_chaining_right active_set c =
  Util.enter_prof prof_ineq_chaining_right;
  let ctx = active_set#ctx in
  let spec = Ctx.total_order ~ctx in
  let eligible = C.Eligible.chaining c in
  (* fold on ineq lits *)
  let new_clauses = Lits.fold_ineq ~spec ~eligible c.C.hclits []
    (fun acc ord_lit right_pos ->
      let t1 = ord_lit.TO.left in
      let right_pos = right_pos @ [Position.left_pos] in
      Util.debug 5 "try right ineq chaining with %a at %a" C.pp c Position.pp right_pos;
      I.retrieve_unifiables active_set#idx_ord_side 1 t1 0 acc
        (fun acc s1 with_pos subst ->
          let left = with_pos.C.WithPos.clause in
          let left_pos = with_pos.C.WithPos.pos in
          do_ineq_chaining ~ctx left 1 left_pos c 0 right_pos subst acc))
  in
  Util.exit_prof prof_ineq_chaining_right;
  new_clauses

let reflexivity_res c =
  let ctx = c.C.hcctx in
  let spec = Ctx.total_order ctx in
  let ord = Ctx.ord ctx in
  let eligible = C.Eligible.max c in
  Lits.fold_ineq ~spec ~eligible c.C.hclits []
    (fun acc lit pos ->
      try
        let subst = FOUnif.unification lit.TO.left 0 lit.TO.right 0 in
        if lit.TO.strict
          then begin
            (* remove lit and make a new clause after substitution *)
            let i = List.hd pos in
            let lits = Util.array_except_idx c.C.hclits i in
            let renaming = Ctx.renaming_clear ~ctx in
            let lits = Lit.apply_subst_list ~ord ~renaming subst lits 0 in
            let premises = c.C.hcproof :: lit.TO.instance.TO.proof in
            let theories = ["total_order"] in
            let rule = "reflexivity_res" in
            let proof cc = Proof.mk_c_step ~theories ~rule cc premises in
            let new_c = C.create ~parents:[c] ~ctx lits proof in
            Util.debug 3 "reflexivity res of %a gives %a" C.pp c C.pp new_c;
            new_c :: acc
          end
        else acc
      with FOUnif.Fail -> acc)

let is_tautology c =
  let ctx = c.C.hcctx in
  let spec = Ctx.total_order ctx in
  let eligible = C.Eligible.always in
  try
    Lits.fold_ineq ~spec ~eligible c.C.hclits ()
      (fun () lit pos ->
        if not lit.TO.strict && T.eq lit.TO.left lit.TO.right
          (* a <= a is definitely a tautology *)
          then raise Exit);
    false
  with Exit ->
    true

let simplify c =
  let ctx = c.C.hcctx in
  let spec = Ctx.total_order ctx in
  let eligible = C.Eligible.always in
  let size = Array.length c.C.hclits in
  let bv = BV.create ~size true in
  let instances = ref [] in
  (* remove absurd literals from [bv] *)
  Lits.fold_ineq ~spec ~eligible c.C.hclits ()
    (fun () lit pos ->
      if lit.TO.strict && T.eq lit.TO.left lit.TO.right
        then begin
          (* eliminate literal a <= a *)
          let i = List.hd pos in
          BV.reset bv i;
          instances := lit.TO.instance :: !instances
        end);
  if BV.cardinal bv < size
    then begin
      let lits = List.map fst (BV.select bv c.C.hclits) in
      let theories = ["total_order"] in
      instances := Util.list_uniq TO.eq !instances;
      let proofs = Util.list_flatmap (fun instance -> instance.TO.proof) !instances in
      let rule = "total_order_simplify" in
      let proof cc = Proof.mk_c_step ~theories ~rule cc (c.C.hcproof :: proofs) in
      let new_c = C.create ~ctx ~parents:[c] lits proof in
      Util.debug 3 "total_order_simplify %a into %a" C.pp c C.pp new_c;
      new_c
    end else
      c

let axioms ~ctx ~instance =
  []

(** {2 Env} *)

let add_order ~env ?proof ~less ~lesseq =
  let ctx = Env.ctx env in
  let spec = Ctx.total_order ctx in
  let exists_some = Theories.TotalOrder.exists_order ~spec in
  Util.debug 1 "enable chaining for order %a, %a" Symbol.pp less Symbol.pp lesseq;
  (* first ordering, we have to enable the chaining inferences *)
  if not exists_some then begin
    Util.debug 3 "setup chaining inferences";
    Env.add_is_trivial ~env is_tautology;
    Env.add_simplify ~env simplify;
    Env.add_unary_inf ~env "reflexivity_res" reflexivity_res;
    Env.add_binary_inf ~env "ineq_chaining_left" ineq_chaining_left;
    Env.add_binary_inf ~env "ineq_chaining_right" ineq_chaining_right;
    Env.add_binary_inf ~env "eq_chaining_passive" eq_chaining_passive;
    Env.add_binary_inf ~env "eq_chaining_active" eq_chaining_active;
    end;
  (* declare instance *)
  let instance = Ctx.add_order ~ctx ?proof ~less ~lesseq in
  (* add clauses *)
  let clauses = axioms ~ctx:(Env.ctx env) ~instance in
  Env.add_passive ~env (Sequence.of_list clauses);
  ()

let setup_penv ~penv =
  ()  (* TODO? *)

let setup_env ~env =
  (* plug in the meta-prover *)
  match Env.get_meta ~env with
  | None -> ()
  | Some meta ->
    (* react to future detected theories *)
    let signal = MetaProver.on_theory (MetaProverState.prover meta) in
    Signal.on signal
      (function
        | MetaKB.NewTheory ("total_order",
          [{HOT.term=HOT.Const less}; {HOT.term=HOT.Const lesseq}], lit) ->
          let proof = MetaProverState.explain meta lit in
          add_order ~env ~proof ~less ~lesseq; true
        | _ -> true);
    (* see whether total order instances have already been detected *)
    Sequence.iter
      (function
        | MetaKB.NewTheory ("total_order",
          [{HOT.term=HOT.Const less}; {HOT.term=HOT.Const lesseq}], lit) ->
          let proof = MetaProverState.explain meta lit in
          add_order ~env ~proof ~less ~lesseq
        | _ -> ())
      (MetaKB.cur_theories (MetaProverState.reasoner meta));
    ()

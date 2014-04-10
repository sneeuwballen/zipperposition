
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

module T = FOTerm
module HOT = HOTerm
module C = Clause
module Lit = Literal
module Lits = Literal.Arr
module TO = Theories.TotalOrder
module Ord = Ordering
module S = Substs

let stat_eq_chaining = Util.mk_stat "eq_chaining"
let stat_ineq_chaining = Util.mk_stat "ineq_chaining"
let stat_semantic_tautology = Util.mk_stat "chaining_semantic_tauto"

let prof_eq_chaining_active = Util.mk_profiler "chaining.eq_active"
let prof_eq_chaining_passive = Util.mk_profiler "chaining.eq_passive"
let prof_ineq_chaining_left = Util.mk_profiler "chaining.ineq_left"
let prof_ineq_chaining_right = Util.mk_profiler "chaining.ineq_right"
let prof_semantic_tautology = Util.mk_profiler "chaining.semantic_tauto"

module type S = sig
  module Env : Env.S
  module C : module type of Env.C
  module PS : module type of Env.ProofState

  (** {6 Term Indices} *)

  val idx_ord_left : unit -> PS.TermIndex.t       (** terms at LHS of inequality *)
  val idx_ord_right : unit -> PS.TermIndex.t      (** terms at RHS of inequality *)
  val idx_ord_subterm : unit -> PS.TermIndex.t    (** subterms of inequality literals *)

  val eq_chaining_active : Env.binary_inf_rule
    (** Equality chaining where the clause is active *)

  val eq_chaining_passive : Env.binary_inf_rule
    (** Equality chaining where the clause is passive *)

  val ineq_chaining_left : Env.binary_inf_rule
    (** Inequality chaining where the clause is on the left. *)

  val ineq_chaining_right : Env.binary_inf_rule
    (** Inequality chaining where the clause is on the right. *)

  (* TODO: redundancy criterion:
     a<b subsumes c<d if it is known from unit facts
     that c<a and b<=d, or c<=a and b<d.

     Same as simplify-reflect for equality: maintain a global graph of
     ordering relations used to cut inconsistent literals. *)

  val is_semantic_tautology : C.t -> bool
    (** Check whether the clause is tautological for some ordering completion *)

  val reflexivity_res : Env.unary_inf_rule
    (** Reflexivity resolution *)

  val is_tautology : C.t -> bool
    (** C is always true in ordering models? *)

  val simplify : C.t -> C.t
    (** Simplify the clause, by removing impossible literals *)

  val axioms : instance:Theories.TotalOrder.instance -> C.t list
    (** Additional axioms for a total ordering *)

  (** {6 Env} *)

  val add_order : ?proof:Proof.t list ->
                  less:Symbol.t -> lesseq:Symbol.t -> ty:Type.t -> unit
    (** Declare a new total ordering instance *)

  val add_tstp_order : unit -> unit
end

(* option to enable/disable default instance *)
let __add_tptp_order = ref false

module Make(Sup : Superposition.S) = struct
  module Env = Sup.Env
  module PS = Env.ProofState
  module C = Env.C
  module Ctx = Env.Ctx
  module I = PS.TermIndex

  (** {2 Indexes} *)

  let _idx_ord_left = ref (PS.TermIndex.empty ())
  let _idx_ord_right = ref (PS.TermIndex.empty ())
  let _idx_ord_subterm = ref (PS.TermIndex.empty ())

  let idx_ord_left () = !_idx_ord_left
  let idx_ord_right () = !_idx_ord_right
  let idx_ord_subterm () = !_idx_ord_subterm

  let _update_idx f c =
    let spec = Ctx.Theories.total_order in
    let ord = Ctx.ord () in
    (* terms occurring immediately under an inequation, on LHS or RHS *)
    let left, right = Lits.fold_ineq ~spec
      ~eligible:(C.Eligible.chaining c) (C.lits c) (!_idx_ord_left, !_idx_ord_right)
      (fun (left,right) lit lit_pos ->
        let l = lit.TO.left in
        let with_pos = C.WithPos.( {term=l;
          pos=Position.(append lit_pos (left stop)); clause=c} ) in
        let left = f left l with_pos in
        let r = lit.TO.right in
        let with_pos = C.WithPos.( {term=r;
          pos=Position.(append lit_pos (right stop)); clause=c} ) in
        let right = f right r with_pos in
        left, right)
    in
    _idx_ord_left := left;
    _idx_ord_right := right;
    (* subterms occurring under an inequation *)
    _idx_ord_subterm := Lits.fold_terms ~ord ~which:`Both ~subterms:true
      ~eligible:(C.Eligible.chaining c) (C.lits c) !_idx_ord_subterm
      (fun tree t pos ->
        let pos_term = Lits.Pos.tail pos |> Lit.Pos.tail in
        match pos_term with
        | Position.Stop ->
          tree  (* this must be the inequality itself, not the subterms *)
        | _ ->
          let with_pos = C.WithPos.( {term=t; pos; clause=c} ) in
          f tree t with_pos);
    ()

  let () =
    Signal.on PS.ActiveSet.on_add_clause
      (fun c ->
        _update_idx PS.TermIndex.add c;
        Signal.ContinueListening);
    Signal.on PS.ActiveSet.on_remove_clause
      (fun c ->
        _update_idx PS.TermIndex.remove c;
        Signal.ContinueListening);
    ()

  (** {2 Helpers} *)

  (* finds all positions, in lits that are [eligible], that unify with the
    subterm at the given [pos], and return the list of such positions
    plus the new substitution *)
  let _gather_positions ~eligible ~signature lits scope pos subst =
    let t = Lits.Pos.at lits pos in
    (* position within the literal *)
    let pos' = Lits.Pos.tail pos in
    let positions, subst = Lits.fold_lits ~eligible lits ([pos], subst)
      (fun (pos_list, subst) lit i ->
        try
          let t' = Lit.Pos.at lit pos' in
          if T.is_var t'
            then raise Exit  (* variables are not eligible *)
            else
              let subst = Unif.FO.unification ~subst t scope t' scope in
              let pos_list = Position.(arg i pos') :: pos_list in
              pos_list, subst
        with Not_found | Unif.Fail | Exit | Invalid_argument _ ->
          pos_list, subst)
    in
    let positions = Util.list_uniq Position.eq positions in
    positions, subst

  (* check ordering conditions for the active clause in equality chaining *)
  let _check_eq_chaining_active active s_a active_pos subst =
    let s, t, sign = Lits.get_eqn (C.lits active) active_pos in
    assert (ScopedTerm.DB.closed (t : T.t :> ScopedTerm.t));
    sign &&
    begin (* s' not < t' *)
      let ord = Ctx.ord () in
      let renaming = Ctx.renaming_clear () in
      let s' = S.FO.apply ~renaming subst s s_a in
      let t' = S.FO.apply ~renaming subst t s_a in
      match Ord.compare ord s' t' with
      | Comparison.Lt -> false
      | _ -> true
    end &&
    begin (* eligible for paramodulation *)
      let i = Lits.Pos.idx active_pos in
      let bv = C.eligible_param active i subst in
      BV.get bv i
    end

  (* check ordering conditions for passive clause in equality chaining left *)
  let _check_eq_chaining_left_passive passive s_p positions subst =
    assert (positions <> []);
    let ord = Ctx.ord () in
    let spec = Ctx.Theories.total_order in
    let ord_lit = Lits.get_ineq ~spec (C.lits passive) (List.hd positions) in
    let instance = ord_lit.TO.instance in
    let renaming = S.Renaming.create () in
    let t1' = S.FO.apply ~renaming subst ord_lit.TO.left s_p in
    (* subst(t1) must be a maximal term *)
    Sequence.for_all
      (fun v ->
        let v' = S.FO.apply ~renaming subst v s_p in
        (* all other terms [v'] must not be bigger than [t1'] *)
        match Ord.compare ord t1' v' with
        | Comparison.Lt -> false
        | _ -> true
      )
      (Lits.terms_under_ineq ~instance (C.lits passive))
    &&
    (* each ti <| vi must verify that subst(ti) not < subst(vi) in
      the term ordering *)
    List.for_all
      begin fun pos ->
        let ord_lit = Lits.get_ineq ~spec (C.lits passive) pos in
        let renaming = Ctx.renaming_clear () in
        let ti = S.FO.apply ~renaming subst ord_lit.TO.left s_p in
        let vi = S.FO.apply ~renaming subst ord_lit.TO.right s_p in
        match Ord.compare ord ti vi with
        | Comparison.Lt -> false
        | _ -> true
      end
      positions

  (* check ordering conditions for passive clause in equality chaining right *)
  let _check_eq_chaining_right_passive passive s_p positions subst =
    assert (positions <> []);
    let ord = Ctx.ord () in
    let spec = Ctx.Theories.total_order in
    let ord_lit = Lits.get_ineq ~spec (C.lits passive) (List.hd positions) in
    let instance = ord_lit.TO.instance in
    let renaming = Ctx.renaming_clear () in
    let t1' = S.FO.apply ~renaming subst ord_lit.TO.right s_p in
    (* subst(t1) must be a maximal term *)
    Sequence.for_all
      (fun v ->
        let v' = S.FO.apply ~renaming subst v s_p in
        (* all other terms [v'] must not be bigger than [t1'] *)
        match Ord.compare ord t1' v' with
        | Comparison.Lt -> false
        | _ -> true
      )
      (Lits.terms_under_ineq ~instance (C.lits passive))
    &&
    (* each vi <| ti must verify that subst(ti) not < subst(vi) in
      the term ordering *)
    List.for_all
      begin fun pos ->
        let ord_lit = Lits.get_ineq ~spec (C.lits passive) pos in
        let renaming = Ctx.renaming_clear () in
        let ti = S.FO.apply ~renaming subst ord_lit.TO.right s_p in
        let vi = S.FO.apply ~renaming subst ord_lit.TO.left s_p in
        match Ord.compare ord ti vi with
        | Comparison.Lt -> false
        | _ -> true
      end
      positions

  (* equality chaining left *)
  let do_eq_chaining_left active s_a active_pos passive s_p passive_pos subst acc =
    let signature = Ctx.signature () in
    let spec = Ctx.Theories.total_order in
    (* rewrite s into t *)
    let s, t, _ = Lits.get_eqn (C.lits active) active_pos in
    (* get all inequalities that can be factored with passive_pos: if
        the passive lit is t1[s1]_p <| v1, we want all the ti[si]_p < vi
        such that si is unifiable with s1 (and thus with [s]).
        We only consider literals that are inequations of the same instance
        as [t1 <| v1]. *)
    let ord_lit = Lits.get_ineq ~spec (C.lits passive) passive_pos in
    let eligible = C.Eligible.ineq_of passive ord_lit.TO.instance in
    let positions, subst =
      _gather_positions ~eligible ~signature (C.lits passive) s_p passive_pos subst in
    (* check ordering conditions, and well-typedness *)
    if _check_eq_chaining_active active s_a active_pos subst
    && _check_eq_chaining_left_passive passive s_p positions subst
      then begin
        (* now we can combine the two clauses *)
        let renaming = Ctx.renaming_clear () in
        (* literals of new clause: active... *)
        let lits_a = Util.array_except_idx (C.lits active) (Lits.Pos.idx active_pos) in
        let lits_a = Lit.apply_subst_list ~renaming subst lits_a s_a in
        (* and passive (subst then replace subterms) *)
        let lits_p = Array.copy (C.lits passive) in
        let lits_p = Lits.apply_subst ~renaming subst lits_p s_p in
        let t' = Substs.FO.apply ~renaming subst t s_a in
        List.iter
          (fun pos -> Lits.Pos.replace lits_p ~at:pos ~by:t')
          positions;
        let new_lits = lits_a @ Array.to_list lits_p in
        (* proof *)
        (*let premises = active.C.hcproof :: passive.C.hcproof :: instance.TO.proof in*)
        let premises = [C.proof active; C.proof passive] in
        let theories = ["total_order"] in
        let proof cc = Proof.mk_c_inference ~theories ~rule:"eq_chain_left" cc premises in
        let parents = [active; passive] in
        let new_clause = C.create ~parents new_lits proof in
        Util.debug 3 "eq chaining left --> %a" C.pp new_clause;
        Util.incr_stat stat_eq_chaining;
        new_clause :: acc
      end else begin
        Util.debug 3 "eq chaining left between %a at %a, and %a at %a redundant"
          C.pp active Position.pp active_pos C.pp passive Position.pp passive_pos;
        acc
      end

  let do_eq_chaining_right active s_a active_pos passive s_p passive_pos subst acc =
    let signature = Ctx.signature () in
    let spec = Ctx.Theories.total_order in
    (* rewrite s into t *)
    let s, t, _ = Lits.get_eqn (C.lits active) active_pos in
    (* get all inequalities that can be factored with passive_pos: if
        the passive lit is v1 <| t1[s1]_p, we want all the vi <| ti[si]_p
        such that si is unifiable with s1 (and thus with [s]).
        We only consider literals that are inequations of the same instance
        as [v1 <| t1]. *)
    let ord_lit = Lits.get_ineq ~spec (C.lits passive) passive_pos in
    let eligible = C.Eligible.ineq_of passive ord_lit.TO.instance in
    let positions, subst =
      _gather_positions ~eligible ~signature (C.lits passive) s_p passive_pos subst in
    (* check ordering conditions, and well-typedness *)
    if _check_eq_chaining_active active s_a active_pos subst
    && _check_eq_chaining_right_passive passive s_p positions subst
      then begin
        (* now we can combine the two clauses *)
        let renaming = Ctx.renaming_clear () in
        (* literals of new clause: active... *)
        let lits_a = Util.array_except_idx (C.lits active) (Lits.Pos.idx active_pos) in
        let lits_a = Lit.apply_subst_list ~renaming subst lits_a s_a in
        (* and passive (subst then replace subterms) *)
        let lits_p = Array.copy (C.lits passive) in
        let lits_p = Lits.apply_subst ~renaming subst lits_p s_p in
        let t' = Substs.FO.apply ~renaming subst t s_a in
        List.iter
          (fun pos -> Lits.Pos.replace lits_p ~at:pos ~by:t')
          positions;
        let new_lits = lits_a @ Array.to_list lits_p in
        (* proof *)
        let premises = [C.proof active; C.proof passive]  (* instance.TO.proof *) in
        let theories = ["total_order"] in
        let proof cc = Proof.mk_c_inference ~theories ~rule:"eq_chain_right" cc premises in
        let parents = [active; passive] in
        let new_clause = C.create ~parents new_lits proof in
        Util.debug 3 "eq chaining right --> %a" C.pp new_clause;
        Util.incr_stat stat_eq_chaining;
        new_clause :: acc
      end else begin
        Util.debug 3 "eq chaining right between %a at %a, and %a at %a redundant"
          C.pp active Position.pp active_pos C.pp passive Position.pp passive_pos;
        acc
      end

  (* perform equality chaining *)
  let do_eq_chaining active s_a active_pos passive s_p passive_pos subst acc =
    Util.debug 5 "try eq chaining with %a at %a and %a at %a" C.pp active
      Position.pp active_pos C.pp passive Position.pp passive_pos;
    match passive_pos with
    | Position.Arg(_, Position.Left _) ->
      do_eq_chaining_left active s_a active_pos passive s_p passive_pos subst acc
    | Position.Arg(_, Position.Right _) ->
      do_eq_chaining_right active s_a active_pos passive s_p passive_pos subst acc
    | _ ->
      invalid_arg ("equality chaining: wrong pos " ^ (Position.to_string passive_pos))

  (* use [s = t] to rewrite subterms of active clauses that unify with [s]
      into [t] *)
  let eq_chaining_active c =
    Util.enter_prof prof_eq_chaining_active;
    let ord = Ctx.ord () in
    let eligible = C.Eligible.param c in
    (* fold on eq lits *)
    let new_clauses = Lits.fold_eqn ~ord ~both:true ~eligible (C.lits c) []
      (fun acc s t _ s_pos ->
        I.retrieve_unifiables !_idx_ord_subterm 1 s 0 acc
          (fun acc u_p with_pos subst ->
            let passive = with_pos.C.WithPos.clause in
            let u_pos = with_pos.C.WithPos.pos in
            do_eq_chaining c 0 s_pos passive 1 u_pos subst acc))
    in
    Util.exit_prof prof_eq_chaining_active;
    new_clauses

  (* rewrite subterms in inequalities of the given clause [c] using equations
      of active clauses *)
  let eq_chaining_passive c =
    Util.enter_prof prof_eq_chaining_passive;
    let passive = c in
    let scope = 1 in
    let spec = Ctx.Theories.total_order in
    let eligible = C.Eligible.chaining c in
    (* fold on ineq lits *)
    let new_clauses = Lits.fold_ineq ~spec ~eligible (C.lits c) []
      (fun acc ord_lit lit_pos ->
        let pb = Position.Build.of_pos lit_pos in
        (* factorize code for left and right terms *)
        let explore acc t pos =
          T.all_positions ~pos t acc
          (fun acc t_p passive_pos ->
            (* at this point, [t_p] is a subterm of a side of the inequation *)
            Sup.PS.TermIndex.retrieve_unifiables (Sup.idx_sup_from ()) scope t_p 0 acc
              (fun acc s with_pos subst ->
                (* [s] is the lhs of an equation in some clause, that can
                    potentially rewrite [t_p] into some smaller term *)
                let active = with_pos.C.WithPos.clause in
                let active_pos = with_pos.C.WithPos.pos in
                do_eq_chaining active scope active_pos passive 0 passive_pos subst acc))
        in
        let l = ord_lit.TO.left in
        let r = ord_lit.TO.right in
        let acc = explore acc l Position.Build.(left pb |> to_pos) in
        let acc = explore acc r Position.Build.(right pb |> to_pos) in
        acc)
    in
    Util.exit_prof prof_eq_chaining_passive;
    new_clauses

  (* literals whose position does not occur in [positions] *)
  let _all_lits_but_positions lits positions =
    let bv = BV.create ~size:(Array.length lits) true in
    List.iter
      (fun pos -> match pos with
        | Position.Arg(i, _) -> BV.reset bv i
        | _ -> assert false)
      positions;
    BV.select bv lits

  (* inequality chaining between two clauses *)
  let do_ineq_chaining left s_left left_pos right s_right right_pos subst acc =
    let spec = Ctx.Theories.total_order in
    Util.debug 5 "ineq_chaining between %a at %a and %a at %a" C.pp left
      Position.pp left_pos C.pp right Position.pp right_pos;
    let signature = Ctx.signature () in
    let instance = (Lits.get_ineq ~spec (C.lits left) left_pos).TO.instance in
    let mk_less t1 t2 = Lit.mk_true
      (T.app_full (TO.less_const ~instance) [T.ty t1] [t1; t2]) in
    let t1 = (Lits.get_ineq ~spec (C.lits right) right_pos).TO.left in
    (* find other inequality literals that can be chained on *)
    let eligible c = C.Eligible.ineq_of c instance in
    let left_pos_list, subst =
      _gather_positions ~eligible:(eligible left) ~signature
      (C.lits left) s_left left_pos subst in
    let right_pos_list, subst =
      _gather_positions ~eligible:(eligible right) ~signature
      (C.lits right) s_right right_pos subst in
    Util.debug 5 "positions for left: [%a]" (Util.pp_list Position.pp) left_pos_list;
    Util.debug 5 "positions for right: [%a]" (Util.pp_list Position.pp) right_pos_list;
    (* check ordering conditions. Note that the conditions are inversed w.r.t
      equality chaining (ie, in left clause, right hand side terms must
      be maximal, and conversely *)
    if _check_eq_chaining_right_passive left s_left left_pos_list subst
    && _check_eq_chaining_left_passive right s_right right_pos_list subst
      then begin
        let renaming = Ctx.renaming_clear () in
        (* build literal list *)
        let lits_left = _all_lits_but_positions (C.lits left) left_pos_list in
        let lits_left = Lit.apply_subst_list ~renaming subst lits_left s_left in
        let lits_right = _all_lits_but_positions (C.lits right) right_pos_list in
        let lits_right = Lit.apply_subst_list ~renaming subst lits_right s_right in
        let product = List.fold_left
          (fun acc left_pos ->
            let lit_left = Lits.get_ineq ~spec (C.lits left) left_pos in
            let ui = lit_left.TO.left in
            List.fold_left
              (fun acc right_pos ->
                let lit_right = Lits.get_ineq ~spec (C.lits right) right_pos in
                let vj = lit_right.TO.right in
                (* now to compute INEQ(ui, vj) *)
                let ui' = S.FO.apply ~renaming subst ui s_left in
                let vj' = S.FO.apply ~renaming subst vj s_right in
                let t1' = S.FO.apply ~renaming subst t1 s_right in
                if lit_left.TO.strict || lit_right.TO.strict
                  then (* ui < vj *)
                    mk_less ui' vj' :: acc
                  else (* ui < vj OR ui = t1 *)
                    mk_less ui' vj' :: Lit.mk_eq ui' t1' :: acc
                )
              acc right_pos_list)
          [] left_pos_list
        in
        let lits = lits_left @ lits_right @ product in
        (* build clause *)
        let theories = ["total_order"] in
        let rule = "inequality_chaining" in
        let premises = [C.proof left; C.proof right] in (* instance.TO.proof *)
        let proof cc = Proof.mk_c_inference ~theories ~rule cc premises in
        let new_clause = C.create ~parents:[left;right] lits proof in
        Util.debug 3 "ineq_chaining of %a and %a gives %a"
          C.pp left C.pp right C.pp new_clause;
        Util.incr_stat stat_ineq_chaining;
        new_clause :: acc
      end else
        let () = Util.debug 5 "ordering conditions won't do." in
        acc

  (* inequality chaining, where [c] is on the left (which means terms on
      the RHS of inequalities will be chained) *)
  let ineq_chaining_left c =
    Util.enter_prof prof_ineq_chaining_left;
    let spec = Ctx.Theories.total_order in
    let eligible = C.Eligible.chaining c in
    (* fold on ineq lits *)
    let new_clauses = Lits.fold_ineq ~spec ~eligible (C.lits c) []
      (fun acc ord_lit lit_pos ->
        let s1 = ord_lit.TO.right in
        let left_pos = Position.(append lit_pos (right stop)) in
        Util.debug 5 "try left ineq chaining with %a at %a" C.pp c Position.pp left_pos;
        I.retrieve_unifiables !_idx_ord_left 1 s1 0 acc
          (fun acc t1 with_pos subst ->
            let right = with_pos.C.WithPos.clause in
            let right_pos = with_pos.C.WithPos.pos in
            do_ineq_chaining c 0 left_pos right 1 right_pos subst acc))
    in
    Util.exit_prof prof_ineq_chaining_left;
    new_clauses

  (* inequality chaining where [c] is on the right *)
  let ineq_chaining_right c =
    Util.enter_prof prof_ineq_chaining_right;
    let spec = Ctx.Theories.total_order in
    let eligible = C.Eligible.chaining c in
    (* fold on ineq lits *)
    let new_clauses = Lits.fold_ineq ~spec ~eligible (C.lits c) []
      (fun acc ord_lit lit_pos ->
        let t1 = ord_lit.TO.left in
        let right_pos = Position.(append lit_pos (left stop)) in
        Util.debug 5 "try right ineq chaining with %a at %a" C.pp c Position.pp right_pos;
        I.retrieve_unifiables !_idx_ord_right 1 t1 0 acc
          (fun acc s1 with_pos subst ->
            let left = with_pos.C.WithPos.clause in
            let left_pos = with_pos.C.WithPos.pos in
            do_ineq_chaining left 1 left_pos c 0 right_pos subst acc))
    in
    Util.exit_prof prof_ineq_chaining_right;
    new_clauses

  let reflexivity_res c =
    let spec = Ctx.Theories.total_order in
    let eligible = C.Eligible.max c in
    Lits.fold_ineq ~spec ~eligible (C.lits c) []
      (fun acc lit lit_pos ->
        if lit.TO.strict
          then try
            let subst = Unif.FO.unification lit.TO.left 0 lit.TO.right 0 in
            (* remove lit and make a new clause after substitution *)
            let i = Lits.Pos.idx lit_pos in
            let lits = Util.array_except_idx (C.lits c) i in
            let renaming = Ctx.renaming_clear () in
            let lits = Lit.apply_subst_list ~renaming subst lits 0 in
            let premises = [C.proof c] (* lit.TO.instance.TO.proof *) in
            let theories = ["total_order"] in
            let rule = "reflexivity_res" in
            let proof cc = Proof.mk_c_inference ~theories ~rule cc premises in
            let new_c = C.create ~parents:[c] lits proof in
            Util.debug 3 "reflexivity res of %a gives %a" C.pp c C.pp new_c;
            new_c :: acc
          with Unif.Fail -> acc
        else acc)

  let is_tautology c =
    let spec = Ctx.Theories.total_order in
    let eligible = C.Eligible.always in
    try
      Lits.fold_ineq ~spec ~eligible (C.lits c) ()
        (fun () lit _lit_pos ->
          if not lit.TO.strict && T.eq lit.TO.left lit.TO.right
            (* a <= a is definitely a tautology *)
            then raise Exit);
      false
    with Exit ->
      true

  (* redundancy criterion: if variables are replaced by constants,
     do equations and inequations in left part of =>
     always imply something on the right part of => ?

     e,g, transitivity is redundant, because we have
     ~ x<y | ~ y<z | x<z, once simplified and grounded,
     x < y & y < z ==> x < z is always trivial

     We consider that  a <= b is negation for b < a, and use the congruence
     closure (same as for {!Superposition.is_semantic_tautology}) *)
  let is_semantic_tautology c =
    Util.enter_prof prof_semantic_tautology;
    let spec = Ctx.Theories.total_order in
    (* find ordering instances *)
    let instances = Lits.order_instances ~spec (C.lits c) in
    let res = List.exists
      (fun instance ->
        let cc = Congruence.FO.create ~size:13 () in
        (* ineq to checks afterward *)
        let to_check = ref [] in
        (* build congruence *)
        Array.iter
          (fun lit -> match lit with
          | Lit.Equation (l, r, false) -> Congruence.FO.mk_eq cc l r
          | Lit.Equation (l, r, true) -> to_check := `Eq (l,r) :: !to_check
          | Lit.Prop (_, true) ->
            begin try
              let olit = Lit.ineq_lit_of ~instance lit in
              if olit.TO.strict
                then to_check := `Lt (olit.TO.left, olit.TO.right) :: !to_check
                else
                  (* left <= right ----> we add  right < less to CC *)
                  let l = olit.TO.left in
                  let r = olit.TO.right in
                  Congruence.FO.mk_less cc r l
            with Not_found -> ()
            end
          | _ -> ()
          )
          (C.lits c);
        (* check if inequality holds in congruence OR congruence tautological *)
        Congruence.FO.cycles cc ||
        List.exists
          (function
          | `Eq (l,r) -> Congruence.FO.is_eq cc l r
          | `Lt (l,r) -> Congruence.FO.is_less cc l r)
          !to_check)
      instances
    in
    if res then begin
      Util.incr_stat stat_semantic_tautology;
      Util.debug 2 "%a is a chaining semantic tautology" C.pp c;
      end;
    Util.exit_prof prof_semantic_tautology;
    res

  let simplify c =
    let spec = Ctx.Theories.total_order in
    let eligible = C.Eligible.always in
    let size = Array.length (C.lits c) in
    let bv = BV.create ~size true in
    let instances = ref [] in
    (* remove absurd literals from [bv] *)
    Lits.fold_ineq ~spec ~eligible (C.lits c) ()
      (fun () lit lit_pos ->
        if lit.TO.strict && T.eq lit.TO.left lit.TO.right
          then begin
            (* eliminate literal a <= a *)
            let i = Lits.Pos.idx lit_pos in
            BV.reset bv i;
            instances := lit.TO.instance :: !instances
          end);
    if BV.cardinal bv < size
      then begin
        let lits = BV.select bv (C.lits c) in
        let theories = ["total_order"] in
        instances := Util.list_uniq TO.eq !instances;
        (*
        let proofs = Util.list_flatmap (fun instance -> instance.TO.proof) !instances in
        *)
        let proofs = [] in
        let rule = "total_order_simplify" in
        let proof cc = Proof.mk_c_inference ~theories ~rule cc (C.proof c :: proofs) in
        let new_c = C.create ~parents:[c] lits proof in
        Util.debug 3 "total_order_simplify %a into %a" C.pp c C.pp new_c;
        new_c
      end else
        c

  let axioms ~instance =
    []

  (** {2 Env} *)

  let _setup_rules () =
    Util.debug 3 "setup chaining inferences";
    Env.add_is_trivial is_tautology;
    Env.add_is_trivial is_semantic_tautology;
    Env.add_simplify simplify;
    Env.add_unary_inf "reflexivity_res" reflexivity_res;
    Env.add_binary_inf "ineq_chaining_left" ineq_chaining_left;
    Env.add_binary_inf "ineq_chaining_right" ineq_chaining_right;
    Env.add_binary_inf "eq_chaining_passive" eq_chaining_passive;
    Env.add_binary_inf "eq_chaining_active" eq_chaining_active;
    ()

  (* we have to enable the chaining inferences on the first ordering *)
  let _setup_rules_if_first () =
    let exists_some = Theories.TotalOrder.exists_order Env.Ctx.Theories.total_order in
    if not exists_some then _setup_rules ();
    ()

  let add_order ?proof ~less ~lesseq ~ty =
    Util.debug 1 "enable chaining for order %a, %a (type %a)"
      Symbol.pp less Symbol.pp lesseq Type.pp ty;
    (* declare instance *)
    let instance = Ctx.Theories.add_order ?proof ~less ~lesseq ~ty in
    (* add clauses *)
    let clauses = axioms ~instance in
    Env.add_passive (Sequence.of_list clauses);
    ()

  let add_tstp_order () =
    Util.debug 1 "enable chaining for TSTP order";
    (* add instance *)
    _setup_rules_if_first ();
    let instance = Env.Ctx.Theories.add_tstp_order () in
    (* add clauses *)
    let clauses = axioms ~instance in
    Env.add_passive (Sequence.of_list clauses);
    ()

  let register () =
    Util.debug 2 "register chaining...";
    let spec = Ctx.Theories.total_order in
    let signal = Theories.TotalOrder.on_add_instance spec in
    (* TODO: index active clauses upon signal? too late? *)
    Signal.on signal
      (fun instance ->
        _setup_rules_if_first ();
        Signal.ContinueListening
      );
    if Theories.TotalOrder.exists_order ~spec
      then _setup_rules_if_first ();
    (* add TPTP order *)
    if !__add_tptp_order then add_tstp_order ();
    ()
end

let setup_penv penv =
  ()

let extension =
  let module DOIT(Env : Env.S) = struct
    include Extensions.MakeAction(Env)
    let actions =
      try
        let sup = Mixtbl.find ~inj:Superposition.key Env.mixtbl "superposition" in
        let module Sup = (val sup : Superposition.S) in
        let module Chaining = Make(Sup) in
        [Ext_general Chaining.register]
      with Not_found ->
        Printf.eprintf "Chaining needs Superposition to work";
        exit 1
  end
  in
  { Extensions.name="chaining";
    Extensions.penv_actions = [Extensions.Ext_penv_do setup_penv];
    Extensions.make=(module DOIT : Extensions.ENV_TO_S);
  }

let () =
  Params.add_opts
    [ "-chaining"
    , Arg.Unit (fun () -> Extensions.register extension)
    , "enable chaining"
    ; "-chaining-tptp"
    , Arg.Set __add_tptp_order
    , "enable TPTP standard ordering $less,$lesseq"
    ];
  ()


(* This file is free software, part of Zipperposition. See file "license" for more details. *)

open Logtk
open Libzipperposition

module BV = CCBV
module T = Term
module O = Ordering
module S = Subst
module Lit = Literal
module Lits = Literals
module Comp = Comparison
module US = Unif_subst

let section = Util.Section.make ~parent:Const.section "sup"

(* flag meaning the clause has been simplified already *)
let flag_simplified = SClause.new_flag()

module type S = Superposition_intf.S

(* statistics *)
let stat_basic_simplify_calls = Util.mk_stat "sup.basic_simplify calls"
let stat_basic_simplify = Util.mk_stat "sup.basic_simplify"
let stat_superposition_call = Util.mk_stat "sup.superposition calls"
let stat_equality_resolution_call = Util.mk_stat "sup.equality_resolution calls"
let stat_equality_factoring_call = Util.mk_stat "sup.equality_factoring calls"
let stat_subsumption_call = Util.mk_stat "sup.subsumption_calls"
let stat_eq_subsumption_call = Util.mk_stat "sup.equality_subsumption calls"
let stat_eq_subsumption_success = Util.mk_stat "sup.equality_subsumption success"
let stat_subsumed_in_active_set_call = Util.mk_stat "sup.subsumed_in_active_set calls"
let stat_subsumed_by_active_set_call = Util.mk_stat "sup.subsumed_by_active_set calls"
let stat_clauses_subsumed = Util.mk_stat "sup.num_clauses_subsumed"
let stat_demodulate_call = Util.mk_stat "sup.demodulate calls"
let stat_demodulate_step = Util.mk_stat "sup.demodulate steps"
let stat_semantic_tautology = Util.mk_stat "sup.semantic_tautologies"
let stat_condensation = Util.mk_stat "sup.condensation"
let stat_clc = Util.mk_stat "sup.clc"

let prof_demodulate = Util.mk_profiler "sup.demodulate"
let prof_back_demodulate = Util.mk_profiler "sup.backward_demodulate"
let prof_pos_simplify_reflect = Util.mk_profiler "sup.simplify_reflect+"
let prof_neg_simplify_reflect = Util.mk_profiler "sup.simplify_reflect-"
let prof_clc = Util.mk_profiler "sup.contextual_literal_cutting"
let prof_semantic_tautology = Util.mk_profiler "sup.semantic_tautology"
let prof_condensation = Util.mk_profiler "sup.condensation"
let prof_basic_simplify = Util.mk_profiler "sup.basic_simplify"
let prof_subsumption = Util.mk_profiler "sup.subsumption"
let prof_eq_subsumption = Util.mk_profiler "sup.equality_subsumption"
let prof_subsumption_set = Util.mk_profiler "sup.forward_subsumption"
let prof_subsumption_in_set = Util.mk_profiler "sup.backward_subsumption"
let prof_infer_active = Util.mk_profiler "sup.infer_active"
let prof_infer_passive = Util.mk_profiler "sup.infer_passive"
let prof_infer_equality_resolution = Util.mk_profiler "sup.infer_equality_resolution"
let prof_infer_equality_factoring = Util.mk_profiler "sup.infer_equality_factoring"

let _use_semantic_tauto = ref true
let _use_simultaneous_sup = ref true
let _dot_sup_into = ref None
let _dot_sup_from = ref None
let _dot_simpl = ref None
let _dont_simplify = ref false
let _sup_at_vars = ref false
let _restrict_hidden_sup_at_vars = ref false
let _dot_demod_into = ref None

module Make(Env : Env.S) : S with module Env = Env = struct
  module Env = Env
  module Ctx = Env.Ctx
  module C = Env.C
  module PS = Env.ProofState
  module I = PS.TermIndex
  module TermIndex = PS.TermIndex
  module SubsumIdx = PS.SubsumptionIndex
  module UnitIdx = PS.UnitIndex

  (** {6 Index Management} *)

  let _idx_sup_into = ref (TermIndex.empty ())
  let _idx_sup_from = ref (TermIndex.empty ())
  let _idx_back_demod = ref (TermIndex.empty ())
  let _idx_fv = ref (SubsumIdx.empty ())
  let _idx_simpl = ref (UnitIdx.empty ())

  let idx_sup_into () = !_idx_sup_into
  let idx_sup_from () = !_idx_sup_from
  let idx_fv () = !_idx_fv

  (* apply operation [f] to some parts of the clause [c] just added/removed
     from the active set *)
  let _update_active f c =
    let ord = Ctx.ord () in
    (* index subterms that can be rewritten by superposition *)
    _idx_sup_into :=
      Lits.fold_terms ~vars:!_sup_at_vars ~ty_args:false ~ord ~which:`Max ~subterms:true
        ~eligible:(C.Eligible.res c) (C.lits c)
      |> Sequence.filter (fun (t, _) -> not (T.is_var t) || T.is_ho_var t)
      (* TODO: could exclude more variables from the index:
         they are not needed if they occur with the same args everywhere in the clause *)
      |> Sequence.fold
        (fun tree (t, pos) ->
           let with_pos = C.WithPos.({term=t; pos; clause=c;}) in
           f tree t with_pos)
        !_idx_sup_into;
    (* index terms that can rewrite into other clauses *)
    _idx_sup_from :=
      Lits.fold_eqn ~ord ~both:true ~sign:true
        ~eligible:(C.Eligible.param c) (C.lits c)
      |> Sequence.fold
        (fun tree (l, _, sign, pos) ->
           assert sign;
           let with_pos = C.WithPos.({term=l; pos; clause=c;}) in
           f tree l with_pos)
        !_idx_sup_from ;
    (* terms that can be demodulated: all subterms (but vars) *)
    _idx_back_demod :=
      Lits.fold_terms ~vars:false ~ty_args:false ~ord ~subterms:true ~which:`All
        ~eligible:C.Eligible.always (C.lits c)
      |> Sequence.fold
        (fun tree (t, pos) ->
           let with_pos = C.WithPos.( {term=t; pos; clause=c} ) in
           f tree t with_pos)
        !_idx_back_demod;
    Signal.ContinueListening

  (* update simpl. index using the clause [c] just added or removed to
     the simplification set *)
  let _update_simpl f c =
    let ord = Ctx.ord () in
    let idx = !_idx_simpl in
    let idx' = match C.lits c with
      | [| Lit.Equation (l,r,true) |] ->
        begin match Ordering.compare ord l r with
          | Comparison.Gt ->
            f idx (l,r,true,c)
          | Comparison.Lt ->
            f idx (r,l,true,c)
          | Comparison.Incomparable ->
            let idx = f idx (l,r,true,c) in
            f idx (r,l,true,c)
          | Comparison.Eq -> idx  (* no modif *)
        end
      | [| Lit.Equation (l,r,false) |] ->
        f idx (l,r,false,c)
      | [| Lit.Prop (p, sign) |] ->
        f idx (p,T.true_,sign,c)
      | _ -> idx
    in
    _idx_simpl := idx';
    Signal.ContinueListening

  let () =
    Signal.on PS.ActiveSet.on_add_clause
      (fun c ->
         _idx_fv := SubsumIdx.add !_idx_fv c;
         _update_active TermIndex.add c);
    Signal.on PS.ActiveSet.on_remove_clause
      (fun c ->
         _idx_fv := SubsumIdx.remove !_idx_fv c;
         _update_active TermIndex.remove c);
    Signal.on PS.SimplSet.on_add_clause
      (_update_simpl UnitIdx.add);
    Signal.on PS.SimplSet.on_remove_clause
      (_update_simpl UnitIdx.remove);
    ()

  (** {6 Inference Rules} *)

  (* all the information needed for a superposition inference *)
  module SupInfo = struct
    type t = {
      active : C.t;
      active_pos : Position.t; (* position of [s] *)
      scope_active : int;
      s : T.t; (* lhs of rule *)
      t : T.t; (* rhs of rule *)
      passive : C.t;
      passive_pos : Position.t; (* position of [u_p] *)
      passive_lit : Lit.t;
      scope_passive : int;
      u_p : T.t; (* rewritten subterm *)
      subst : US.t;
    }
  end

  exception ExitSuperposition of string

  (* check for hidden superposition at variables,
     e.g. superposing g x = f x into h (x b) = a to give h (f b) = a.
     Returns a term only containing the concerned variable
     and a term consisting of the part of info.t that unifies with the variable,
     e.g. (x, f) in the example above. *)
  let is_hidden_sup_at_var info =
    let open SupInfo in
    let active_idx = Lits.Pos.idx info.active_pos in
    begin match T.view info.u_p with
      | T.App (head, args) ->
        begin match T.as_var head with
          | Some _ ->
            (* rewritten term is variable-headed *)
            begin match T.view info.s, T.view info.t  with
              | T.App (f, ss), T.App (g, tt) ->
                let s_args = Array.of_list ss in
                let t_args = Array.of_list tt in
                if
                  Array.length s_args >= List.length args
                  && Array.length t_args >= List.length args
                  (* Check whether the last argument(s) of s and t are equal *)
                  && Array.sub s_args (Array.length s_args - List.length args) (List.length args) =
                  Array.sub t_args (Array.length t_args - List.length args) (List.length args)
                  (* Check whether they are all variables that occur nowhere else *)
                  && CCList.(Array.length s_args - List.length args --^ Array.length s_args)
                     |> List.for_all (fun idx ->
                       match T.as_var (Array.get s_args idx) with
                         | Some v ->
                           (* Check whether variable occurs in previous arguments: *)
                           not (CCArray.exists (T.var_occurs ~var:v) (Array.sub s_args 0 idx))
                           && not (CCArray.exists (T.var_occurs ~var:v) (Array.sub t_args 0 (Array.length t_args - List.length args))
                                   (* Check whether variable occurs in heads: *)
                                   && not (T.var_occurs ~var:v f)
                                   && not (T.var_occurs ~var:v g)
                                   (* Check whether variable occurs in other literals: *)
                                   && not (List.exists (Literal.var_occurs v) (CCArray.except_idx (C.lits info.active) active_idx)))
                         | None -> false
                     )
                then
                  (* Calculate the part of t that unifies with the variable *)
                  let t_prefix = T.app g (Array.to_list (Array.sub t_args 0 (Array.length t_args - List.length args))) in
                  Some (head, t_prefix)
                else
                  None
              | _ -> None
            end
          | None -> None
        end
      | _ -> None
    end

  (* Checks whether we must allow superposition at variables to be complete. *)
  let sup_at_var_condition info var replacement =
    let open SupInfo in
    let ord = Ctx.ord () in
    let us = info.subst in
    let subst = US.subst us in
    let renaming = S.Renaming.create () in
    let replacement' = S.FO.apply renaming subst (replacement, info.scope_active) in
    let var' = S.FO.apply renaming subst (var, info.scope_passive) in
    if (not (Type.is_fun (Term.ty var')) || not (O.might_flip ord var' replacement'))
    then (
      Util.debugf ~section 5
        "Cannot flip: %a = %a"
        (fun k->k T.pp var' T.pp replacement');
      false (* If the lhs vs rhs cannot flip, we don't need a sup at var *)
    )
    else (
      (* Check whether var occurs only with the same arguments everywhere. *)
      let unique_args_of_var =
        C.lits info.passive
        |> Lits.fold_terms ~vars:true ~ty_args:false ~which:`All ~ord ~subterms:true ~eligible:(fun _ _ -> true)
        |> Sequence.fold_while
          (fun unique_args (t,_) ->
             if Head.term_to_head t == Head.term_to_head var
             then (
               if unique_args == Some (Head.term_to_args t)
               then (unique_args, `Continue) (* found the same arguments of var again *)
               else (None, `Stop) (* different arguments of var found *)
             ) else (unique_args, `Continue) (* this term doesn't have var as head *)
          )
          None
      in
      match unique_args_of_var with
        | Some _ ->
          Util.debugf ~section 5
            "Variable %a has same args everywhere in %a"
            (fun k->k T.pp var C.pp info.passive);
          false (* If var occurs with the same arguments everywhere, we don't need sup at vars *)
        | None ->
          (* Check whether Cσ is >= C[var -> replacement]σ *)
          let passive'_lits = Lits.apply_subst renaming subst (C.lits info.passive, info.scope_passive) in
          let subst_t = Unif.FO.update subst (T.as_var_exn var, info.scope_passive) (replacement, info.scope_active) in
          let passive_t'_lits = Lits.apply_subst renaming subst_t (C.lits info.passive, info.scope_passive) in
          if Lits.compare_multiset ~ord passive'_lits passive_t'_lits = Comp.Gt
          then (
            Util.debugf ~section 5
              "Sup at var condition is not fulfilled because: %a >= %a"
              (fun k->k Lits.pp passive'_lits Lits.pp passive_t'_lits);
            false
          )
          else true (* If Cσ is either <= or incomparable to C[var -> replacement]σ, we need sup at var.*)
    )


  (* Helper that does one or zero superposition inference, with all
     the given parameters. Clauses have a scope. *)
  let do_classic_superposition info acc =
    let ord = Ctx.ord () in
    let open SupInfo in
    let module P = Position in
    Util.incr_stat stat_superposition_call;
    let sc_a = info.scope_active in
    let sc_p = info.scope_passive in
    Util.debugf ~section 3
      "@[<2>sup@ (@[<2>%a[%d]@ @[s=%a@]@ @[t=%a@]@])@ \
       (@[<2>%a[%d]@ @[passive_lit=%a@]@ @[p=%a@]@])@ with subst=@[%a@]@]"
      (fun k->k C.pp info.active sc_a T.pp info.s T.pp info.t
          C.pp info.passive sc_p Lit.pp info.passive_lit
          Position.pp info.passive_pos US.pp info.subst);
    assert (InnerTerm.DB.closed (info.s:>InnerTerm.t));
    assert (InnerTerm.DB.closed (info.u_p:T.t:>InnerTerm.t));
    assert (not(T.is_var info.u_p) || T.is_ho_var info.u_p);
    let active_idx = Lits.Pos.idx info.active_pos in
    let passive_idx, passive_lit_pos = Lits.Pos.cut info.passive_pos in
    try
      let renaming = S.Renaming.create () in
      let us = info.subst in
      let subst = US.subst us in
      let t' = S.FO.apply renaming subst (info.t, sc_a) in
      begin match info.passive_lit, info.passive_pos with
        | Lit.Prop (_, true), P.Arg(_, P.Left P.Stop) ->
          if T.equal t' T.true_
          then raise (ExitSuperposition "will yield a bool tautology")
        | Lit.Equation (_, v, true), P.Arg(_, P.Left P.Stop)
        | Lit.Equation (v, _, true), P.Arg(_, P.Right P.Stop) ->
          (* are we in the specific, but no that rare, case where we
             rewrite s=t using s=t (into a tautology t=t)? *)
          (* TODO: use Unif.FO.eq? *)
          let v' = S.FO.apply renaming subst (v, sc_p) in
          if T.equal t' v'
          then raise (ExitSuperposition "will yield a tautology");
        | _ -> ()
      end;
      let passive_lit' = Lit.apply_subst_no_simp renaming subst (info.passive_lit, sc_p) in
      let new_trail = C.trail_l [info.active; info.passive] in
      if Env.is_trivial_trail new_trail then raise (ExitSuperposition "trivial trail");
      let s' = S.FO.apply renaming subst (info.s, sc_a) in
      if (
        O.compare ord s' t' = Comp.Lt ||
        not (Lit.Pos.is_max_term ~ord passive_lit' passive_lit_pos) ||
        not (BV.get (C.eligible_res (info.passive, sc_p) subst) passive_idx) ||
        not (C.is_eligible_param (info.active, sc_a) subst ~idx:active_idx)
      ) then raise (ExitSuperposition "bad ordering conditions");
      (* Check for superposition at a variable *)
      if not !_sup_at_vars then
        assert (not (T.is_var info.u_p))
      else if T.is_var info.u_p && not (sup_at_var_condition info info.u_p info.t) then
        raise (ExitSuperposition "superposition at variable");
      (* Check for hidden superposition at a variable *)
      if !_restrict_hidden_sup_at_vars then (
        match is_hidden_sup_at_var info with
          | Some (var,replacement) when not (sup_at_var_condition info var replacement)
            -> raise (ExitSuperposition "hidden superposition at variable")
          | _ -> ()
      );
      (* ordering constraints are ok *)
      let lits_a = CCArray.except_idx (C.lits info.active) active_idx in
      let lits_p = CCArray.except_idx (C.lits info.passive) passive_idx in
      (* replace s\sigma by t\sigma in u|_p\sigma *)
      let new_passive_lit =
        Lit.Pos.replace passive_lit'
          ~at:passive_lit_pos ~by:t' in
      let c_guard = Literal.of_unif_subst renaming us in
      let tags = Unif_subst.tags us in
      (* apply substitution to other literals *)
      let new_lits =
        new_passive_lit ::
          c_guard @
          Lit.apply_subst_list renaming subst (lits_a, sc_a) @
          Lit.apply_subst_list renaming subst (lits_p, sc_p)
      in
      let rule =
        let name = if Lit.sign passive_lit' then "sup+" else "sup-" in
        Proof.Rule.mk name
      in
      let proof =
        Proof.Step.inference ~rule ~tags
          [C.proof_parent_subst renaming (info.active,sc_a) subst;
           C.proof_parent_subst renaming (info.passive,sc_p) subst]
      and penalty =
        C.penalty info.active
        + C.penalty info.passive
        + (if T.is_var s' then 2 else 0) (* superposition from var = bad *)
      in
      let new_clause = C.create ~trail:new_trail ~penalty new_lits proof in
      Util.debugf ~section 3 "@[... ok, conclusion@ @[%a@]@]" (fun k->k C.pp new_clause);
      new_clause :: acc
    with ExitSuperposition reason ->
      Util.debugf ~section 3 "... cancel, %s" (fun k->k reason);
      acc

  (* simultaneous superposition: when rewriting D with C \lor s=t,
      replace s with t everywhere in D rather than at one place. *)
  let do_simultaneous_superposition info acc =
    let ord = Ctx.ord () in
    let open SupInfo in
    let module P = Position in
    Util.incr_stat stat_superposition_call;
    let sc_a = info.scope_active in
    let sc_p = info.scope_passive in
    Util.debugf ~section 3
      "@[<hv2>simultaneous sup@ \
       @[<2>active@ %a[%d]@ s=@[%a@]@ t=@[%a@]@]@ \
       @[<2>passive@ %a[%d]@ passive_lit=@[%a@]@ p=@[%a@]@]@ with subst=@[%a@]@]"
      (fun k->k C.pp info.active sc_a T.pp info.s T.pp info.t
          C.pp info.passive sc_p Lit.pp info.passive_lit
          Position.pp info.passive_pos US.pp info.subst);
    assert (InnerTerm.DB.closed (info.s:>InnerTerm.t));
    assert (InnerTerm.DB.closed (info.u_p:T.t:>InnerTerm.t));
    assert (not(T.is_var info.u_p) || T.is_ho_var info.u_p);
    let active_idx = Lits.Pos.idx info.active_pos in
    let passive_idx, passive_lit_pos = Lits.Pos.cut info.passive_pos in
    try
      let renaming = S.Renaming.create () in
      let us = info.subst in
      let subst = US.subst us in
      let t' = S.FO.apply renaming subst (info.t, sc_a) in
      begin match info.passive_lit, info.passive_pos with
        | Lit.Prop (_, true), P.Arg(_, P.Left P.Stop) ->
          if T.equal t' T.true_
          then raise (ExitSuperposition "will yield a bool tautology")
        | Lit.Equation (_, v, true), P.Arg(_, P.Left P.Stop)
        | Lit.Equation (v, _, true), P.Arg(_, P.Right P.Stop) ->
          (* are we in the specific, but no that rare, case where we
             rewrite s=t using s=t (into a tautology t=t)? *)
          let v' = S.FO.apply renaming subst (v, sc_p) in
          if T.equal t' v'
          then raise (ExitSuperposition "will yield a tautology");
        | _ -> ()
      end;
      let passive_lit' =
        Lit.apply_subst_no_simp renaming subst (info.passive_lit, sc_p)
      in
      let new_trail = C.trail_l [info.active; info.passive] in
      if Env.is_trivial_trail new_trail then raise (ExitSuperposition "trivial trail");
      let s' = S.FO.apply renaming subst (info.s, sc_a) in
      if (
        O.compare ord s' t' = Comp.Lt ||
        not (Lit.Pos.is_max_term ~ord passive_lit' passive_lit_pos) ||
        not (BV.get (C.eligible_res (info.passive, sc_p) subst) passive_idx) ||
        not (C.is_eligible_param (info.active, sc_a) subst ~idx:active_idx)
      ) then raise (ExitSuperposition "bad ordering conditions");
      (* Check for superposition at a variable *)
      if not !_sup_at_vars then
        assert (not (T.is_var info.u_p))
      else if T.is_var info.u_p && not (sup_at_var_condition info info.u_p info.t) then
        raise (ExitSuperposition "superposition at variable");
      (* Check for hidden superposition at a variable *)
      match is_hidden_sup_at_var info with
        | Some (var,replacement) when not (sup_at_var_condition info var replacement)
          -> raise (ExitSuperposition "hidden superposition at variable")
        | _ -> ();
          (* ordering constraints are ok, build new active lits (excepted s=t) *)
          let lits_a = CCArray.except_idx (C.lits info.active) active_idx in
          let lits_a = Lit.apply_subst_list renaming subst (lits_a, sc_a) in
          (* build passive literals and replace u|p\sigma with t\sigma *)
          let u' = S.FO.apply renaming subst (info.u_p, sc_p) in
          assert (Type.equal (T.ty u') (T.ty t'));
          let lits_p = Array.to_list (C.lits info.passive) in
          let lits_p = Lit.apply_subst_list renaming subst (lits_p, sc_p) in
          (* assert (T.equal (Lits.Pos.at (Array.of_list lits_p) info.passive_pos) u'); *)
          let lits_p = List.map (Lit.map (fun t-> T.replace t ~old:u' ~by:t')) lits_p in
          let c_guard = Literal.of_unif_subst renaming us in
          let tags = Unif_subst.tags us in
          (* build clause *)
          let new_lits = c_guard @ lits_a @ lits_p in
          let rule =
            let name = if Lit.sign passive_lit' then "s_sup+" else "s_sup-" in
            Proof.Rule.mk name
          in
          let proof =
            Proof.Step.inference ~rule ~tags
              [C.proof_parent_subst renaming (info.active,sc_a) subst;
               C.proof_parent_subst renaming (info.passive,sc_p) subst]
          and penalty =
            C.penalty info.active
            + C.penalty info.passive
            + (if T.is_var s' then 2 else 0) (* superposition from var = bad *)
          in
          let new_clause = C.create ~trail:new_trail ~penalty new_lits proof in
          Util.debugf ~section 3 "@[... ok, conclusion@ @[%a@]@]" (fun k->k C.pp new_clause);
          new_clause :: acc
    with ExitSuperposition reason ->
      Util.debugf ~section 3 "@[... cancel, %s@]" (fun k->k reason);
      acc

  (* choose between regular and simultaneous superposition *)
  let do_superposition info acc=
    let open SupInfo in
    assert (Type.equal (T.ty info.s) (T.ty info.t));
    assert (Unif.Ty.equal ~subst:(US.subst info.subst)
        (T.ty info.s, info.scope_active) (T.ty info.u_p, info.scope_passive));
    if !_use_simultaneous_sup
    then do_simultaneous_superposition info acc
    else do_classic_superposition info acc

  let infer_active clause =
    Util.enter_prof prof_infer_active;
    (* no literal can be eligible for paramodulation if some are selected.
       This checks if inferences with i-th literal are needed? *)
    let eligible = C.Eligible.param clause in
    (* do the inferences where clause is active; for this,
       we try to rewrite conditionally other clauses using
       non-minimal sides of every positive literal *)
    let new_clauses =
      Lits.fold_eqn ~sign:true ~ord:(Ctx.ord ())
        ~both:true ~eligible (C.lits clause)
      |> Sequence.fold
        (fun acc (s, t, _, s_pos) ->
           (* rewrite clauses using s *)
           I.retrieve_unifiables (!_idx_sup_into, 1) (s, 0)
           |> Sequence.filter (fun (u_p,_,_) -> T.DB.is_closed u_p)
           |> Sequence.fold
             (fun acc (u_p, with_pos, subst) ->
                (* rewrite u_p with s *)
                let passive = with_pos.C.WithPos.clause in
                let passive_pos = with_pos.C.WithPos.pos in
                let passive_lit, _ = Lits.Pos.lit_at (C.lits passive) passive_pos in
                let info = SupInfo.( {
                    s; t; active=clause; active_pos=s_pos; scope_active=0;
                    u_p; passive; passive_lit; passive_pos; scope_passive=1; subst;
                  }) in
                do_superposition info acc)
             acc)
        []
    in
    Util.exit_prof prof_infer_active;
    new_clauses

  let infer_passive clause =
    Util.enter_prof prof_infer_passive;
    (* perform inference on this lit? *)
    let eligible = C.Eligible.(res clause) in
    (* do the inferences in which clause is passive (rewritten),
       so we consider both negative and positive literals *)
    let new_clauses =
      Lits.fold_terms ~vars:!_sup_at_vars ~subterms:true ~ord:(Ctx.ord ())
        ~which:`Max ~eligible (C.lits clause)
      |> Sequence.filter (fun (u_p, _) -> not (T.is_var u_p) || T.is_ho_var u_p)
      (* TODO: could exclude more variables from the index:
         they are not needed if they occur with the same args everywhere in the clause *)
      |> Sequence.filter (fun (u_p, _) -> T.DB.is_closed u_p)
      |> Sequence.fold
        (fun acc (u_p, passive_pos) ->
           let passive_lit, _ = Lits.Pos.lit_at (C.lits clause) passive_pos in
           (* all terms that occur in an equation in the active_set
              and that are potentially unifiable with u_p (u at position p) *)
           I.retrieve_unifiables (!_idx_sup_from, 1) (u_p,0)
           |> Sequence.fold
             (fun acc (_, with_pos, subst) ->
                let active = with_pos.C.WithPos.clause in
                let s_pos = with_pos.C.WithPos.pos in
                match Lits.View.get_eqn (C.lits active) s_pos with
                  | Some (s, t, true) ->
                    let info = SupInfo.({
                        s; t; active; active_pos=s_pos; scope_active=1; subst;
                        u_p; passive=clause; passive_lit; passive_pos; scope_passive=0;
                      }) in
                    do_superposition info acc
                  | _ -> acc)
             acc)
        []
    in
    Util.exit_prof prof_infer_passive;
    new_clauses

  let infer_equality_resolution clause =
    Util.enter_prof prof_infer_equality_resolution;
    let eligible = C.Eligible.always in
    (* iterate on those literals *)
    let new_clauses =
      Lits.fold_eqn ~sign:false ~ord:(Ctx.ord ())
        ~both:false ~eligible (C.lits clause)
      |> Sequence.filter_map
        (fun (l, r, _, l_pos) ->
           let pos = Lits.Pos.idx l_pos in
           try
             let us = Unif.FO.unify_full (l, 0) (r, 0) in
             if BV.get (C.eligible_res_no_subst clause) pos
             (* subst(lit) is maximal, we can do the inference *)
             then (
               Util.incr_stat stat_equality_resolution_call;
               let renaming = Subst.Renaming.create () in
               let subst = US.subst us in
               let rule = Proof.Rule.mk "eq_res" in
               let new_lits = CCArray.except_idx (C.lits clause) pos in
               let new_lits = Lit.apply_subst_list renaming subst (new_lits,0) in
               let c_guard = Literal.of_unif_subst renaming us in
               let tags = Unif_subst.tags us in
               let trail = C.trail clause and penalty = C.penalty clause in
               let proof = Proof.Step.inference ~rule ~tags
                   [C.proof_parent_subst renaming (clause,0) subst] in
               let new_clause = C.create ~trail ~penalty (c_guard@new_lits) proof in
               Util.debugf ~section 3 "@[<hv2>equality resolution on@ @[%a@]@ yields @[%a@]@]"
                 (fun k->k C.pp clause C.pp new_clause);
               Some new_clause
             ) else None
           with Unif.Fail ->
             (* l and r not unifiable, try next *)
             None)
      |> Sequence.to_rev_list
    in
    Util.exit_prof prof_infer_equality_resolution;
    new_clauses

  module EqFactInfo = struct
    type t = {
      clause : C.t;
      active_idx : int;
      s : T.t;
      t : T.t;
      u : T.t;
      v : T.t;
      subst : US.t;
      scope : int;
    }
  end

  (* do the inference between given positions, if ordering conditions are respected *)
  let do_eq_factoring info acc =
    let open EqFactInfo in
    let ord = Ctx.ord () in
    let s = info.s and t = info.t and v = info.v in
    let us = info.subst in
    (* check whether subst(lit) is maximal, and not (subst(s) < subst(t)) *)
    let renaming = S.Renaming.create () in
    let subst = US.subst us in
    if O.compare ord (S.FO.apply renaming subst (s, info.scope))
        (S.FO.apply renaming subst (t, info.scope)) <> Comp.Lt
       &&
       C.is_eligible_param (info.clause,info.scope) subst ~idx:info.active_idx
    then (
      Util.incr_stat stat_equality_factoring_call;
      let tags = Unif_subst.tags us in
      let proof =
        Proof.Step.inference
          ~rule:(Proof.Rule.mk"eq_fact") ~tags
          [C.proof_parent_subst renaming (info.clause,0) subst]
      (* new_lits: literals of the new clause. remove active literal
         and replace it by a t!=v one, and apply subst *)
      and new_lits = CCArray.except_idx (C.lits info.clause) info.active_idx in
      let new_lits = Lit.apply_subst_list renaming subst (new_lits,info.scope) in
      let c_guard = Literal.of_unif_subst renaming us in
      let lit' = Lit.mk_neq
          (S.FO.apply renaming subst (t, info.scope))
          (S.FO.apply renaming subst (v, info.scope))
      in
      let new_lits = lit' :: c_guard @ new_lits in
      let new_clause =
        C.create ~trail:(C.trail info.clause) ~penalty:(C.penalty info.clause)
          new_lits proof
      in
      Util.debugf ~section 3 "@[<hv2>equality factoring on@ @[%a@]@ yields @[%a@]@]"
        (fun k->k C.pp info.clause C.pp new_clause);
      new_clause :: acc
    ) else
      acc

  let infer_equality_factoring clause =
    Util.enter_prof prof_infer_equality_factoring;
    let eligible = C.Eligible.(filter Lit.is_pos) in
    (* find root terms that are unifiable with s and are not in the
       literal at s_pos. Calls [k] with a position and substitution *)
    let find_unifiable_lits idx s _s_pos k =
      Array.iteri
        (fun i lit ->
           match lit with
             | _ when i = idx -> () (* same index *)
             | Lit.Prop (p, true) ->
               (* positive proposition *)
               begin try
                   let subst = Unif.FO.unify_full (s,0) (p,0) in
                   k (p, T.true_, subst)
                 with Unif.Fail -> ()
               end
             | Lit.Equation (u, v, true) ->
               (* positive equation *)
               begin try
                   let subst = Unif.FO.unify_full (s,0) (u,0) in
                   k (u, v, subst)
                 with Unif.Fail -> ()
               end;
               begin try
                   let subst = Unif.FO.unify_full (s,0) (v,0) in
                   k (v, u, subst)
                 with Unif.Fail -> ()
               end;
             | _ -> () (* ignore other literals *)
        ) (C.lits clause)
    in
    (* try to do inferences with each positive literal *)
    let new_clauses =
      Lits.fold_eqn ~sign:true ~ord:(Ctx.ord ())
        ~both:true ~eligible (C.lits clause)
      |> Sequence.fold
        (fun acc (s, t, _, s_pos) -> (* try with s=t *)
           let active_idx = Lits.Pos.idx s_pos in
           find_unifiable_lits active_idx s s_pos
           |> Sequence.fold
             (fun acc (u,v,subst) ->
                let info = EqFactInfo.({
                    clause; s; t; u; v; active_idx; subst; scope=0;
                  }) in
                do_eq_factoring info acc)
             acc)
        []
    in
    Util.exit_prof prof_infer_equality_factoring;
    new_clauses

  (* ----------------------------------------------------------------------
   * simplifications
   * ---------------------------------------------------------------------- *)

  (* TODO: put forward pointers in simpl_set, to make some rewriting steps
      faster? (invalidate when updated, also allows to reclaim memory) *)

  (* TODO: use a record with
     - head
     - args
     - subst
     so as not to rebuild intermediate terms, and also to avoid mixing
     the head normal form and the substitution for (evaluated) arguments.

     Might even convert rules into De Bruijn, because:
       - special restriction (vars rhs ⊆ vars lhs)
       - indexing on first symbol might be sufficient if matching is fast
       - must rewrite matching to work on the record anyway
  *)

  let lazy_false = Lazy.from_val false

  type demod_state = {
    mutable demod_clauses: (C.t * Subst.t * Scoped.scope) list; (* rules used *)
    mutable demod_sc: Scoped.scope; (* current scope *)
  }

  (** Compute normal form of term w.r.t active set. Clauses used to
      rewrite are added to the clauses hashset.
      restrict is an option for restricting demodulation in positive maximal terms *)
  let demod_nf ?(restrict=lazy_false) (st:demod_state) c t : T.t =
    let ord = Ctx.ord () in
    (* compute normal form of subterm. If restrict is true, substitutions that
       are variable renamings are forbidden (since we are at root of a max term) *)
    let rec reduce_at_root ~restrict t k =
      (* find equations l=r that match subterm *)
      let cur_sc = st.demod_sc in
      assert (cur_sc > 0);
      let step =
        UnitIdx.retrieve ~sign:true (!_idx_simpl, cur_sc) (t, 0)
        |> Sequence.find_map
          (fun (l, r, (_,_,sign,unit_clause), subst) ->
             (* r is the term subterm is going to be rewritten into *)
             assert (C.is_unit_clause unit_clause);
             if sign &&
                (not (Lazy.force restrict) || not (S.is_renaming subst)) &&
                C.trail_subsumes unit_clause c &&
                (O.compare ord
                   (S.FO.apply Subst.Renaming.none subst (l,cur_sc))
                   (S.FO.apply Subst.Renaming.none subst (r,cur_sc)) = Comp.Gt)
                (* subst(l) > subst(r) and restriction does not apply, we can rewrite *)
             then (
               Util.debugf ~section 5
                 "@[<hv2>demod:@ @[<hv>t=%a[%d],@ l=%a[%d],@ r=%a[%d]@],@ subst=@[%a@]@]"
                 (fun k->k T.pp t 0 T.pp l cur_sc T.pp r cur_sc S.pp subst);
               (* sanity checks *)
               assert (Type.equal (T.ty l) (T.ty r));
               assert (Unif.FO.equal ~subst (l,cur_sc) (t,0));
               st.demod_clauses <-
                 (unit_clause,subst,cur_sc) :: st.demod_clauses;
               st.demod_sc <- 1 + st.demod_sc; (* allocate new scope *)
               Util.incr_stat stat_demodulate_step;
               Some (r, subst, cur_sc)
             ) else None)
      in
      begin match step with
        | None -> k t (* not found any match, normal form found *)
        | Some (rhs,subst,cur_sc) ->
          (* reduce [rhs] in current scope [cur_sc] *)
          assert (cur_sc < st.demod_sc);
          Util.debugf ~section 5
            "@[<2>demod:@ rewrite `@[%a@]`@ into `@[%a@]`@ using %a[%d]@]"
            (fun k->k T.pp t T.pp rhs Subst.pp subst cur_sc);
          (* NOTE: we retraverse the term several times, but this is simpler *)
          let rhs = Subst.FO.apply Subst.Renaming.none subst (rhs,cur_sc) in
          normal_form ~restrict rhs k (* done one rewriting step, continue *)
      end
    (* rewrite innermost-leftmost of [subst(t,scope)]. The initial scope is
       0, but then we normal_form terms in which variables are really the variables
       of the RHS of a previously applied rule (in context !sc); all those
       variables are bound to terms in context 0 *)
    and normal_form ~restrict t k =
      match T.view t with
        | T.Const _ -> reduce_at_root ~restrict t k
        | T.App (hd, l) ->
          (* rewrite subterms in call by value.
             Note that we keep restrictions for the head, so as
             not to rewrite [f x=g x] into ⊤ after equality completion
             of [f=g] *)
          normal_form ~restrict hd
            (fun hd' ->
               normal_form_l l
                 (fun l' ->
                    let t' =
                      if T.equal hd hd' && T.same_l l l'
                      then t
                      else T.app hd' l'
                    in
                    (* rewrite term at root *)
                    reduce_at_root ~restrict t' k))
        | T.Fun (ty_arg, body) ->
          (* reduce under lambdas *)
          normal_form ~restrict:lazy_false body
            (fun body' ->
               let u = if T.equal body body' then t else T.fun_ ty_arg body' in
               k u)
        | T.Var _ | T.DB _ -> k t
        | T.AppBuiltin (b, l) ->
          normal_form_l l
            (fun l' ->
               let u =
                 if T.same_l l l' then t else T.app_builtin ~ty:(T.ty t) b l'
               in
               k u)
    and normal_form_l l k = match l with
      | [] -> k []
      | t :: tail ->
        normal_form ~restrict:lazy_false t
          (fun t' ->
             normal_form_l tail
               (fun l' -> k (t' :: l')))
    in
    normal_form ~restrict t (fun t->t)

  let[@inline] eq_c_subst (c1,s1,sc1)(c2,s2,sc2) =
    C.equal c1 c2 && sc1=sc2 && Subst.equal s1 s2

  (* Demodulate the clause, with restrictions on which terms to rewrite *)
  let demodulate_ c =
    Util.incr_stat stat_demodulate_call;
    let ord = Ctx.ord () in
    (* state for storing proofs and scope *)
    let st = {
      demod_clauses=[];
      demod_sc=1;
    } in
    (* literals that are eligible for paramodulation. *)
    let eligible_param = lazy (C.eligible_param (c,0) S.empty) in
    (* demodulate literals *)
    let demod_lit i lit =
      (* strictly maximal terms might be blocked *)
      let strictly_max = lazy (
        begin match lit with
          | Lit.Equation (t1,t2,true) ->
            begin match O.compare ord t1 t2 with
              | Comp.Gt -> [t1] | Comp.Lt -> [t2] | _ -> []
            end
          | Lit.Prop (t,true) -> [t]
          | _ -> []
        end
      ) in
      (* shall we restrict a subterm? only for max terms in positive
          equations that are eligible for paramodulation.

         NOTE: E's paper mentions that restrictions should occur for
         literals eligible for {b resolution}, not paramodulation, but
         it seems it might be a typo
      *)
      let restrict_term t = lazy (
        Lit.is_pos lit &&
        BV.get (Lazy.force eligible_param) i &&
        (* restrict max terms in positive literals eligible for resolution *)
        CCList.mem ~eq:T.equal t (Lazy.force strictly_max)
      ) in
      Lit.map_no_simp
        (fun t -> demod_nf ~restrict:(restrict_term t) st c t)
        lit
    in
    (* demodulate every literal *)
    let lits = Array.mapi demod_lit (C.lits c) in
    if CCList.is_empty st.demod_clauses then (
      (* no rewriting performed *)
      SimplM.return_same c
    ) else (
      assert (not (Lits.equal_com lits (C.lits c)));
      (* construct new clause *)
      st.demod_clauses <- CCList.uniq ~eq:eq_c_subst st.demod_clauses;
      let proof =
        Proof.Step.simp
          ~rule:(Proof.Rule.mk "demod")
          (C.proof_parent c ::
             List.rev_map
               (fun (c,subst,sc) ->
                  C.proof_parent_subst Subst.Renaming.none (c,sc) subst)
               st.demod_clauses) in
      let trail = C.trail c in (* we know that demodulating rules have smaller trail *)
      let new_c = C.create_a ~trail ~penalty:(C.penalty c) lits proof in
      Util.debugf ~section 3 "@[<hv2>demodulate@ @[%a@]@ into @[%a@]@ using {@[<hv>%a@]}@]"
        (fun k->
           let pp_c_s out (c,s,sc) =
             Format.fprintf out "(@[%a@ :subst %a[%d]@])" C.pp c Subst.pp s sc in
           k C.pp c C.pp new_c (Util.pp_list pp_c_s) st.demod_clauses);
      (* return simplified clause *)
      SimplM.return_new new_c
    )

  let demodulate c = Util.with_prof prof_demodulate demodulate_ c

  (** Find clauses that [given] may demodulate, add them to set *)
  let backward_demodulate set given =
    Util.enter_prof prof_back_demodulate;
    let ord = Ctx.ord () in
    let renaming = Subst.Renaming.create () in
    (* find clauses that might be rewritten by l -> r *)
    let recurse ~oriented set l r =
      I.retrieve_specializations (!_idx_back_demod,1) (l,0)
      |> Sequence.fold
        (fun set (_t',with_pos,subst) ->
           let c = with_pos.C.WithPos.clause in
           (* subst(l) matches t' and is > subst(r), very likely to rewrite! *)
           if ((oriented ||
                O.compare ord
                  (S.FO.apply renaming subst (l,0))
                  (S.FO.apply renaming subst (r,0)) = Comp.Gt
           ) && C.trail_subsumes c given
           )
           then  (* add the clause to the set, it may be rewritten by l -> r *)
             C.ClauseSet.add c set
           else set)
        set
    in
    let set' = match C.lits given with
      | [|Lit.Equation (l,r,true) |] ->
        begin match Ordering.compare ord l r with
          | Comp.Gt -> recurse ~oriented:true set l r
          | Comp.Lt -> recurse ~oriented:true set r l
          | _ ->
            let set' = recurse ~oriented:false set l r in
            recurse ~oriented:false set' r l
            (* both sides can rewrite, but we need to check ordering *)
        end
      | _ -> set
    in
    Util.exit_prof prof_back_demodulate;
    set'

  let is_tautology c =
    let is_tauto = Lits.is_trivial (C.lits c) || Trail.is_trivial (C.trail c) in
    if is_tauto then Util.debugf ~section 3 "@[@[%a@]@ is a tautology@]" (fun k->k C.pp c);
    is_tauto

  (* semantic tautology deletion, using a congruence closure algorithm
     to see if negative literals imply some positive literal *)
  let is_semantic_tautology_real (c:C.t) : bool =
    (* create the congruence closure of all negative equations of [c] *)
    let cc = Congruence.FO.create ~size:8 () in
    let cc =
      Array.fold_left
        (fun cc lit -> match lit with
           | Lit.Equation (l, r, false) ->
             Congruence.FO.mk_eq cc l r
           | Lit.Prop (p, false) ->
             Congruence.FO.mk_eq cc p T.true_
           | _ -> cc)
        cc (C.lits c)
    in
    let res = CCArray.exists
        (function
          | Lit.Equation (l, r, true) ->
            (* if l=r is implied by the congruence, then the clause is redundant *)
            Congruence.FO.is_eq cc l r
          | Lit.Prop (p, true) ->
            Congruence.FO.is_eq cc p T.true_
          | _ -> false)
        (C.lits c)
    in
    if res then (
      Util.incr_stat stat_semantic_tautology;
      Util.debugf ~section 2 "@[@[%a@]@ is a semantic tautology@]" (fun k->k C.pp c);
    );
    res

  let is_semantic_tautology_ c =
    if Array.length (C.lits c) >= 2 &&
       CCArray.exists Lit.is_neg (C.lits c) &&
       CCArray.exists Lit.is_pos (C.lits c)
    then is_semantic_tautology_real c
    else false

  let is_semantic_tautology c =
    Util.with_prof prof_semantic_tautology is_semantic_tautology_ c

  let var_in_subst_ us v sc =
    S.mem (US.subst us) ((v:T.var:>InnerTerm.t HVar.t),sc)

  let basic_simplify c =
    if C.get_flag flag_simplified c
    then SimplM.return_same c
    else (
      Util.enter_prof prof_basic_simplify;
      Util.incr_stat stat_basic_simplify_calls;
      let lits = C.lits c in
      let has_changed = ref false in
      let tags = ref [] in
      (* bv: literals to keep *)
      let bv = BV.create ~size:(Array.length lits) true in
      (* eliminate absurd lits *)
      Array.iteri
        (fun i lit ->
           if Lit.is_absurd lit then (
             has_changed := true;
             tags := Lit.is_absurd_tags lit @ !tags;
             BV.reset bv i
           ))
        lits;
      (* eliminate inequations x != t *)
      let us = ref US.empty in
      let try_unif i t1 sc1 t2 sc2 =
        try
          let subst' = Unif.FO.unify_full ~subst:!us (t1,sc1) (t2,sc2) in
          has_changed := true;
          BV.reset bv i;
          us := subst';
        with Unif.Fail -> ()
      in
      Array.iteri
        (fun i lit ->
           let can_destr_eq_var v =
             not (var_in_subst_ !us v 0) && not (Type.is_fun (HVar.ty v))
           in
           if BV.get bv i then match lit with
             | Lit.Equation (l, r, false) ->
               begin match T.view l, T.view r with
                 | T.Var v, _ when can_destr_eq_var v ->
                   (* eligible for destructive Equality Resolution, try to update
                       [subst]. Careful: in the case [X!=a | X!=b | C] we must
                       bind X only to [a] or [b], not unify [a] with [b].

                      NOTE: this also works for HO constraints for unshielded vars *)
                   try_unif i l 0 r 0
                 | _, T.Var v when can_destr_eq_var v ->
                   try_unif i r 0 l 0
                 | _ -> ()
               end
             | Lit.Equation (l, r, true) when Type.is_prop (T.ty l) ->
               begin match T.view l, T.view r with
                 | ( T.AppBuiltin (Builtin.True, []), T.Var x
                   | T.Var x, T.AppBuiltin (Builtin.True, []))
                   when not (var_in_subst_ !us x 0) ->
                   (* [C or x=true ---> C[x:=false]] *)
                   begin
                     try
                       let subst' = US.FO.bind !us (x,0) (T.false_,0) in
                       has_changed := true;
                       BV.reset bv i;
                       us := subst';
                     with Unif.Fail -> ()
                   end

                 | _ -> ()
               end
             | _ -> ())
        lits;
      let new_lits = BV.select bv lits in
      let new_lits =
        if US.is_empty !us then new_lits
        else (
          assert !has_changed;
          let subst = US.subst !us in
          let tgs = US.tags !us in
          tags := tgs @ !tags;
          let c_guard = Literal.of_unif_subst Subst.Renaming.none !us in
          c_guard @ Lit.apply_subst_list Subst.Renaming.none subst (new_lits,0)
        )
      in
      let new_lits = CCList.uniq ~eq:Lit.equal_com new_lits in
      if not !has_changed && List.length new_lits = Array.length lits then (
        Util.exit_prof prof_basic_simplify;
        C.set_flag flag_simplified c true;
        SimplM.return_same c  (* no simplification *)
      ) else (
        let parent =
          if Subst.is_empty (US.subst !us) then C.proof_parent c
          else C.proof_parent_subst Subst.Renaming.none (c,0) (US.subst !us)
        in
        let proof =
          Proof.Step.simp [parent]
            ~tags:!tags ~rule:(Proof.Rule.mk "simplify") in
        let new_clause =
          C.create ~trail:(C.trail c) ~penalty:(C.penalty c) new_lits proof
        in
        Util.debugf ~section 3
          "@[<>@[%a@]@ @[<2>basic_simplifies into@ @[%a@]@]@ with @[%a@]@]"
          (fun k->k C.pp c C.pp new_clause US.pp !us);
        Util.incr_stat stat_basic_simplify;
        Util.exit_prof prof_basic_simplify;
        SimplM.return_new new_clause
      )
    )

  let handle_distinct_constants lit =
    match lit with
      | Lit.Equation (l, r, sign) when T.is_const l && T.is_const r ->
        let s1 = T.head_exn l and s2 = T.head_exn r in
        if ID.is_distinct_object s1 && ID.is_distinct_object s2
        then
          if sign = (ID.equal s1 s2)
          then Some (Lit.mk_tauto,[],[Proof.Tag.T_distinct])  (* "a" = "a", or "a" != "b" *)
          else Some (Lit.mk_absurd,[],[Proof.Tag.T_distinct]) (* "a" = "b" or "a" != "a" *)
        else None
      | _ -> None

  exception FoundMatch of T.t * C.t * S.t

  let positive_simplify_reflect c =
    Util.enter_prof prof_pos_simplify_reflect;
    (* iterate through literals and try to resolve negative ones *)
    let rec iterate_lits acc lits clauses = match lits with
      | [] -> List.rev acc, clauses
      | (Lit.Equation (s, t, false) as lit)::lits' ->
        begin match equatable_terms clauses s t with
          | None -> (* keep literal *)
            iterate_lits (lit::acc) lits' clauses
          | Some new_clauses -> (* drop literal, remember clauses *)
            iterate_lits acc lits' new_clauses
        end
      | lit::lits' -> iterate_lits (lit::acc) lits' clauses
    (** try to make the terms equal using some positive unit clauses
        from active_set *)
    and equatable_terms clauses t1 t2 =
      match T.Classic.view t1, T.Classic.view t2 with
        | _ when T.equal t1 t2 -> Some clauses  (* trivial *)
        | T.Classic.App (f, ss), T.Classic.App (g, ts)
          when ID.equal f g && List.length ss = List.length ts ->
          (* try to make the terms equal directly *)
          begin match equate_root clauses t1 t2 with
            | None -> (* otherwise try to make subterms pairwise equal *)
              let ok, clauses = List.fold_left2
                  (fun (ok, clauses) t1' t2' ->
                     if ok
                     then match equatable_terms clauses t1' t2' with
                       | None -> false, []
                       | Some clauses -> true, clauses
                     else false, [])
                  (true, clauses) ss ts
              in
              if ok then Some clauses else None
            | Some clauses -> Some clauses
          end
        | _ -> equate_root clauses t1 t2 (* try to solve it with a unit equality *)
    (** try to equate terms with a positive unit clause that match them *)
    and equate_root clauses t1 t2 =
      try
        UnitIdx.retrieve ~sign:true (!_idx_simpl,1)(t1,0)
        |> Sequence.iter
          (fun (l,r,(_,_,_,c'),subst) ->
             assert (Unif.FO.equal ~subst (l,1)(t1,0));
             if Unif.FO.equal ~subst (r,1)(t2,0)
             && C.trail_subsumes c' c
             then begin
               (* t1!=t2 is refuted by l\sigma = r\sigma *)
               Util.debugf ~section 4
                 "@[<2>equate @[%a@]@ and @[%a@]@ using @[%a@]@]"
                 (fun k->k T.pp t1 T.pp t2 C.pp c');
               raise (FoundMatch (r, c', subst)) (* success *)
             end
          );
        None (* no match *)
      with FoundMatch (_r, c', subst) ->
        Some (C.proof_parent_subst Subst.Renaming.none (c',1) subst :: clauses)  (* success *)
    in
    (* fold over literals *)
    let lits, premises = iterate_lits [] (C.lits c |> Array.to_list) [] in
    if List.length lits = Array.length (C.lits c)
    then (
      (* no literal removed, keep c *)
      Util.exit_prof prof_pos_simplify_reflect;
      SimplM.return_same c
    ) else (
      let proof =
        Proof.Step.simp ~rule:(Proof.Rule.mk "simplify_reflect+")
          (C.proof_parent c::premises) in
      let trail = C.trail c and penalty = C.penalty c in
      let new_c = C.create ~trail ~penalty lits proof in
      Util.debugf ~section 3 "@[@[%a@]@ pos_simplify_reflect into @[%a@]@]"
        (fun k->k C.pp c C.pp new_c);
      Util.exit_prof prof_pos_simplify_reflect;
      SimplM.return_new new_c
    )

  let negative_simplify_reflect c =
    Util.enter_prof prof_neg_simplify_reflect;
    (* iterate through literals and try to resolve positive ones *)
    let rec iterate_lits acc lits clauses = match lits with
      | [] -> List.rev acc, clauses
      | (Lit.Equation (s, t, true) as lit)::lits' ->
        begin match can_refute s t, can_refute t s with
          | None, None -> (* keep literal *)
            iterate_lits (lit::acc) lits' clauses
          | Some new_clause, _ | _, Some new_clause -> (* drop literal, remember clause *)
            iterate_lits acc lits' (new_clause :: clauses)
        end
      | lit::lits' -> iterate_lits (lit::acc) lits' clauses
    (** try to remove the literal using a negative unit clause *)
    and can_refute s t =
      try
        UnitIdx.retrieve ~sign:false (!_idx_simpl,1) (s,0)
        |> Sequence.iter
          (fun (l, r, (_,_,_,c'), subst) ->
             assert (Unif.FO.equal ~subst (l, 1) (s, 0));
             if Unif.FO.equal ~subst (r, 1) (t, 0)
             && C.trail_subsumes c' c
             then begin
               (* TODO: useless? *)
               let subst = Unif.FO.matching ~subst ~pattern:(r, 1) (t, 0) in
               Util.debugf ~section 3 "@[neg_reflect eliminates@ @[%a=%a@]@ with @[%a@]@]"
                 (fun k->k T.pp s T.pp t C.pp c');
               raise (FoundMatch (r, c', subst)) (* success *)
             end
          );
        None (* no match *)
      with FoundMatch (_r, c', subst) ->
        Some (C.proof_parent_subst Subst.Renaming.none (c',1) subst) (* success *)
    in
    (* fold over literals *)
    let lits, premises = iterate_lits [] (C.lits c |> Array.to_list) [] in
    if List.length lits = Array.length (C.lits c)
    then (
      (* no literal removed *)
      Util.exit_prof prof_neg_simplify_reflect;
      SimplM.return_same c
    ) else (
      let proof =
        Proof.Step.simp
          ~rule:(Proof.Rule.mk "simplify_reflect-")
          (C.proof_parent c :: premises) in
      let new_c = C.create ~trail:(C.trail c) ~penalty:(C.penalty c) lits proof in
      Util.debugf ~section 3 "@[@[%a@]@ neg_simplify_reflect into @[%a@]@]"
        (fun k->k C.pp c C.pp new_c);
      Util.exit_prof prof_neg_simplify_reflect;
      SimplM.return_new new_c
    )

  (* ----------------------------------------------------------------------
   * subsumption
   * ---------------------------------------------------------------------- *)

  (** raised when a subsuming substitution is found *)
  exception SubsumptionFound of S.t

  (** check that every literal in a matches at least one literal in b *)
  let all_lits_match a sc_a b sc_b =
    CCArray.for_all
      (fun lita ->
         CCArray.exists
           (fun litb ->
              not (Sequence.is_empty (Lit.subsumes (lita, sc_a) (litb, sc_b))))
           b)
      a

  (** Compare literals by subsumption difficulty
      (see "towards efficient subsumption", Tammet).
      We sort by increasing order, so non-ground, deep, heavy literals are
      smaller (thus tested early) *)
  let compare_literals_subsumption lita litb =
    CCOrd.(
      (* ground literal is bigger *)
      bool (Lit.is_ground lita) (Lit.is_ground litb)
      (* deep literal is smaller *)
      <?> (map Lit.depth (opp int), lita, litb)
      (* heavy literal is smaller *)
      <?> (map Lit.weight (opp int), lita, litb)
    )

  (* replace the bitvector system by some backtracking scheme?
     XXX: maybe not a good idea. the algorithm is actually quite subtle
     and needs tight control over the traversal (lookahead of free
     variables in next literals, see [check_vars]...) *)

  (** Check whether [a] subsumes [b], and if it does, return the
      corresponding substitution *)
  let subsumes_with_ (a,sc_a) (b,sc_b) : _ option =
    (* a must not have more literals, and it must be possible to bind
        all its vars during subsumption *)
    if Array.length a > Array.length b
    || not (all_lits_match a sc_a b sc_b)
    then None
    else (
      (* sort a copy of [a] by decreasing difficulty *)
      let a = Array.copy a in
      let tags = ref [] in
      (* try to subsumes literals of b whose index are not in bv, with [subst] *)
      let rec try_permutations i subst bv =
        if i = Array.length a
        then raise (SubsumptionFound subst)
        else
          let lita = a.(i) in
          find_matched lita i subst bv 0
      (* find literals of b that are not bv and that are matched by lita *)
      and find_matched lita i subst bv j =
        if j = Array.length b then ()
        (* if litb is already matched, continue *)
        else if BV.get bv j then find_matched lita i subst bv (j+1)
        else (
          let litb = b.(j) in
          BV.set bv j;
          (* match lita and litb, then flag litb as used, and try with next literal of a *)
          let n_subst = ref 0 in
          Lit.subsumes ~subst (lita, sc_a) (litb, sc_b)
            (fun (subst',tgs) ->
               incr n_subst;
               tags := tgs @ !tags;
               try_permutations (i+1) subst' bv);
          BV.reset bv j;
          (* some variable of lita occur in a[j+1...], try another literal of b *)
          if !n_subst > 0 && not (check_vars lita (i+1))
          then () (* no backtracking for litb *)
          else find_matched lita i subst bv (j+1)
        )
      (* does some literal in a[j...] contain a variable in l or r? *)
      and check_vars lit j =
        let vars = Lit.vars lit in
        if vars = []
        then false
        else
          try
            for k = j to Array.length a - 1 do
              if List.exists (fun v -> Lit.var_occurs v a.(k)) vars
              then raise Exit
            done;
            false
          with Exit -> true
      in
      try
        Array.sort compare_literals_subsumption a;
        let bv = BV.empty () in
        try_permutations 0 S.empty bv;
        None
      with (SubsumptionFound subst) ->
        Util.debugf ~section 2 "(@[<hv>subsumes@ :c1 @[%a@]@ :c2 @[%a@]@ :subst %a%a@]"
          (fun k->k Lits.pp a Lits.pp b Subst.pp subst Proof.pp_tags !tags);
        Some (subst, !tags)
    )

  let subsumes_with a b =
    Util.enter_prof prof_subsumption;
    Util.incr_stat stat_subsumption_call;
    let res = subsumes_with_ a b in
    Util.exit_prof prof_subsumption;
    res

  let subsumes a b = match subsumes_with (a,0) (b,1) with
    | None -> false
    | Some _ -> true

  (* anti-unification of the two terms with at most one disagreement point *)
  let anti_unify (t:T.t)(u:T.t): (T.t * T.t) option =
    match Unif.FO.anti_unify ~cut:1 t u with
      | Some [pair] -> Some pair
      | _ -> None

  let eq_subsumes_with (a,sc_a) (b,sc_b) =
    (* subsume a literal using a = b *)
    let rec equate_lit_with a b lit = match lit with
      | Lit.Equation (u, v, true) when not (T.equal u v) -> equate_terms a b u v
      | _ -> None
    (* make u=v using a=b once *)
    and equate_terms a b u v =
      begin match anti_unify u v with
        | None -> None
        | Some (u', v') -> equate_root a b u' v'
      end
    (* check whether a\sigma = u and b\sigma = v, for some sigma;
       or the commutation thereof *)
    and equate_root a b u v: Subst.t option =
      let check_ a b u v =
        try
          let subst = Unif.FO.matching ~pattern:(a, sc_a) (u, sc_b) in
          let subst = Unif.FO.matching ~subst ~pattern:(b, sc_a) (v, sc_b) in
          Some subst
        with Unif.Fail -> None
      in
      begin match check_ a b u v with
        | Some _ as s -> s
        | None -> check_ b a u v
      end
    in
    (* check for each literal *)
    Util.enter_prof prof_eq_subsumption;
    Util.incr_stat stat_eq_subsumption_call;
    let res = match a with
      | [|Lit.Equation (s, t, true)|] ->
        let res = CCArray.find (equate_lit_with s t) b in
        begin match res with
          | None -> None
          | Some subst ->
            Util.debugf ~section 3 "@[<2>@[%a@]@ eq-subsumes @[%a@]@ :subst %a@]"
              (fun k->k Lits.pp a Lits.pp b Subst.pp subst);
            Util.incr_stat stat_eq_subsumption_success;
            Some subst
        end
      | _ -> None (* only a positive unit clause unit-subsumes a clause *)
    in
    Util.exit_prof prof_eq_subsumption;
    res

  let eq_subsumes a b = CCOpt.is_some (eq_subsumes_with (a,1) (b,0))

  let subsumed_by_active_set c =
    Util.enter_prof prof_subsumption_set;
    Util.incr_stat stat_subsumed_by_active_set_call;
    (* if there is an equation in c, try equality subsumption *)
    let try_eq_subsumption = CCArray.exists Lit.is_eqn (C.lits c) in
    (* use feature vector indexing *)
    let res =
      SubsumIdx.retrieve_subsuming_c !_idx_fv c
      |> Sequence.exists
        (fun c' ->
           C.trail_subsumes c' c
           &&
           ( (try_eq_subsumption && eq_subsumes (C.lits c') (C.lits c))
             ||
             subsumes (C.lits c') (C.lits c)
           ))
    in
    Util.exit_prof prof_subsumption_set;
    if res then (
      Util.debugf ~section 3 "@[<2>@[%a@]@ subsumed by active set@]" (fun k->k C.pp c);
      Util.incr_stat stat_clauses_subsumed;
    );
    res

  let subsumed_in_active_set acc c =
    Util.enter_prof prof_subsumption_in_set;
    Util.incr_stat stat_subsumed_in_active_set_call;
    (* if c is a single unit clause *)
    let try_eq_subsumption =
      C.is_unit_clause c && Lit.is_pos (C.lits c).(0)
    in
    (* use feature vector indexing *)
    let res =
      SubsumIdx.retrieve_subsumed_c !_idx_fv c
      |> Sequence.fold
        (fun res c' ->
           if C.trail_subsumes c c'
           then
             let redundant =
               (try_eq_subsumption && eq_subsumes (C.lits c) (C.lits c'))
               || subsumes (C.lits c) (C.lits c')
             in
             if redundant then (
               Util.incr_stat stat_clauses_subsumed;
               C.ClauseSet.add c' res
             ) else res
           else res)
        acc
    in
    Util.exit_prof prof_subsumption_in_set;
    res

  (* Number of equational lits. Used as an estimation for the difficulty of the subsumption
     check for this clause. *)
  let num_equational lits =
    Array.fold_left
      (fun acc lit -> match lit with
         | Lit.Equation _ -> acc+1
         | _ -> acc
      ) 0 lits

  (* ----------------------------------------------------------------------
   * contextual literal cutting
   * ---------------------------------------------------------------------- *)

  (* Performs successive contextual literal cuttings *)
  let rec contextual_literal_cutting_rec c =
    let open SimplM.Infix in
    if Array.length (C.lits c) <= 1
    || num_equational (C.lits c) > 3
    || Array.length (C.lits c) > 8
    then SimplM.return_same c
    else (
      (* do we need to try to use equality subsumption? *)
      let try_eq_subsumption = CCArray.exists Lit.is_eqn (C.lits c) in
      (* try to remove one literal from the literal array *)
      let remove_one_lit lits =
        Sequence.of_array_i lits
        |> Sequence.filter (fun (_,lit) -> not (Lit.is_constraint lit))
        |> Sequence.find_map
          (fun (i,old_lit) ->
             (* negate literal *)
             lits.(i) <- Lit.negate old_lit;
             (* test for subsumption *)
             SubsumIdx.retrieve_subsuming !_idx_fv
               (Lits.Seq.to_form lits) (C.trail c |> Trail.labels)
             |> Sequence.filter (fun c' -> C.trail_subsumes c' c)
             |> Sequence.find_map
               (fun c' ->
                  let subst =
                    match
                      if try_eq_subsumption
                      then eq_subsumes_with (C.lits c',1) (lits,0)
                      else None
                    with
                      | Some s -> Some (s, [])
                      | None -> subsumes_with (C.lits c',1) (lits,0)
                  in
                  subst
                  |> CCOpt.map
                    (fun (subst,tags) ->
                       (* remove the literal and recurse *)
                       CCArray.except_idx lits i, i, c', subst, tags))
             |> CCFun.tap
               (fun _ ->
                  (* restore literal *)
                  lits.(i) <- old_lit))
      in
      begin match remove_one_lit (Array.copy (C.lits c)) with
        | None ->
          SimplM.return_same c (* no literal removed *)
        | Some (new_lits, _, c',subst,tags) ->
          (* hc' allowed us to cut a literal *)
          assert (List.length new_lits + 1 = Array.length (C.lits c));
          let proof =
            Proof.Step.inference
              ~rule:(Proof.Rule.mk "clc") ~tags
              [C.proof_parent c;
               C.proof_parent_subst Subst.Renaming.none (c',1) subst] in
          let new_c = C.create ~trail:(C.trail c) ~penalty:(C.penalty c) new_lits proof in
          Util.debugf ~section 3
            "@[<2>contextual literal cutting@ in @[%a@]@ using @[%a@]@ gives @[%a@]@]"
            (fun k->k C.pp c C.pp c' C.pp new_c);
          Util.incr_stat stat_clc;
          (* try to cut another literal *)
          SimplM.return_new new_c >>= contextual_literal_cutting_rec
      end
    )

  let contextual_literal_cutting c =
    Util.enter_prof prof_clc;
    let res = contextual_literal_cutting_rec c in
    Util.exit_prof prof_clc;
    res

  (* ----------------------------------------------------------------------
   * contraction (condensation)
   * ---------------------------------------------------------------------- *)

  exception CondensedInto of Lit.t array * S.t * Subst.Renaming.t * Proof.tag list

  (** performs condensation on the clause. It looks for two literals l1 and l2 of same
      sign such that l1\sigma = l2, and hc\sigma \ {l2} subsumes hc. Then
      hc is simplified into hc\sigma \ {l2}.
      If there are too many equational literals, the simplification is disabled to
      avoid pathologically expensive subsumption checks.
      TODO remove this limitation after an efficient subsumption check is implemented. *)
  let rec condensation_rec c =
    let open SimplM.Infix in
    if Array.length (C.lits c) <= 1
    || num_equational (C.lits c) > 3
    || Array.length (C.lits c) > 8
    then SimplM.return_same c
    else
      (* scope is used to rename literals for subsumption *)
      let lits = C.lits c in
      let n = Array.length lits in
      try
        for i = 0 to n - 1 do
          let lit = lits.(i) in
          for j = i+1 to n - 1 do
            let lit' = lits.(j) in
            (* see whether [lit |= lit'], and if removing [lit] gives a clause
               that subsumes c. Also try to swap [lit] and [lit']. *)
            let subst_remove_lit =
              Lit.subsumes (lit, 0) (lit', 0)
              |> Sequence.map (fun s -> s, i)
            and subst_remove_lit' =
              Lit.subsumes (lit', 0) (lit, 0)
              |> Sequence.map (fun s -> s, j)
            in
            (* potential condensing substitutions *)
            let substs = Sequence.append subst_remove_lit subst_remove_lit' in
            Sequence.iter
              (fun ((subst,tags),idx_to_remove) ->
                 let new_lits = Array.sub lits 0 (n - 1) in
                 if idx_to_remove <> n-1
                 then new_lits.(idx_to_remove) <- lits.(n-1);  (* remove lit *)
                 let renaming = Subst.Renaming.create () in
                 let new_lits = Lits.apply_subst renaming subst (new_lits,0) in
                 (* check subsumption *)
                 if subsumes new_lits lits then (
                   raise (CondensedInto (new_lits, subst, renaming, tags))
                 ))
              substs
          done;
        done;
        SimplM.return_same c
      with CondensedInto (new_lits, subst, renaming, tags) ->
        (* clause is simplified *)
        let proof =
          Proof.Step.simp
            ~rule:(Proof.Rule.mk "condensation") ~tags
            [C.proof_parent_subst renaming (c,0) subst] in
        let c' = C.create_a ~trail:(C.trail c) ~penalty:(C.penalty c) new_lits proof in
        Util.debugf ~section 3
          "@[<2>condensation@ of @[%a@] (with @[%a@])@ gives @[%a@]@]"
          (fun k->k C.pp c S.pp subst C.pp c');
        (* try to condense further *)
        Util.incr_stat stat_condensation;
        SimplM.return_new c' >>= condensation_rec

  let condensation c =
    Util.with_prof prof_condensation condensation_rec c

  (** {2 Registration} *)

  (* print index into file *)
  let _print_idx ~f file idx =
    CCIO.with_out file
      (fun oc ->
         let out = Format.formatter_of_out_channel oc in
         Format.fprintf out "@[%a@]@." f idx;
         flush oc)

  let setup_dot_printers () =
    let pp_leaf _ _ = () in
    CCOpt.iter
      (fun file ->
         Signal.once Signals.on_dot_output
           (fun () -> _print_idx ~f:(TermIndex.to_dot pp_leaf) file !_idx_sup_into))
      !_dot_sup_into;
    CCOpt.iter
      (fun file ->
         Signal.once Signals.on_dot_output
           (fun () -> _print_idx ~f:(TermIndex.to_dot pp_leaf) file !_idx_sup_from))
      !_dot_sup_from;
    CCOpt.iter
      (fun file ->
         Signal.once Signals.on_dot_output
           (fun () -> _print_idx ~f:UnitIdx.to_dot file !_idx_simpl))
      !_dot_simpl;
    CCOpt.iter
      (fun file ->
         Signal.once Signals.on_dot_output
           (fun () -> _print_idx ~f:(TermIndex.to_dot pp_leaf) file !_idx_back_demod))
      !_dot_demod_into;
    ()

  let register () =
    let open SimplM.Infix in
    let rw_simplify c =
      demodulate c
      >>= basic_simplify
      >>= positive_simplify_reflect
      >>= negative_simplify_reflect
    and active_simplify c =
      condensation c
      >>= contextual_literal_cutting
    and backward_simplify c =
      let set = C.ClauseSet.empty in
      backward_demodulate set c
    and redundant = subsumed_by_active_set
    and backward_redundant = subsumed_in_active_set
    and is_trivial = is_tautology in
    Env.add_binary_inf "superposition_passive" infer_passive;
    Env.add_binary_inf "superposition_active" infer_active;
    Env.add_unary_inf "equality_factoring" infer_equality_factoring;
    Env.add_unary_inf "equality_resolution" infer_equality_resolution;
    if not (!_dont_simplify) then (
      Env.add_rw_simplify rw_simplify;
      Env.add_basic_simplify basic_simplify;
      Env.add_active_simplify active_simplify;
      Env.add_backward_simplify backward_simplify
    );
    Env.add_redundant redundant;
    Env.add_backward_redundant backward_redundant;
    if !_use_semantic_tauto
    then Env.add_is_trivial is_semantic_tautology;
    Env.add_is_trivial is_trivial;
    Env.add_lit_rule "distinct_symbol" handle_distinct_constants;
    setup_dot_printers ();
    ()
end

let key = Flex_state.create_key()

let register ~sup =
  let module Sup = (val sup : S) in
  let module E = Sup.Env in
  E.update_flex_state (Flex_state.add key sup)

(* TODO: move DOT index printing into the extension *)

let extension =
  let action env =
    let module E = (val env : Env.S) in
    let module Sup = Make(E) in
    Sup.register();
    register ~sup:(module Sup : S)
  in
  { Extensions.default with Extensions.
                         name="superposition";
                         env_actions = [action];
  }

let () =
  Params.add_opts
    [ "--semantic-tauto"
    , Arg.Set _use_semantic_tauto
    , " enable semantic tautology check"
    ; "--no-semantic-tauto"
    , Arg.Clear _use_semantic_tauto
    , " disable semantic tautology check"
    ; "--dot-sup-into"
    , Arg.String (fun s -> _dot_sup_into := Some s)
    , " print superposition-into index into file"
    ; "--dot-sup-from"
    , Arg.String (fun s -> _dot_sup_from := Some s)
    , " print superposition-from index into file"
    ; "--dot-demod"
    , Arg.String (fun s -> _dot_simpl := Some s)
    , " print forward rewriting index into file"
    ; "--dot-demod-into"
    , Arg.String (fun s -> _dot_demod_into := Some s)
    , " print backward rewriting index into file"
    ; "--simultaneous-sup"
    , Arg.Bool (fun b -> _use_simultaneous_sup := b)
    , " enable/disable simultaneous superposition"
    ; "--dont-simplify"
    , Arg.Set _dont_simplify
    , " disable simplification rules"
    ; "--sup-at-vars"
    , Arg.Set _sup_at_vars
    , " enable superposition at variables under certain ordering conditions"
    ; "--restrict-hidden-sup-at-vars"
    , Arg.Set _restrict_hidden_sup_at_vars
    , " perform hidden superposition at variables only under certain ordering conditions"
    ]

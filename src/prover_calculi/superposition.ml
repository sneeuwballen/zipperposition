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
module P = Position

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
let stat_ext_dec = Util.mk_stat "sup.ext_dec calls"
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
let prof_ext_dec = Util.mk_profiler "sup.ext_dec"
let prof_infer_fluidsup_active = Util.mk_profiler "sup.infer_fluidsup_active"
let prof_infer_fluidsup_passive = Util.mk_profiler "sup.infer_fluidsup_passive"
let prof_infer_equality_resolution = Util.mk_profiler "sup.infer_equality_resolution"
let prof_infer_equality_factoring = Util.mk_profiler "sup.infer_equality_factoring"

let k_trigger_bool_inst = Flex_state.create_key ()
let k_sup_at_vars = Flex_state.create_key ()
let k_sup_in_var_args = Flex_state.create_key ()
let k_sup_under_lambdas = Flex_state.create_key ()
let k_sup_true_false = Flex_state.create_key ()
let k_sup_at_var_headed = Flex_state.create_key ()
let k_fluidsup = Flex_state.create_key ()
let k_dupsup = Flex_state.create_key ()
let k_lambdasup = Flex_state.create_key ()
let k_sup_w_pure_vars = Flex_state.create_key ()
let k_demod_in_var_args = Flex_state.create_key ()
let k_lambda_demod = Flex_state.create_key ()
let k_ext_dec_lits = Flex_state.create_key ()
let k_max_lits_ext_dec = Flex_state.create_key ()
let k_use_simultaneous_sup = Flex_state.create_key ()
let k_unif_alg = Flex_state.create_key ()
let k_fluidsup_penalty = Flex_state.create_key ()
let k_ground_subs_check = Flex_state.create_key ()
let k_solid_subsumption = Flex_state.create_key ()
let k_dot_sup_into = Flex_state.create_key ()
let k_dot_sup_from = Flex_state.create_key ()
let k_dot_simpl = Flex_state.create_key ()
let k_dot_demod_into = Flex_state.create_key ()
let k_recognize_injectivity = Flex_state.create_key ()
let k_complete_ho_unification = Flex_state.create_key ()
let k_max_infs = Flex_state.create_key ()
let k_switch_stream_extraction = Flex_state.create_key ()
let k_dont_simplify = Flex_state.create_key ()
let k_use_semantic_tauto = Flex_state.create_key ()
let k_restrict_fluidsup = Flex_state.create_key ()



let _NO_LAMSUP = -1


module Make(Env : Env.S) : S with module Env = Env = struct
  module Env = Env
  module Ctx = Env.Ctx
  module C = Env.C
  module PS = Env.ProofState
  module I = PS.TermIndex
  module TermIndex = PS.TermIndex
  module SubsumIdx = PS.SubsumptionIndex
  module UnitIdx = PS.UnitIndex
  module Stm = Stream.Make(struct
      module Ctx = Ctx
      module C = C
    end)
  module StmQ = StreamQueue.Make(Stm)
  module Bools = Booleans.Make(Env)
  module SS = SolidSubsumption.Make(struct let st = Env.flex_state () end)

  (** {6 Stream queue} *)

  type queue = {q : StmQ.t;}

  let _stmq = {q = StmQ.default();}

  (** {6 Index Management} *)

  let _idx_sup_into = ref (TermIndex.empty ())
  let _idx_lambdasup_into = ref (TermIndex.empty ())
  let _idx_fluidsup_into = ref (TermIndex.empty ())
  let _idx_dupsup_into = ref (TermIndex.empty ())
  let _idx_sup_from = ref (TermIndex.empty ())
  let _idx_back_demod = ref (TermIndex.empty ())
  let _idx_fv = ref (SubsumIdx.empty ())
  (* let _idx_fv = ref (SubsumIdx.of_signature (Ctx.signature()) ()) *)

  let _idx_simpl = ref (UnitIdx.empty ())
  let _cls_w_pred_vars = ref (C.ClauseSet.empty)
  let _trigger_bools   = ref (Term.Set.empty)
  let _ext_dec_from_idx = ref (ID.Map.empty)
  let _ext_dec_into_idx = ref (ID.Map.empty)

  let idx_sup_into () = !_idx_sup_into
  let idx_sup_from () = !_idx_sup_from
  let idx_fv () = !_idx_fv

  (* Beta-Eta-Normalizes terms before comparing them. Note that the Clause
     module calls Ctx.ord () independently, but clauses should be normalized
     independently by simplification rules. *) 
  let ord =
    Ctx.ord ()

  let pred_vars c =
    (* instantiate only variables in eligible lits *)
    let eligible = C.Eligible.res c in
    let lits = 
      CCArray.mapi (fun i lit -> (i, lit)) (C.lits c)
      |> CCArray.filter_map (fun (i,lit) -> 
          if eligible i lit then Some lit else None) in
    CCList.to_seq (Literals.vars lits) 
    |> Iter.filter (fun v -> 
        let ty = HVar.ty v in
        Type.is_fun ty && Type.returns_prop ty)
    |> Iter.to_list

  let get_triggers c =
    let trivial_trigger t =
      T.is_const (T.head_term t) ||
      T.is_var (snd @@ T.open_fun t) in

    Literals.fold_terms ~ord ~subterms:true ~eligible:C.Eligible.always 
      ~which:`All (C.lits c) ~fun_bodies:false 
    |> Iter.filter_map (fun (t,p) -> 
        let ty = Term.ty t and hd = Term.head_term t in
        let cached_t = Subst.FO.canonize_all_vars t in
        if not (Term.Set.mem cached_t !Higher_order.prim_enum_terms) &&
           Type.is_fun ty && Type.returns_prop ty && not (Term.is_var hd) &&
           not (trivial_trigger t) then (        
          Some t
        ) else None
      )

  let handle_pred_var_inst c =
    if C.proof_depth c < Env.flex_get k_trigger_bool_inst then (
      if not (CCList.is_empty (pred_vars c)) then (
        _cls_w_pred_vars := C.ClauseSet.add c !_cls_w_pred_vars;
      );
      _trigger_bools := Term.Set.add_seq !_trigger_bools (get_triggers c);
    );
    Signal.ContinueListening

  let fluidsup_applicable cl =
    not (Env.flex_get k_restrict_fluidsup) ||
    Array.length (C.lits cl) <= 2 ||  (C.proof_depth cl) == 0

  (* Syntactic overapproximation of fluid or deep terms *)
  let is_fluid_or_deep c t = 
    (* Fluid (1): Applied variables *)
    T.is_var (T.head_term t) && not (CCList.is_empty @@ T.args t) 
    (* Fluid (2): A lambda-expression that might eta-reduce to a non-lambda-expression after substitution (overapproximated) *)
    || T.is_fun t && not (T.is_ground t)                          
    (* Deep: A variable also occurring in a lambda-expression or in an argument of a variable in the same clause*)
    || match T.as_var t with
    | Some v -> 
      Lits.fold_terms ~vars:false ~var_args:false ~fun_bodies:false 
        ~ty_args:false ~which:`All ~ord ~subterms:true
        ~eligible:(fun _ _ -> true) (C.lits c)
      |> Iter.exists 
        (fun (t, _) -> 
           match T.view t with
           | App (head, args) when T.is_var head -> 
             Iter.exists (HVar.equal Type.equal v) (args |> Iter.of_list |> Iter.flat_map T.Seq.vars)
           | Fun (_, body) -> 
             Iter.exists (HVar.equal Type.equal v) (T.Seq.vars body)
           | _ -> false)
    | None -> false

  (* apply operation [f] to some parts of the clause [c] just added/removed
     from the active set *)
  let _update_active f c =
    (* index subterms that can be rewritten by superposition *)
    let sup_at_vars = Env.flex_get k_sup_at_vars in
    let sup_in_var_args = Env.flex_get k_sup_in_var_args in
    let sup_under_lambdas = Env.flex_get k_sup_under_lambdas in
    let sup_t_f = Env.flex_get k_sup_true_false in
    let sup_at_var_headed = Env.flex_get k_sup_at_var_headed in
    let fluidsup = Env.flex_get k_fluidsup in
    let dupsup = Env.flex_get k_dupsup in
    let lambdasup = Env.flex_get k_lambdasup in
    let sup_with_pure_vars = Env.flex_get k_sup_w_pure_vars in
    let demod_in_var_args = Env.flex_get k_demod_in_var_args in
    let lambda_demod = Env.flex_get k_lambda_demod in


    _idx_sup_into :=
      Lits.fold_terms ~vars:sup_at_vars ~var_args:sup_in_var_args ~fun_bodies:sup_under_lambdas 
        ~ty_args:false ~ord ~which:`Max ~subterms:true  ~eligible:(C.Eligible.res c) (C.lits c)
      |> Iter.filter (fun (t, _) ->
          (* Util.debugf ~section 3 "@[ Filtering vars %a,1  @]" (fun k-> k T.pp t); *)
          (sup_t_f || not (Term.is_true_or_false t)) &&
          (not (T.is_var t) || T.is_ho_var t))
      (* TODO: could exclude more variables from the index:
         they are not needed if they occur with the same args everywhere in the clause *)
      |> Iter.filter (fun (t, _) ->
          (* Util.debugf ~section 3 "@[ Filtering vars %a,2  @]" (fun k-> k T.pp t); *)
          sup_at_var_headed || not (T.is_var (T.head_term t)))
      |> Iter.fold
        (fun tree (t, pos) ->
           (* Util.debugf ~section 3 "@[ Adding %a to into index %B @]" (fun k-> k T.pp t !_sup_under_lambdas); *)
           let with_pos = C.WithPos.({term=t; pos; clause=c;}) in
           f tree t with_pos)
        !_idx_sup_into;

    (* index subterms that can be rewritten by FluidSup *)
    if fluidsup then
      _idx_fluidsup_into :=
        Lits.fold_terms ~vars:true ~var_args:false ~fun_bodies:false
          ~ty_args:false ~ord ~which:`Max ~subterms:true
          ~eligible:(C.Eligible.res c) (C.lits c)
        |> Iter.filter (fun (t, _) -> is_fluid_or_deep c t) 
        |> Iter.fold
          (fun tree (t, pos) ->
             let with_pos = C.WithPos.({term=t; pos; clause=c;}) in
             f tree t with_pos)
          !_idx_fluidsup_into;

    if dupsup then 
      _idx_dupsup_into :=
        Lits.fold_terms ~vars:false ~var_args:false ~fun_bodies:false
          ~ty_args:false ~ord ~which:`Max ~subterms:true
          ~eligible:(C.Eligible.res c) (C.lits c)
        |> Iter.filter (fun (t, _) -> 
            T.is_var (T.head_term t) && not (CCList.is_empty @@ T.args t)
            && Type.is_ground (T.ty t)) (* Only applied variables *)
        |> Iter.fold
          (fun tree (t, pos) ->
             let with_pos = C.WithPos.({term=t; pos; clause=c;}) in
             f tree t with_pos)
          !_idx_dupsup_into;

    (* index subterms that can be rewritten by LambdaSup --
       the ones that can rewrite those are actually the ones
       already indexed by _idx_sup_from*)
    if lambdasup != _NO_LAMSUP then
      _idx_lambdasup_into :=
        Lits.fold_terms ~vars:sup_at_vars ~var_args:sup_in_var_args
          ~fun_bodies:true ~ty_args:false ~ord
          ~which:`Max ~subterms:true
          ~eligible:(C.Eligible.res c) (C.lits c)
        (* We are going only under lambdas *)
        |> Iter.filter_map (fun (t, p) ->
            if not (T.is_fun t) then None
            else (let tyargs, body = T.open_fun t in
                  let new_pos = List.fold_left (fun p _ -> P.(append p (body stop))) p tyargs in
                  let hd = T.head_term body in
                  if (not (T.is_var body) || T.is_ho_var body) &&
                     (not (T.is_const hd) || not (ID.is_skolem (T.as_const_exn hd))) &&
                     (sup_at_var_headed || not (T.is_var (T.head_term body))) then
                    ( (*CCFormat.printf "Adding %a to LS index.\n" T.pp body; *)
                      Some (body, new_pos)) else None))
        |> Iter.fold
          (fun tree (t, pos) ->
             let with_pos = C.WithPos.({term=t; pos; clause=c;}) in
             f tree t with_pos)
          !_idx_lambdasup_into;

    (* index terms that can rewrite into other clauses *)
    _idx_sup_from :=
      Lits.fold_eqn ~ord ~both:true ~sign:true
        ~eligible:(C.Eligible.param c) (C.lits c)
      |> Iter.filter (fun (l,r,_,_) -> 
          (sup_with_pure_vars || not (Term.is_var l) || not (Term.is_var r))
          && (sup_t_f || not (Term.is_true_or_false l)))
      |> Iter.filter((fun (l, _, _, _) -> not (T.equal l T.false_)))
      |> Iter.fold
        (fun tree (l, _, sign, pos) ->
           assert sign;
           let with_pos = C.WithPos.({term=l; pos; clause=c;}) in
           f tree l with_pos)
        !_idx_sup_from ;
    (* terms that can be demodulated: all subterms (but vars) *)
    _idx_back_demod :=
      (* TODO: allow demod under lambdas under certain conditions (DemodExt) *)
      Lits.fold_terms ~vars:false ~var_args:(demod_in_var_args) ~fun_bodies:lambda_demod  
        ~ty_args:false ~ord ~subterms:true ~which:`All
        ~eligible:C.Eligible.always (C.lits c)
      |> Iter.fold
        (fun tree (t, pos) ->
           let with_pos = C.WithPos.( {term=t; pos; clause=c} ) in
           f tree t with_pos)
        !_idx_back_demod;
    Signal.ContinueListening

  (* update simpl. index using the clause [c] just added or removed to
     the simplification set *)
  let _update_simpl f c =
    assert (CCArray.for_all Lit.no_prop_invariant (C.lits c));
    let idx = !_idx_simpl in
    let idx' = match C.lits c with
      | [| Lit.Equation (l,r,true) |] ->
        begin match Ordering.compare ord l r with
          | Comparison.Gt ->
            f idx (l,r,true,c)
          | Comparison.Lt ->
            assert(not (T.is_true_or_false r));
            f idx (r,l,true,c)
          | Comparison.Incomparable ->
            assert(T.is_var (T.head_term l) || not (T.is_true_or_false l));
            let idx = f idx (l,r,true,c) in
            f idx (r,l,true,c)
          | Comparison.Eq -> idx  (* no modif *)
        end
      | [| Lit.Equation (l,r,false) |] ->
        assert(not (T.equal r T.true_ || T.equal r T.false_));
        f idx (l,r,false,c)
      | _ -> idx
    in
    _idx_simpl := idx';
    Signal.ContinueListening

  let insert_into_ext_dec_index index (c,pos,t) =
    let key = T.head_exn t in
    let clause_map = ID.Map.find_opt key !index in
    let clause_map = match clause_map with 
      | None -> C.Tbl.create 8
      | Some res -> res in
    let all_pos =
      (try 
         (t,pos) :: (C.Tbl.find clause_map c)
       with _ -> 
         [(t,pos)]) in
    C.Tbl.replace clause_map c all_pos;
    index := ID.Map.add key clause_map !index

  let remove_from_ext_dec_index index (c,_,t) =
    let key = T.head_exn t in
    let clause_map = ID.Map.find_opt key !index in
    match clause_map with
    | None -> Util.debugf ~section 1 "all clauses allready deleted." CCFun.id
    | Some res -> (
        C.Tbl.remove res c;
        index := ID.Map.add key res !index
      )

  let update_ext_dec_indices f c =
    let which, eligible = if Env.flex_get k_ext_dec_lits = `OnlyMax 
      then `Max, C.Eligible.res c else `All, C.Eligible.always in
    if C.proof_depth c <= Env.flex_get k_max_lits_ext_dec then (
      Lits.fold_terms ~vars:false ~var_args:false ~fun_bodies:false ~ty_args:false 
        ~ord ~which ~subterms:true ~eligible (C.lits c)
      |> Iter.filter (fun (t, _) ->
          not (T.is_var t) || T.is_ho_var t)
      |> Iter.filter (fun (t, _) ->
          not (T.is_var (T.head_term t)) &&
          T.is_const (T.head_term t) && Term.has_ho_subterm t)
      |> Iter.iter
        (fun (t, pos) ->
           f _ext_dec_into_idx (c,pos,t));

      let eligible = if Env.flex_get k_ext_dec_lits = `OnlyMax then C.Eligible.param c 
        else C.Eligible.always in
      Lits.fold_eqn ~ord ~both:true ~sign:true ~eligible (C.lits c)
      |> Iter.iter
        (fun (l, _, sign, pos) ->
           assert sign;
           let hd,_ = T.as_app l in
           if T.is_const hd && Term.has_ho_subterm l then (
             (* CCFormat.printf "adding %a to ext_dec index.\n" T.pp l; *)
             f _ext_dec_from_idx (c,pos,l)
           )));
    Signal.ContinueListening


  let () =
    Signal.on PS.ActiveSet.on_add_clause
      (fun c ->
         _idx_fv := SubsumIdx.add !_idx_fv c;
         ignore(_update_active TermIndex.add c);
         ignore(handle_pred_var_inst c);
         update_ext_dec_indices insert_into_ext_dec_index c);
    Signal.on PS.ActiveSet.on_remove_clause
      (fun c ->
         _idx_fv := SubsumIdx.remove !_idx_fv c;
         _cls_w_pred_vars := C.ClauseSet.remove c !_cls_w_pred_vars;
         ignore(update_ext_dec_indices remove_from_ext_dec_index c);
         _update_active TermIndex.remove c);
    Signal.on PS.SimplSet.on_add_clause
      (_update_simpl UnitIdx.add);
    Signal.on PS.SimplSet.on_remove_clause
      (_update_simpl UnitIdx.remove);
    ()

  (** {6 Inference Rules} *)

  (* ----------------------------------------------------------------------
   * Superposition rule
   * ---------------------------------------------------------------------- *)

  type supkind =
    | Classic
    | FluidSup
    | LambdaSup
    | DupSup

  let kind_to_str = function
    | Classic -> "sup"
    | FluidSup -> "fluidSup"
    | LambdaSup -> "lambdaSup"
    | DupSup -> "dupSup"

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
      sup_kind: supkind;
    }
  end

  exception ExitSuperposition of string

  (* Checks whether we must allow superposition at variables to be complete. *)
  let sup_at_var_condition info var replacement =
    let open SupInfo in
    let us = info.subst in
    let subst = US.subst us in
    let renaming = S.Renaming.create () in
    let replacement' = S.FO.apply renaming subst (replacement, info.scope_active) in
    let var' = S.FO.apply renaming subst (var, info.scope_passive) in
    if (not (Type.is_fun (Term.ty var')) || not (O.might_flip ord var' replacement'))
    then (
      Util.debugf ~section 3
        "Cannot flip: %a = %a"
        (fun k->k T.pp var' T.pp replacement');
      false (* If the lhs vs rhs cannot flip, we don't need a sup at var *)
    )
    else (
      (* Check whether Cσ is >= C[var -> replacement]σ *)
      try
        let passive'_lits = Lits.apply_subst renaming subst (C.lits info.passive, info.scope_passive) in
        let subst_t = Unif.FO.bind_or_update subst (T.as_var_exn var, info.scope_passive) (replacement, info.scope_active) in
        let passive_t'_lits = Lits.apply_subst renaming subst_t (C.lits info.passive, info.scope_passive) in
        if Lits.compare_multiset ~ord passive'_lits passive_t'_lits = Comp.Gt
        then (
          Util.debugf ~section 3
            "Sup at var condition is not fulfilled because: %a >= %a"
            (fun k->k Lits.pp passive'_lits Lits.pp passive_t'_lits);
          false
        )
        else true (* If Cσ is either <= or incomparable to C[var -> replacement]σ, we need sup at var.*)
      with Unif.Fail -> true (* occurs check failed, do the inference -- check with Alex.*)
    )


  let dup_sup_apply_subst t sc_a sc_p subst renaming =
    let z, args = T.as_app t in
    assert(T.is_var z);
    assert(CCList.length args >= 2);
    let u_n, t' = CCList.take_drop (List.length args - 1) args in
    let in_passive = S.FO.apply renaming subst (T.app z u_n, sc_p) in
    let t' = S.FO.apply renaming subst (List.hd t', sc_a) in
    T.app in_passive [t']


  (* Helper that does one or zero superposition inference, with all
     the given parameters. Clauses have a scope. *)
  let do_classic_superposition info =
    let open SupInfo in
    let module P = Position in
    Util.incr_stat stat_superposition_call;
    let sc_a = info.scope_active in
    let sc_p = info.scope_passive in
    assert (InnerTerm.DB.closed (info.s:>InnerTerm.t));
    assert (info.sup_kind == LambdaSup || InnerTerm.DB.closed (info.u_p:T.t:>InnerTerm.t));
    assert (not(T.is_var info.u_p) || T.is_ho_var info.u_p || info.sup_kind = FluidSup);
    assert (Env.flex_get k_sup_at_var_headed || info.sup_kind = FluidSup || 
            info.sup_kind = DupSup || not (T.is_var (T.head_term info.u_p)));
    let active_idx = Lits.Pos.idx info.active_pos in
    let shift_vars = if info.sup_kind = LambdaSup then 0 else -1 in
    let passive_idx, passive_lit_pos = Lits.Pos.cut info.passive_pos in
    assert(Array.for_all Literal.no_prop_invariant (C.lits info.passive));
    assert(Array.for_all Literal.no_prop_invariant (C.lits info.passive));
    try
      Util.debugf ~section 3 
        "@[<2>sup, kind %s@ (@[<2>%a[%d]@ @[s=%a@]@ @[t=%a@]@])@ \
         (@[<2>%a[%d]@ @[passive_lit=%a@]@ @[p=%a@]@])@ with subst=@[%a@]@].\n"
        (fun k -> k
            (kind_to_str info.sup_kind) C.pp info.active sc_a T.pp info.s T.pp info.t
            C.pp info.passive sc_p Lit.pp info.passive_lit
            Position.pp info.passive_pos US.pp info.subst);

      let renaming = S.Renaming.create () in
      let us = info.subst in
      let subst = US.subst us in
      let lambdasup_vars =
        if (info.sup_kind = LambdaSup) then (
          Term.Seq.subterms info.u_p |> Iter.filter Term.is_var |> Term.Set.of_seq)
        else Term.Set.empty in
      let t' = if info.sup_kind != DupSup then 
          S.FO.apply ~shift_vars renaming subst (info.t, sc_a)
        else dup_sup_apply_subst info.t sc_a sc_p subst renaming in
      Util.debugf ~section 1
        "@[<2>sup, kind %s(%d)@ (@[<2>%a[%d]@ @[s=%a@]@ @[t=%a, t'=%a@]@])@ \
         (@[<2>%a[%d]@ @[passive_lit=%a@]@ @[p=%a@]@])@ with subst=@[%a@]@]"
        (fun k->k (kind_to_str info.sup_kind) (Term.Set.cardinal lambdasup_vars) C.pp info.active sc_a T.pp info.s T.pp info.t
            T.pp t' C.pp info.passive sc_p Lit.pp info.passive_lit
            Position.pp info.passive_pos US.pp info.subst);

      if(info.sup_kind = LambdaSup && 
         T.Set.exists (fun v -> 
             not @@ T.DB.is_closed @@  Subst.FO.apply renaming subst (v,sc_p)) 
           lambdasup_vars) then (
        Util.debugf ~section 3 "LambdaSup -- an into free variable sneaks in bound variable" (fun k->k);
        raise @@ ExitSuperposition("LambdaSup -- an into free variable sneaks in bound variable");
      );

      begin match info.passive_lit, info.passive_pos with
        | Lit.Equation (_, v, true), P.Arg(_, P.Left P.Stop)
        | Lit.Equation (v, _, true), P.Arg(_, P.Right P.Stop) ->
          (* are we in the specific, but no that rare, case where we
             rewrite s=t using s=t (into a tautology t=t)? *)
          (* TODO: use Unif.FO.eq? *)
          let v' = S.FO.apply ~shift_vars:0 renaming subst (v, sc_p) in
          if T.equal t' v'
          then (
            Util.debugf ~section 3 "will yield a tautology" (fun k->k);
            raise (ExitSuperposition "will yield a tautology");)
        | _ -> ()
      end;

      if (info.sup_kind = LambdaSup) then (
        let vars_a = CCArray.except_idx (C.lits info.active) active_idx
                     |> CCArray.of_list |> Literals.vars |> T.VarSet.of_list in
        let vars_p = C.lits info.passive |> Literals.vars |> T.VarSet.of_list in
        let dbs = ref [] in
        let vars_bound_to_closed_terms var_set scope =
          T.VarSet.iter (fun v -> 
              match Subst.FO.get_var subst ((v :> InnerTerm.t HVar.t),scope) with
              | Some (t,_) -> dbs := T.DB.unbound t @ !dbs (* hack *)
              | None -> ()) var_set in
        (* I am going crazy from different castings  *)
        vars_bound_to_closed_terms vars_a sc_a;
        vars_bound_to_closed_terms vars_p sc_p;

        if Util.Int_set.cardinal (Util.Int_set.of_list !dbs)  > Env.flex_get k_lambdasup   then (
          Util.debugf ~section 3 "Too many skolems will be introduced for LambdaSup." (fun k->k);
          raise (ExitSuperposition "Too many skolems will be introduced for LambdaSup.");
        )
      );     

      let subst', new_sk =
        if info.sup_kind = LambdaSup then
          S.FO.unleak_variables subst else subst, [] in
      let passive_lit' = Lit.apply_subst_no_simp renaming subst' (info.passive_lit, sc_p) in
      let new_trail = C.trail_l [info.active; info.passive] in
      if Env.is_trivial_trail new_trail then raise (ExitSuperposition "trivial trail");
      let s' = S.FO.apply ~shift_vars renaming subst (info.s, sc_a) in
      if (
        O.compare ord s' t' = Comp.Lt ||
        not (Lit.Pos.is_max_term ~ord passive_lit' passive_lit_pos) ||
        not (BV.get (C.eligible_res (info.passive, sc_p) subst) passive_idx) ||
        not (C.is_eligible_param (info.active, sc_a) subst ~idx:active_idx)
      ) then raise (ExitSuperposition "bad ordering conditions");
      (* Check for superposition at a variable *)
      if info.sup_kind != FluidSup then
        if not @@ Env.flex_get k_sup_at_vars then
          assert (not (T.is_var info.u_p))
        else if T.is_var info.u_p && not (sup_at_var_condition info info.u_p info.t) then (
          Util.debugf ~section 3 "superposition at variable" (fun k->k);
          raise (ExitSuperposition "superposition at variable");
        );

      (* ordering constraints are ok *)
      let lits_a = CCArray.except_idx (C.lits info.active) active_idx in
      let lits_p = CCArray.except_idx (C.lits info.passive) passive_idx in
      (* replace s\sigma by t\sigma in u|_p\sigma *)
      let new_passive_lit =
        Lit.Pos.replace passive_lit'
          ~at:passive_lit_pos ~by:t' in
      let c_guard = Literal.of_unif_subst renaming us in
      (* apply substitution to other literals *)
      (* Util.debugf 1 "Before unleak: %a, after unleak: %a"
         (fun k -> k Subst.pp subst Subst.pp subst'); *)
      let new_lits =
        new_passive_lit ::
        c_guard @
        Lit.apply_subst_list renaming subst' (lits_a, sc_a) @
        Lit.apply_subst_list renaming subst' (lits_p, sc_p)
      in
      (* For some reason type comparison does not work. *)

      let pos_enclosing_up = Position.until_first_fun passive_lit_pos in
      let around_up =  Subst.FO.apply renaming subst' 
          (Lit.Pos.at info.passive_lit pos_enclosing_up, sc_p) in
      let vars = Iter.append (T.Seq.vars around_up) (T.Seq.vars t')
                 |> Term.VarSet.of_seq
                 |> Term.VarSet.to_list in
      let skolem_decls = ref [] in
      let sk_with_vars =
        List.fold_left (fun acc t ->
            let sk_decl, new_sk_vars = Term.mk_fresh_skolem vars (Term.ty t) in
            skolem_decls := sk_decl :: !skolem_decls;
            Term.Map.add t new_sk_vars acc)
          Term.Map.empty new_sk in
      let new_lits =
        List.mapi (fun i lit ->
            Lit.map (fun t ->
                Term.Map.fold 
                  (fun sk sk_v acc -> 
                     (* For polymorphism -- will apply type substitution.  *)
                     let scope = if i < (List.length c_guard + List.length lits_a) 
                       then sc_a else sc_p in
                     let sk = S.FO.apply renaming subst (sk, scope) in
                     Term.replace ~old:sk ~by:sk_v acc)
                  sk_with_vars t ) lit) new_lits in

      if List.exists (fun lit -> 
          Lit.Seq.terms lit 
          |> Iter.exists (fun t ->
              not (Lambda.is_properly_encoded t))) 
          new_lits then (
        raise (ExitSuperposition "improperly formed quantified expressions.");
      );

      let subst_has_lams = 
        Subst.codomain subst
        |> Iter.exists (fun (t,_) -> 
            Iter.exists T.is_fun (T.Seq.subterms (T.of_term_unsafe t))) in

      let rule =
        let r = kind_to_str info.sup_kind in
        let sign = if Lit.is_pos passive_lit' then "+" else "-" in
        Proof.Rule.mk (r ^ sign)
      in
      CCList.iter (fun (sym,ty) -> Ctx.declare sym ty) !skolem_decls;
      let tags = (if subst_has_lams then [Proof.Tag.T_ho] else []) @ Unif_subst.tags us in
      let proof =
        Proof.Step.inference ~rule ~tags
          [C.proof_parent_subst renaming (info.active,sc_a) subst';
           C.proof_parent_subst renaming (info.passive,sc_p) subst']
      and penalty =
        max (C.penalty info.active) (C.penalty info.passive)
        + (if T.is_var s' then 2 else 0) (* superposition from var = bad *)
      in
      let new_clause = C.create ~trail:new_trail ~penalty new_lits proof in
      (* Format.printf "LS: %a\n" C.pp new_clause;  *)
      Util.debugf ~section 3 "@[... ok, conclusion@ @[%a@]@]" (fun k->k C.pp new_clause);
      assert (List.for_all (Lit.for_all Term.DB.is_closed) new_lits); 
      assert(Array.for_all Literal.no_prop_invariant (C.lits new_clause));
      Some new_clause
    with ExitSuperposition reason ->
      Util.debugf ~section 3 "... cancel, %s" (fun k->k reason);
      None

  (* simultaneous superposition: when rewriting D with C \lor s=t,
      replace s with t everywhere in D rather than at one place. *)
  let do_simultaneous_superposition info =
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
    assert (info.sup_kind == LambdaSup || InnerTerm.DB.closed (info.u_p:T.t:>InnerTerm.t));
    assert (not(T.is_var info.u_p) || T.is_ho_var info.u_p || info.sup_kind = FluidSup);
    assert (Env.flex_get k_sup_at_var_headed || info.sup_kind = FluidSup || 
            info.sup_kind = DupSup || not (T.is_var (T.head_term info.u_p)));
    let active_idx = Lits.Pos.idx info.active_pos in
    let passive_idx, passive_lit_pos = Lits.Pos.cut info.passive_pos in
    let shift_vars = if info.sup_kind = LambdaSup then 0 else -1 in
    try
      let renaming = S.Renaming.create () in
      let us = info.subst in
      let subst = US.subst us in
      let t' = S.FO.apply ~shift_vars renaming subst (info.t, sc_a) in
      begin match info.passive_lit, info.passive_pos with
        | Lit.Equation (_, v, true), P.Arg(_, P.Left P.Stop)
        | Lit.Equation (v, _, true), P.Arg(_, P.Right P.Stop) ->
          (* are we in the specific, but no that rare, case where we
             rewrite s=t using s=t (into a tautology t=t)? *)
          let v' = S.FO.apply ~shift_vars renaming subst (v, sc_p) in
          if T.equal t' v'
          then raise (ExitSuperposition "will yield a tautology");
        | _ -> ()
      end;
      let passive_lit' =
        Lit.apply_subst_no_simp renaming subst (info.passive_lit, sc_p)
      in
      let new_trail = C.trail_l [info.active; info.passive] in
      if Env.is_trivial_trail new_trail then raise (ExitSuperposition "trivial trail");
      let s' = S.FO.apply ~shift_vars renaming subst (info.s, sc_a) in
      if (
        O.compare ord s' t' = Comp.Lt ||
        not (Lit.Pos.is_max_term ~ord passive_lit' passive_lit_pos) ||
        not (BV.get (C.eligible_res (info.passive, sc_p) subst) passive_idx) ||
        not (C.is_eligible_param (info.active, sc_a) subst ~idx:active_idx)
      ) then raise (ExitSuperposition "bad ordering conditions");
      (* Check for superposition at a variable *)
      if info.sup_kind != FluidSup then
        if not @@ Env.flex_get k_sup_at_vars then
          assert (not (T.is_var info.u_p))
        else if T.is_var info.u_p && not (sup_at_var_condition info info.u_p info.t) then
          raise (ExitSuperposition "superposition at variable");
      (* ordering constraints are ok, build new active lits (excepted s=t) *)
      let lits_a = CCArray.except_idx (C.lits info.active) active_idx in
      let lits_a = Lit.apply_subst_list renaming subst (lits_a, sc_a) in
      (* build passive literals and replace u|p\sigma with t\sigma *)
      let u' = S.FO.apply ~shift_vars renaming subst (info.u_p, sc_p) in
      assert (Type.equal (T.ty u') (T.ty t'));
      let lits_p = Array.to_list (C.lits info.passive) in
      let lits_p = Lit.apply_subst_list renaming subst (lits_p, sc_p) in
      (* assert (T.equal (Lits.Pos.at (Array.of_list lits_p) info.passive_pos) u'); *)
      let lits_p = List.map (Lit.map (fun t-> T.replace t ~old:u' ~by:t')) lits_p in
      let c_guard = Literal.of_unif_subst renaming us in
      (* build clause *)
      let new_lits = c_guard @ lits_a @ lits_p in
      let rule =
        let r = kind_to_str info.sup_kind in
        let sign = if Lit.is_pos passive_lit' then "+" else "-" in
        Proof.Rule.mk ("s_" ^ r ^ sign)
      in
      let subst_has_lams = 
        Subst.codomain subst
        |> Iter.exists (fun (t,_) -> 
            Iter.exists T.is_fun (T.Seq.subterms (T.of_term_unsafe t))) in
      let tags = (if subst_has_lams then [Proof.Tag.T_ho] else []) @ Unif_subst.tags us in
      let proof =
        Proof.Step.inference ~rule ~tags
          [C.proof_parent_subst renaming (info.active,sc_a) subst;
           C.proof_parent_subst renaming (info.passive,sc_p) subst]
      and penalty =
        max (C.penalty info.active) (C.penalty info.passive)
        + (if T.is_var s' then 2 else 0) (* superposition from var = bad *)
      in
      let new_clause = C.create ~trail:new_trail ~penalty new_lits proof in
      Util.debugf ~section 3 "@[... ok, conclusion@ @[%a@]@]" (fun k->k C.pp new_clause);
      Some new_clause
    with ExitSuperposition reason ->
      Util.debugf ~section 3 "@[... cancel, %s@]" (fun k->k reason);
      None

  (* choose between regular and simultaneous superposition *)
  let do_superposition info =
    let open SupInfo in
    assert (info.sup_kind=DupSup || Type.equal (T.ty info.s) (T.ty info.t));
    assert (info.sup_kind=DupSup ||
            Unif.Ty.equal ~subst:(US.subst info.subst)
              (T.ty info.s, info.scope_active) (T.ty info.u_p, info.scope_passive));
    let renaming = Subst.Renaming.create () in
    let s = Subst.FO.apply renaming (US.subst info.subst) (info.s, info.scope_active) in
    let u_p = Subst.FO.apply renaming (US.subst info.subst) (info.u_p, info.scope_passive) in
    assert(Term.equal (Lambda.eta_reduce @@ Lambda.snf @@ s) (Lambda.eta_reduce @@ Lambda.snf @@ u_p) || US.has_constr info.subst);
    if Env.flex_get k_use_simultaneous_sup && info.sup_kind != LambdaSup && info.sup_kind != DupSup
    then do_simultaneous_superposition info
    else do_classic_superposition info

  let infer_active_aux ~retrieve_from_index ~process_retrieved clause =
    Util.enter_prof prof_infer_active;
    (* no literal can be eligible for paramodulation if some are selected.
       This checks if inferences with i-th literal are needed? *)
    let eligible = C.Eligible.param clause in
    (* do the inferences where clause is active; for this,
       we try to rewrite conditionally other clauses using
       non-minimal sides of every positive literal *)
    let new_clauses =
      Lits.fold_eqn ~sign:true ~ord
        ~both:true ~eligible (C.lits clause)
      |> Iter.filter (fun (l,r,_,_) -> 
          (Env.flex_get k_sup_w_pure_vars || not (Term.is_var l) || not (Term.is_var r))
          && (Env.flex_get k_sup_true_false || not (Term.is_true_or_false l)))
      |> Iter.flat_map
        (fun (s, t, _, s_pos) ->
           let do_sup u_p with_pos subst =
             (* rewrite u_p with s *)
             if T.DB.is_closed u_p
             then
               let passive = with_pos.C.WithPos.clause in
               let passive_pos = with_pos.C.WithPos.pos in
               let passive_lit, _ = Lits.Pos.lit_at (C.lits passive) passive_pos in
               let info = SupInfo.( {
                   s; t; active=clause; active_pos=s_pos; scope_active=0;
                   u_p; passive; passive_lit; passive_pos; scope_passive=1; subst; sup_kind=Classic
                 }) in
               do_superposition info
             else None
           in
           (* rewrite clauses using s *)
           retrieve_from_index (!_idx_sup_into, 1) (s, 0)
           |> Iter.filter_map (process_retrieved do_sup)
        )
      |> Iter.to_rev_list

    in
    Util.exit_prof prof_infer_active;
    new_clauses

  let infer_passive_aux ~retrieve_from_index ~process_retrieved clause =
    Util.enter_prof prof_infer_passive;
    (* perform inference on this lit? *)
    let eligible = C.Eligible.(res clause) in
    (* do the inferences in which clause is passive (rewritten),
       so we consider both negative and positive literals *)
    let new_clauses =
      Lits.fold_terms ~vars:(Env.flex_get k_sup_at_vars) 
        ~var_args:(Env.flex_get k_sup_in_var_args)
        ~fun_bodies:(Env.flex_get k_sup_under_lambdas) 
        ~subterms:true ~ord ~which:`Max ~eligible ~ty_args:false 
        (C.lits clause)
      |> Iter.filter (fun (u_p, _) -> not (T.is_var u_p) || T.is_ho_var u_p)
      |> Iter.filter (fun (u_p, _) -> T.DB.is_closed u_p)
      |> Iter.filter (fun (u_p, _) -> 
          Env.flex_get k_sup_at_var_headed || not (T.is_var (T.head_term u_p)))
      |> Iter.flat_map
        (fun (u_p, passive_pos) ->
           let passive_lit, _ = Lits.Pos.lit_at (C.lits clause) passive_pos in
           let do_sup _ with_pos subst =
             let active = with_pos.C.WithPos.clause in
             let s_pos = with_pos.C.WithPos.pos in
             match Lits.View.get_eqn (C.lits active) s_pos with
             | Some (s, t, true) ->
               let info = SupInfo.({
                   s; t; active; active_pos=s_pos; scope_active=1; subst;
                   u_p; passive=clause; passive_lit; passive_pos; scope_passive=0; sup_kind=Classic
                 }) in
               do_superposition info
             | _ -> None
           in
           (* all terms that occur in an equation in the active_set
              and that are potentially unifiable with u_p (u at position p) *)
           retrieve_from_index (!_idx_sup_from, 1) (u_p,0)
           |> Iter.filter_map (process_retrieved do_sup)
        )
      |> Iter.to_rev_list
    in
    Util.exit_prof prof_infer_passive;
    new_clauses

  let infer_active clause =
    infer_active_aux
      ~retrieve_from_index:I.retrieve_unifiables
      ~process_retrieved:(fun do_sup (u_p, with_pos, subst) -> do_sup u_p with_pos subst)
      clause

  let infer_lambdasup_from clause =
    (* no literal can be eligible for paramodulation if some are selected.
       This checks if inferences with i-th literal are needed? *)
    let eligible = C.Eligible.param clause in
    (* do the inferences where clause is active; for this,
       we try to rewrite conditionally other clauses using
       non-minimal sides of every positive literal *)
    Lits.fold_eqn ~sign:true ~ord  ~both:true ~eligible (C.lits clause)
    |> Iter.flat_map
      (fun (s, t, _, s_pos) ->
         let do_lambdasup u_p with_pos subst =
           (* rewrite u_p with s *)
           let passive = with_pos.C.WithPos.clause in
           let passive_pos = with_pos.C.WithPos.pos in
           let passive_lit, _ = Lits.Pos.lit_at (C.lits passive) passive_pos in
           let info = SupInfo.( {
               s; t; active=clause; active_pos=s_pos; scope_active=0;
               u_p; passive; passive_lit; passive_pos; scope_passive=1;
               subst; sup_kind=LambdaSup
             }) in
           Util.debugf ~section 10 "[Trying lambdasup from %a into %a with term %a into term %a]"
             (fun k -> k C.pp clause C.pp passive T.pp s T.pp u_p);

           do_superposition info
         in
         I.retrieve_unifiables (!_idx_lambdasup_into, 1) (s, 0)
         |> Iter.filter_map (fun (u_p, with_pos, subst) -> do_lambdasup u_p with_pos subst))
    |> Iter.to_rev_list

  let infer_passive clause =
    infer_passive_aux
      ~retrieve_from_index:I.retrieve_unifiables
      ~process_retrieved:(fun do_sup (u_p, with_pos, subst) -> do_sup u_p with_pos subst)
      clause

  let infer_lambdasup_into clause =
    (* perform inference on this lit? *)
    let eligible = C.Eligible.(res clause) in
    (* do the inferences in which clause is passive (rewritten),
       so we consider both negative and positive literals *)
    let new_clauses =
      Lits.fold_terms ~vars:(Env.flex_get k_sup_at_vars) 
        ~var_args:(Env.flex_get k_sup_in_var_args)
        ~fun_bodies:true ~subterms:true ~ord
        ~which:`Max ~eligible ~ty_args:false (C.lits clause)
      |> Iter.filter_map (fun (u_p, p) ->
          (* we rewrite only under lambdas  *)
          if not (T.is_fun u_p) then None
          else (let tyargs, body = T.open_fun u_p in
                let hd = T.head_term body in
                let new_pos = List.fold_left (fun p _ -> P.(append p (body stop)) ) p tyargs in
                (* we check normal superposition conditions  *)
                if (not (T.is_var body) || T.is_ho_var body) &&
                   (not (T.is_const hd) || not (ID.is_skolem (T.as_const_exn hd))) &&
                   (Env.flex_get k_sup_at_var_headed || not (T.is_var (T.head_term body))) then
                  Some (body, new_pos)
                else None) )
      |> Iter.flat_map
        (fun (u_p, passive_pos) ->
           let passive_lit, _ = Lits.Pos.lit_at (C.lits clause) passive_pos in
           let do_sup _ with_pos subst =
             let active = with_pos.C.WithPos.clause in
             let s_pos = with_pos.C.WithPos.pos in
             match Lits.View.get_eqn (C.lits active) s_pos with
             | Some (s, t, true) ->
               let info = SupInfo.({
                   s; t; active; active_pos=s_pos; scope_active=1; subst;
                   u_p; passive=clause; passive_lit; passive_pos;
                   scope_passive=0; sup_kind=LambdaSup
                 }) in
               Util.debugf ~section 10 "[Trying lambdasup from %a into %a with term %a into term %a]"
                 (fun k -> k C.pp active C.pp clause T.pp s T.pp u_p);
               do_superposition info
             | _ -> None
           in
           (* all terms that occur in an equation in the active_set
              and that are potentially unifiable with u_p (u at position p) *)
           I.retrieve_unifiables (!_idx_sup_from, 1) (u_p,0)
           |> Iter.filter_map (fun (t,p,s) -> do_sup t p s))
      |> Iter.to_rev_list
    in
    Util.exit_prof prof_infer_passive;
    new_clauses

  let infer_active_complete_ho clause =
    let inf_res = infer_active_aux
        ~retrieve_from_index:(I.retrieve_unifiables_complete ~unif_alg:(Env.flex_get k_unif_alg))
        ~process_retrieved:(fun do_sup (u_p, with_pos, substs) ->
            let penalty = max (C.penalty clause) (C.penalty with_pos.C.WithPos.clause) in
            (* /!\ may differ from the actual penalty (by -2) *)
            Some (penalty, OSeq.map (CCOpt.flat_map (do_sup u_p with_pos)) substs))
        clause
    in
    let stm_res = List.map (fun (p,s) -> Stm.make ~penalty:p s) inf_res in
    StmQ.add_lst _stmq.q stm_res; []

  let infer_passive_complete_ho clause =
    let inf_res = infer_passive_aux
        ~retrieve_from_index:(I.retrieve_unifiables_complete ~unif_alg:(Env.flex_get k_unif_alg))
        ~process_retrieved:(fun do_sup (u_p, with_pos, substs) ->
            let penalty = max (C.penalty clause) (C.penalty with_pos.C.WithPos.clause) in
            (* /!\ may differ from the actual penalty (by -2) *)
            Some (penalty, OSeq.map (CCOpt.flat_map (do_sup u_p with_pos)) substs))
        clause
    in
    let stm_res = List.map (fun (p,s) -> Stm.make ~penalty:p s) inf_res in
    StmQ.add_lst _stmq.q stm_res; []

  let infer_active_pragmatic_ho max_unifs clause =
    let inf_res = infer_active_aux
        ~retrieve_from_index:(I.retrieve_unifiables_complete ~unif_alg:(Env.flex_get k_unif_alg))
        ~process_retrieved:(fun do_sup (u_p, with_pos, substs) ->
            let all_substs = OSeq.to_list @@ OSeq.take max_unifs @@ OSeq.filter_map CCFun.id substs   in
            let res = List.map (fun subst -> do_sup u_p with_pos subst) all_substs in
            Some res
          )
        clause
    in
    inf_res |> CCList.flatten |> List.filter CCOpt.is_some  |> List.map CCOpt.get_exn

  let infer_passive_pragmatic_ho max_unifs clause =
    let inf_res = infer_passive_aux
        ~retrieve_from_index:(I.retrieve_unifiables_complete ~unif_alg:(Env.flex_get k_unif_alg))
        ~process_retrieved:(fun do_sup (u_p, with_pos, substs) ->
            let all_substs = OSeq.to_list @@ OSeq.take max_unifs @@ OSeq.filter_map CCFun.id substs   in
            let res = List.map (fun subst -> do_sup u_p with_pos subst) all_substs in
            Some res   
          )
        clause
    in
    inf_res |> CCList.flatten |> List.filter CCOpt.is_some  |> List.map CCOpt.get_exn

  (* ----------------------------------------------------------------------
   * FluidSup rule (Superposition at applied variables)
   * ---------------------------------------------------------------------- *)

  let infer_fluidsup_active clause =
    Util.enter_prof prof_infer_fluidsup_active;
    (* no literal can be eligible for paramodulation if some are selected.
       This checks if inferences with i-th literal are needed? *)
    let eligible = C.Eligible.param clause in
    (* do the inferences where clause is active; for this,
       we try to rewrite conditionally other clauses using
       non-minimal sides of every positive literal *)
    let new_clauses =
      if fluidsup_applicable clause then
        Lits.fold_eqn ~sign:true ~ord ~both:true ~eligible (C.lits clause)
        |> Iter.flat_map
          (fun (s, t, _, s_pos) ->
             I.fold !_idx_fluidsup_into
               (fun acc u_p with_pos ->
                  assert (is_fluid_or_deep with_pos.C.WithPos.clause u_p);
                  assert (T.DB.is_closed u_p);
                  (* Create prefix variable H and use H s = H t for superposition *)
                  let var_h = T.var (HVar.fresh ~ty:(Type.arrow [T.ty s] (Type.var (HVar.fresh ~ty:Type.tType ()))) ()) in
                  let hs = T.app var_h [s] in
                  let ht = T.app var_h [t] in
                  let res = Env.flex_get k_unif_alg (u_p,1) (hs,0) |> OSeq.map (
                      fun osubst ->
                        osubst |> CCOpt.flat_map (
                          fun subst ->
                            let passive = with_pos.C.WithPos.clause in
                            let passive_pos = with_pos.C.WithPos.pos in
                            let passive_lit, _ = Lits.Pos.lit_at (C.lits passive) passive_pos in
                            let info = SupInfo.({
                                s=hs; t=ht; active=clause; active_pos=s_pos; scope_active=0;
                                u_p; passive; passive_lit; passive_pos; scope_passive=1; subst; sup_kind=FluidSup
                              }) in
                            do_superposition info
                        )
                    )
                  in
                  let penalty = 
                    max (C.penalty clause) (C.penalty with_pos.C.WithPos.clause)
                    + (Env.flex_get k_fluidsup_penalty) in
                  (* /!\ may differ from the actual penalty (by -2) *)
                  Iter.cons (penalty,res) acc
               )
               Iter.empty
          )
        |> Iter.to_rev_list
      else []
    in
    let stm_res = List.map (fun (p,s) -> Stm.make ~penalty:p s) new_clauses in
    StmQ.add_lst _stmq.q stm_res;
    Util.exit_prof prof_infer_fluidsup_active;
    []

  let infer_fluidsup_passive clause =
    Util.enter_prof prof_infer_fluidsup_passive;
    (* perform inference on this lit? *)
    let eligible = C.Eligible.(res clause) in
    (* do the inferences in which clause is passive (rewritten),
       so we consider both negative and positive literals *)
    let new_clauses =
      if fluidsup_applicable clause then
        Lits.fold_terms ~vars:true ~var_args:false ~fun_bodies:false ~subterms:true ~ord
          ~which:`Max ~eligible ~ty_args:false (C.lits clause)
        |> Iter.filter (fun (u_p, _) -> is_fluid_or_deep clause u_p)
        |> Iter.flat_map
          (fun (u_p, passive_pos) ->
             let passive_lit, _ = Lits.Pos.lit_at (C.lits clause) passive_pos in
             I.fold !_idx_sup_from
               (fun acc _ with_pos ->
                  let active = with_pos.C.WithPos.clause in
                  let s_pos = with_pos.C.WithPos.pos in
                  let res = match Lits.View.get_eqn (C.lits active) s_pos with
                    | Some (s, t, true) ->
                      (* Create prefix variable H and use H s = H t for superposition *)
                      let var_h = T.var (HVar.fresh ~ty:(Type.arrow [T.ty s] (Type.var (HVar.fresh ~ty:Type.tType ()))) ()) in
                      let hs = T.app var_h [s] in
                      let ht = T.app var_h [t] in
                      Env.flex_get k_unif_alg (hs,1) (u_p,0)
                      |> OSeq.map
                        (fun osubst ->
                           osubst |> CCOpt.flat_map (fun subst ->
                               let info = SupInfo.({
                                   s = hs; t = ht; active; active_pos=s_pos; scope_active=1; subst;
                                   u_p; passive=clause; passive_lit; passive_pos; scope_passive=0; sup_kind=FluidSup
                                 }) in
                               do_superposition info
                             )
                        )
                    | _ -> assert false
                  in
                  let penalty = 
                    max (C.penalty clause) (C.penalty with_pos.C.WithPos.clause) 
                    + Env.flex_get k_fluidsup_penalty in
                  (* /!\ may differ from the actual penalty (by -2) *)
                  Iter.cons (penalty,res) acc
               )
               Iter.empty
          )
        |> Iter.to_rev_list
      else []
    in
    let stm_res = List.map (fun (p,s) -> Stm.make ~penalty:p s) new_clauses in
    StmQ.add_lst _stmq.q stm_res;
    Util.exit_prof prof_infer_fluidsup_passive;
    []

  (* ----------------------------------------------------------------------
   * DupSup rule (Lightweight superposition at applied variables)
   * ---------------------------------------------------------------------- *)

  let infer_dupsup_active clause =
    let eligible = C.Eligible.param clause in
    let new_clauses =
      Lits.fold_eqn ~sign:true ~ord ~both:true ~eligible (C.lits clause)
      |> Iter.flat_map
        (fun (s, t, _, s_pos) ->
           I.fold !_idx_dupsup_into
             (fun acc u_p with_pos ->
                assert (T.is_var (T.head_term u_p));
                assert (T.DB.is_closed u_p);
                (* Create prefix variable H and use H s = H t for superposition *)
                if (T.Seq.vars s |> Iter.append (T.Seq.vars t)
                    |> Iter.exists (fun v -> Type.is_tType (HVar.ty v))) then (
                  acc
                )
                else (
                  let scope_passive, scope_active = 0, 1 in
                  let hd_up, args_up = T.as_app u_p in
                  let arg_types = List.map T.ty args_up in
                  let n = List.length args_up in
                  let var_up = T.as_var_exn hd_up in
                  let var_w = HVar.fresh ~ty:(Type.arrow arg_types (T.ty t)) () in
                  let var_z = HVar.fresh ~ty:(Type.arrow (List.append arg_types [T.ty t]) (T.ty u_p)) () in
                  let db_args = List.mapi (fun i ty -> T.bvar ~ty (n-1-i)) arg_types in
                  let term_w,term_z = T.var var_w, T.var var_z in
                  let w_db = T.app term_w db_args in
                  let z_db = T.app term_z (List.append db_args [w_db]) in
                  let y_subst_val = T.fun_l arg_types z_db in
                  assert (T.DB.is_closed y_subst_val);
                  let subst_y = US.FO.bind (US.empty) (var_up, scope_passive) (y_subst_val, scope_passive) in
                  let w_args = T.app term_w args_up in
                  let w_args = Subst.FO.apply Subst.Renaming.none (US.subst subst_y) (w_args,scope_passive) in
                  let z_args = T.app term_z (List.append args_up [t]) in
                  let res = Env.flex_get k_unif_alg (s,scope_active) (w_args,scope_passive) |> OSeq.map (
                      fun osubst ->
                        osubst |> CCOpt.flat_map (
                          fun subst ->
                            let subst = US.merge subst subst_y in
                            let passive = with_pos.C.WithPos.clause in
                            let passive_pos = with_pos.C.WithPos.pos in
                            let passive_lit, _ = Lits.Pos.lit_at (C.lits passive) passive_pos in
                            let info = SupInfo.({
                                s; t=z_args; active=clause; active_pos=s_pos; scope_active;
                                u_p=w_args; passive; passive_lit; passive_pos; scope_passive; subst; 
                                sup_kind=DupSup
                              }) in
                            do_superposition info
                        )
                    )
                  in
                  let penalty = 
                    max (C.penalty clause) (C.penalty with_pos.C.WithPos.clause) 
                    + Env.flex_get k_fluidsup_penalty in
                  (* /!\ may differ from the actual penalty (by -2) *)
                  Iter.cons (penalty,res) acc
                ))
             Iter.empty
        )
      |> Iter.to_rev_list
    in
    let stm_res = List.map (fun (p,s) -> Stm.make ~penalty:p s) new_clauses in
    StmQ.add_lst _stmq.q stm_res;
    Util.exit_prof prof_infer_fluidsup_active;
    []

  let infer_dupsup_passive clause =
    Util.enter_prof prof_infer_fluidsup_passive;
    (* perform inference on this lit? *)
    let eligible = C.Eligible.(res clause) in
    (* do the inferences in which clause is passive (rewritten),
       so we consider both negative and positive literals *)
    let new_clauses =
      Lits.fold_terms ~vars:false ~var_args:false ~fun_bodies:false ~subterms:true
        ~ord ~which:`Max ~eligible ~ty_args:false (C.lits clause)
      |> Iter.filter (fun (u_p, _) -> 
          (T.is_var (T.head_term u_p) && not (CCList.is_empty @@ T.args u_p)
           && Type.is_ground (T.ty u_p)))
      |> Iter.flat_map
        (fun (u_p, passive_pos) ->
           let passive_lit, _ = Lits.Pos.lit_at (C.lits clause) passive_pos in
           I.fold !_idx_sup_from
             (fun acc _ with_pos ->
                let active = with_pos.C.WithPos.clause in
                let s_pos = with_pos.C.WithPos.pos in
                match Lits.View.get_eqn (C.lits active) s_pos with
                | Some (s, t, true) -> (
                    if (T.Seq.vars s |> Iter.append (T.Seq.vars t)
                        |> Iter.exists (fun v -> Type.is_tType (HVar.ty v))) then (
                      acc
                    )
                    else (
                      let scope_passive, scope_active = 0, 1 in
                      let hd_up, args_up = T.as_app u_p in
                      let arg_types = List.map T.ty args_up in
                      let n = List.length args_up in
                      let var_up = T.as_var_exn hd_up in
                      let var_w = HVar.fresh ~ty:(Type.arrow arg_types (T.ty t)) () in
                      let var_z = HVar.fresh ~ty:(Type.arrow (List.append arg_types [(T.ty t)]) (T.ty u_p)) () in
                      let db_args = List.mapi (fun i ty -> T.bvar ~ty (n-1-i)) arg_types in
                      let term_w,term_z = T.var var_w, T.var var_z in
                      let w_db = T.app term_w db_args in
                      let z_db = T.app term_z (List.append db_args [w_db]) in
                      let y_subst_val = T.fun_l arg_types z_db in
                      assert (T.DB.is_closed y_subst_val);
                      let subst_y = US.FO.bind (US.empty) (var_up, scope_passive) (y_subst_val, scope_passive) in
                      let w_args = T.app term_w args_up in
                      let w_args = Subst.FO.apply Subst.Renaming.none (US.subst subst_y) (w_args,scope_passive) in
                      let z_args = T.app term_z (List.append args_up [t]) in
                      let res = Env.flex_get k_unif_alg (w_args,scope_passive) (s,scope_active) |> OSeq.map (
                          fun osubst ->
                            osubst |> CCOpt.flat_map (
                              fun subst ->
                                let subst = US.merge subst subst_y in
                                let info = SupInfo.({
                                    s; t=z_args; active; active_pos=s_pos; scope_active;
                                    u_p=w_args; passive=clause; passive_lit; passive_pos; scope_passive; subst; 
                                    sup_kind=DupSup
                                  }) in
                                do_superposition info
                            ))
                      in
                      let penalty = 
                        max (C.penalty clause) (C.penalty with_pos.C.WithPos.clause) 
                        + Env.flex_get k_fluidsup_penalty in
                      (* /!\ may differ from the actual penalty (by -2) *)
                      Iter.cons (penalty,res) acc))
                | _ -> acc)
             Iter.empty
        )
      |> Iter.to_rev_list
    in
    let stm_res = List.map (fun (p,s) -> Stm.make ~penalty:p s) new_clauses in
    StmQ.add_lst _stmq.q stm_res;
    Util.exit_prof prof_infer_fluidsup_passive;
    []


  (* ----------------------------------------------------------------------
   * Equality Resolution rule
   * ---------------------------------------------------------------------- *)

  let infer_equality_resolution_aux ~unify ~iterate_substs clause =
    Util.enter_prof prof_infer_equality_resolution;
    let eligible = C.Eligible.always in
    (* iterate on those literals *)
    let new_clauses =
      Lits.fold_eqn ~sign:false ~ord ~both:false ~eligible (C.lits clause)
      |> Iter.filter_map
        (fun (l, r, _, l_pos) ->
           let do_eq_res us =
             let pos = Lits.Pos.idx l_pos in
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
               let subst_has_lams = 
                 Subst.codomain subst
                 |> Iter.exists (fun (t,_) -> 
                     Iter.exists T.is_fun (T.Seq.subterms (T.of_term_unsafe t))) in
               let tags = (if subst_has_lams then [Proof.Tag.T_ho] else []) @ Unif_subst.tags us in
               let trail = C.trail clause and penalty = C.penalty clause in
               let proof = Proof.Step.inference ~rule ~tags
                   [C.proof_parent_subst renaming (clause,0) subst] in
               let new_clause = C.create ~trail ~penalty (c_guard@new_lits) proof in
               Util.debugf ~section 1 "@[<hv2>equality resolution on@ @[%a@]@ yields @[%a@],\n subst @[%a@]@]"
                 (fun k->k C.pp clause C.pp new_clause US.pp us);
               Some new_clause
             ) else None
           in
           let substs = unify (l, 0) (r, 0) in
           iterate_substs substs do_eq_res
        )
      |> Iter.to_rev_list
    in
    Util.exit_prof prof_infer_equality_resolution;
    new_clauses

  let infer_equality_resolution =
    infer_equality_resolution_aux
      ~unify:(fun l r -> try Some (Unif.FO.unify_full l r) with Unif.Fail -> None)
      ~iterate_substs:(fun substs do_eq_res -> CCOpt.flat_map do_eq_res substs)

  let infer_equality_resolution_complete_ho clause =
    let inf_res = infer_equality_resolution_aux
        ~unify:(Env.flex_get k_unif_alg)
        ~iterate_substs:(fun substs do_eq_res -> Some (OSeq.map (CCOpt.flat_map do_eq_res) substs))
        clause
    in
    let penalty = C.penalty clause in
    let stm_res = List.map (Stm.make ~penalty:penalty) inf_res in
    StmQ.add_lst _stmq.q stm_res; []

  let infer_equality_resolution_pragmatic_ho max_unifs clause =
    let inf_res = infer_equality_resolution_aux
        ~unify:(Env.flex_get k_unif_alg)
        ~iterate_substs:(fun substs do_eq_res ->
            (* Some (OSeq.map (CCOpt.flat_map do_eq_res) substs) *)
            let all_substs = OSeq.to_list @@ OSeq.take max_unifs @@ OSeq.filter_map CCFun.id substs   in
            let res = List.map (fun subst -> do_eq_res subst) all_substs in
            Some res)
        clause
    in
    inf_res |> CCList.flatten |> List.filter CCOpt.is_some  |> List.map CCOpt.get_exn

  (* ----------------------------------------------------------------------
   * Equality Factoring rule
   * ---------------------------------------------------------------------- *)

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
  let do_eq_factoring info =
    let open EqFactInfo in
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
      let subst_has_lams = 
        Subst.codomain subst
        |> Iter.exists (fun (t,_) -> 
            Iter.exists T.is_fun (T.Seq.subterms (T.of_term_unsafe t))) in
      let tags = (if subst_has_lams then [Proof.Tag.T_ho] else []) @ Unif_subst.tags us in
      Util.incr_stat stat_equality_factoring_call;
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

      Some new_clause
    ) else
      None

  let ext_eqfact_decompose_aux cl =
    let try_ext_eq_fact (s,t) (u,v) idx =
      let (s_hd, s_args), (u_hd, u_args) = CCPair.map_same T.as_app (s,u) in
      if not (T.equal s_hd u_hd) && Type.equal (T.ty s) (T.ty u) && 
         List.length s_args = List.length u_args &&
         List.for_all (fun (s, t) -> Term.equal s t) (CCList.combine s_args u_args) then (
        let new_lits = 
          Lit.mk_neq s_hd u_hd
          :: Lit.mk_neq t v
          :: CCArray.except_idx (C.lits cl) idx in
        let proof =
          Proof.Step.inference [C.proof_parent cl] 
            ~rule:(Proof.Rule.mk "ext_eqfact_decompose") in
        let new_c = C.create ~trail:(C.trail cl) ~penalty:(C.penalty cl) new_lits proof in
        [new_c]
      ) else [] in

    let aux_eq_rest (s,t) i lits = 
      List.mapi (fun j lit -> 
          if i < j then (
            match lit with 
            | Lit.Equation(u,v,true) ->
              try_ext_eq_fact (s,t) (u,v) i
              @
              try_ext_eq_fact (s,t) (v,u) i 
            | _ -> []
          ) else []) lits
      |> CCList.flatten in

    let lits = CCArray.to_list (C.lits cl) in
    List.mapi (fun i lit -> match lit with
        | Lit.Equation (s,t,true) ->
          aux_eq_rest (s,t) i lits
        | _ -> []) lits
    |> CCList.flatten

  let pred_var_instantiation c trigger_set =
    let p_vars = pred_vars c in
    let substs = CCList.flat_map (fun v ->
        let res = ref [] in (* no really efficient way without turning set to list *) 
        Term.Set.iter (fun t ->
            let var_ty = HVar.ty v and t_ty = Term.ty t in
            if Type.is_ground var_ty && Type.is_ground t_ty && Type.equal var_ty t_ty then (
              let subst = Subst.FO.bind' Subst.empty (v,0) (t,1) in
              res := subst :: !res;
            )) trigger_set;
        !res
      ) p_vars in
    Iter.of_list substs
    |> Iter.map (fun sub ->
        let renaming = Subst.Renaming.create() in
        let new_lits = Lits.apply_subst renaming sub (C.lits c, 0) in
        let trail = C.trail c in 
        let penalty = C.penalty c in
        let rule = Proof.Rule.mk "instantiate_w_trigger" in
        let tags = [Proof.Tag.T_ho] in
        let proof = Proof.Step.inference ~tags ~rule [C.proof_parent_subst renaming (c, 0) sub] in
        let new_clause = C.create ~trail ~penalty (CCArray.to_list new_lits) proof in
        (* CCFormat.printf "[BOOL_INST: %a, %a => %a].\n" C.pp c Subst.pp sub C.pp new_clause; *)
        assert (C.Seq.terms c |> Iter.for_all T.DB.is_closed);
        assert (C.Seq.terms new_clause |> Iter.for_all T.DB.is_closed);
        new_clause)
    |> Iter.to_list

  let instantiate_with_triggers c =
    if C.proof_depth c < Env.flex_get k_trigger_bool_inst then ( 
      pred_var_instantiation c !_trigger_bools)
    else []

  let trigger_insantiation c =
    if C.proof_depth c < Env.flex_get k_trigger_bool_inst then (
      let triggers = Term.Set.of_seq @@ get_triggers c in
      let res = ref [] in
      C.ClauseSet.iter (fun old_c -> 
          res := pred_var_instantiation old_c triggers @ !res
        ) !_cls_w_pred_vars;
      !res)
    else []


  let ext_eqfact_decompose given =
    if Proof.Step.inferences_performed (C.proof_step given)
       < Env.flex_get k_max_lits_ext_dec then  
      Util.with_prof prof_ext_dec ext_eqfact_decompose_aux given
    else []

  let infer_equality_factoring_aux ~unify ~iterate_substs clause =
    Util.enter_prof prof_infer_equality_factoring;
    let eligible = C.Eligible.(filter Lit.is_pos) in
    (* find root terms that are unifiable with s and are not in the
       literal at s_pos. Calls [k] with a position and substitution *)
    let find_unifiable_lits ~var_pred_status idx s _s_pos k =
      let is_pred_var, pred_var_sign = var_pred_status in
      Array.iteri
        (fun i lit ->
           match lit with
           | _ when i = idx -> () (* same index *)
           | Lit.Equation (u, v, true) ->
             (* positive equation *)
             if T.equal v T.false_ && not is_pred_var then ()
             else (
               if is_pred_var && T.is_true_or_false v then (
                 assert(T.is_true_or_false pred_var_sign);
                 if T.equal v pred_var_sign then (
                   k (u, v, unify (s,0) (u,0))
                 ) else (
                   let u = T.Form.not_ u in
                   k (u, pred_var_sign, unify (s,0) (u,0))
                 )
               ) else (
                 k (u, v, unify (s,0) (u,0));
                 k (v, u, unify (s,0) (v,0))
               );
             )
           | _ -> () (* ignore other literals *)
        ) (C.lits clause)
    in
    (* try to do inferences with each positive literal *)
    let new_clauses =
      Lits.fold_eqn ~sign:true ~ord ~both:true ~eligible (C.lits clause)
      |> Iter.flat_map
        (fun (s, t, _, s_pos) -> (* try with s=t *)
           let active_idx = Lits.Pos.idx s_pos in
           let is_var_pred = 
             T.is_var (T.head_term s) && Type.is_prop (T.ty s) && T.is_true_or_false t in
           if not @@ Env.flex_get k_sup_true_false && T.is_true_or_false s 
           then Iter.empty (* disable factoring from false*)
           else if T.equal t T.false_ && not is_var_pred then Iter.empty 
           else (
             let var_pred_status = (is_var_pred, t) in
             find_unifiable_lits ~var_pred_status active_idx s s_pos)
             |> Iter.filter_map
               (fun (u,v,substs) ->
                  iterate_substs substs
                    (fun subst ->
                       let info = EqFactInfo.({
                           clause; s; t; u; v; active_idx; subst; scope=0;
                         }) in
                       do_eq_factoring info)))
      |> Iter.to_rev_list
    in
    Util.exit_prof prof_infer_equality_factoring;
    new_clauses

  let infer_equality_factoring =
    infer_equality_factoring_aux
      ~unify:(fun s t -> try Some (Unif.FO.unify_full s t) with Unif.Fail -> None)
      ~iterate_substs:(fun subst do_eq_fact -> CCOpt.flat_map do_eq_fact subst)

  let infer_equality_factoring_complete_ho clause =
    let inf_res = infer_equality_factoring_aux
        ~unify:(Env.flex_get k_unif_alg)
        ~iterate_substs:(fun substs do_eq_fact -> Some (OSeq.map (CCOpt.flat_map do_eq_fact) substs))
        clause
    in
    let penalty = C.penalty clause in
    let stm_res = List.map (Stm.make ~penalty:penalty) inf_res in
    StmQ.add_lst _stmq.q stm_res; []

  let infer_equality_factoring_pragmatic_ho max_unifs clause =
    let inf_res = infer_equality_factoring_aux
        ~unify:(Env.flex_get k_unif_alg)
        ~iterate_substs:(fun substs do_eq_fact ->
            (* Some (OSeq.map (CCOpt.flat_map do_eq_fact) substs) *)
            let all_substs = OSeq.to_list @@ OSeq.take max_unifs @@ OSeq.filter_map CCFun.id substs in
            let res = List.map (fun subst -> do_eq_fact subst) all_substs in
            Some res)
        clause
    in
    inf_res |> CCList.flatten |> List.filter CCOpt.is_some  |> List.map CCOpt.get_exn

  (* ----------------------------------------------------------------------
   * extraction of a clause from the stream queue (HO feature)
   * ---------------------------------------------------------------------- *)

  let extract_from_stream_queue ~full () =
    let cl =
      if full then
        [StmQ.take_first_anyway _stmq.q]
      else
        StmQ.take_stm_nb _stmq.q
    in
    let opt_res = CCOpt.sequence_l (List.filter CCOpt.is_some cl)
    in
    match opt_res with
    | None -> []
    | Some l ->  l

  let extract_from_stream_queue_fix_stm ~full () =
    let cl =
      if full then
        [StmQ.take_first_anyway _stmq.q]
      else
        StmQ.take_stm_nb_fix_stm _stmq.q
    in
    let opt_res = CCOpt.sequence_l (List.filter CCOpt.is_some cl)
    in
    match opt_res with
    | None -> []
    | Some l -> l


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
    (* compute normal form of subterm. If restrict is true, substitutions that
       are variable renamings are forbidden (since we are at root of a max term) *)
    let rec reduce_at_root ~restrict t k =
      (* find equations l=r that match subterm *)
      let cur_sc = st.demod_sc in
      assert (cur_sc > 0);
      let step =
        UnitIdx.retrieve ~sign:true (!_idx_simpl, cur_sc) (t, 0)
        |> Iter.find_map
          (fun (l, r, (_,_,sign,unit_clause), subst) ->
             let rename = Subst.Renaming.create () in
             (* r is the term subterm is going to be rewritten into *)
             assert (C.is_unit_clause unit_clause);
             if sign &&
                not (C.equal unit_clause c) &&
                (not (Lazy.force restrict) || not (S.is_renaming subst)) &&
                C.trail_subsumes unit_clause c &&
                (O.compare ord
                   (S.FO.apply rename subst (l,cur_sc))
                   (S.FO.apply rename subst (r,cur_sc)) = Comp.Gt)
                (* subst(l) > subst(r) and restriction does not apply, we can rewrite *)
             then (
               Util.debugf ~section 3
                 "@[<hv2>demod:@ @[<hv>t=%a[%d],@ l=%a[%d],@ r=%a[%d]@],@ subst=@[%a@]@]"
                 (fun k->k T.pp t 0 T.pp l cur_sc T.pp r cur_sc S.pp subst);

               let t' = Lambda.eta_reduce @@ Lambda.snf t in
               let l' = Lambda.eta_reduce @@ Lambda.snf @@  Subst.FO.apply Subst.Renaming.none subst (l,cur_sc) in
               (* sanity checks *)
               assert (Type.equal (T.ty l) (T.ty r));
               assert (T.equal l' t');
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
          Util.debugf ~section 3
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
        (* rewrite subterms in call by value. *)
        let rewrite_args = Env.flex_get k_demod_in_var_args || not (T.is_var hd) in
        if rewrite_args
        then
          normal_form_l l
            (fun l' ->
               let t' =
                 if T.same_l l l'
                 then t
                 else T.app hd l'
               in
               (* rewrite term at root *)
               reduce_at_root ~restrict t' k)
        else reduce_at_root ~restrict t k
      | T.Fun (ty_arg, body) ->
        (* reduce under lambdas *)
        if Env.flex_get k_lambda_demod
        then
          normal_form ~restrict:lazy_false body
            (fun body' ->
               let u = if T.equal body body' then t else T.fun_ ty_arg body' in
               reduce_at_root ~restrict u k)
        else reduce_at_root ~restrict t k (* TODO: DemodExt *)
      | T.Var _ | T.DB _ -> k t
      | T.AppBuiltin (b, l) ->
        normal_form_l l
          (fun l' ->
             let u =
               if T.same_l l l' then t else T.app_builtin ~ty:(T.ty t) b l'
             in
             reduce_at_root ~restrict u k)
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
      Lit.map
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
      (* Assertion against variable clashes *)
      (* Not sure if this assertion is in place -- maybe Zip renames the vars afterwards
         TODO: INVESTIGATE!!!
      *)
      (* Lits.vars (C.lits new_c) 
         |> CCList.map (fun v -> (HVar.id v))
         |> (fun vars -> assert (CCList.length (CCList.uniq ~eq:CCInt.equal vars) == CCList.length vars)); *)
      (* return simplified clause *)
      SimplM.return_new new_c
    )

  let demodulate c = 
    assert (Term.VarSet.for_all (fun v -> HVar.id v >= 0) (Literals.vars (C.lits c) |> Term.VarSet.of_list));
    Util.with_prof prof_demodulate demodulate_ c

  let canonize_variables c =
    let all_vars = Literals.vars (C.lits c) 
                   |> (fun v -> InnerTerm.VarSet.of_list (v:>InnerTerm.t HVar.t list)) in
    let neg_vars_renaming = Subst.FO.canonize_neg_vars ~var_set:all_vars in 
    if Subst.is_empty neg_vars_renaming then SimplM.return_same c
    else (
      let new_lits = Literals.apply_subst Subst.Renaming.none neg_vars_renaming (C.lits c, 0)
                     |> CCArray.to_list in
      let proof = Proof.Step.inference [C.proof_parent c] ~rule:(Proof.Rule.mk "cannonize vars") in
      let new_c = C.create ~trail:(C.trail c) ~penalty:(C.penalty c) new_lits proof in
      SimplM.return_new new_c)

  let do_ext_dec from_c from_p from_t into_c into_p into_t = 
    let sc_f, sc_i = 0, 1 in
    let renaming = Subst.Renaming.create () in
    let (hd_f, args_f), (hd_i,args_i) = Term.as_app from_t, Term.as_app into_t in
    if Type.equal (Term.ty from_t) (Term.ty into_t) && List.length args_f = List.length args_i  &&
       not (C.id from_c = C.id into_c && Position.equal from_p into_p) then (
      (* Renaming variables apart *)
      let args_f = List.map (fun t -> Subst.FO.apply renaming Subst.empty (t,sc_f)) args_f in
      let args_i = List.map (fun t -> Subst.FO.apply renaming Subst.empty (t,sc_i)) args_i in
      let combined = CCList.combine args_f args_i in
      if T.equal hd_f hd_i && T.is_const hd_f 
         && List.for_all (fun (s,t) -> Type.equal (T.ty s) (T.ty t)) combined then (
        if List.exists (fun (s,t) ->
            (* otherwise they would be unifyible *)
            not (T.is_var (T.head_term s)) && not (T.is_var (T.head_term t))) combined then (
          let lits_f = Lits.apply_subst renaming Subst.empty (C.lits from_c, sc_f) in
          let lits_i = Lits.apply_subst renaming Subst.empty (C.lits into_c, sc_i) in

          let new_neq_lits = 
            List.map (fun (arg_f, arg_i) -> Lit.mk_neq arg_f arg_i) combined  in

          let (i, pos_f) = Lits.Pos.cut from_p in
          let from_s = Lits.Pos.at lits_f (Position.arg i (Position.opp pos_f)) in
          Lits.Pos.replace lits_i ~at:into_p ~by:from_s;
          let new_lits = new_neq_lits @ CCArray.except_idx lits_f i  @ CCArray.to_list lits_i in
          let trail = C.trail_l [from_c; into_c] in
          let penalty = max (C.penalty from_c) (C.penalty into_c) in
          let tags = [Proof.Tag.T_ho] in
          let proof =
            Proof.Step.inference
              [C.proof_parent_subst renaming (from_c, sc_f) Subst.empty;
               C.proof_parent_subst renaming  (into_c, sc_i) Subst.empty] 
              ~rule:(Proof.Rule.mk "ext_decompose") ~tags in
          let new_c = C.create ~trail ~penalty new_lits proof in
          Some new_c
        ) else None
      ) else None
    ) else None



  (* Given a "from"-clause C \/ f t1 ... tn = s  and 
     "into"-clause D \/ f u1 .. un (~)= v, where some of the t_i 
     (and consequently u_i) are of functional type, construct
     a clause C \/ D \/ t1 ~= u1 \/ ... tn ~= un \/ s (~)= v.

     Intuitively, we are waiting for efficient extensionality rules
     to kick in and fix the problem of not being able to paramodulate
     with this equation.

     Currently with no restrictions or indexing. After initial evaluation,
     will find ways to restrict it somehow. *)
  let retrieve_from_extdec_idx idx id = 
    let cl_map = ID.Map.find_opt id idx in
    match cl_map with
    | None -> Iter.empty
    | Some cl_map -> 
      C.Tbl.to_seq cl_map 
      |> Iter.flat_map (fun (c, l) -> 
          Iter.of_list l
          |> Iter.map (fun (t,p) -> (c,t,p)))

  let ext_decompose_act given =
    if C.length given <= Env.flex_get k_max_lits_ext_dec then (
      let eligible = 
        if Env.flex_get k_ext_dec_lits = `OnlyMax then C.Eligible.param given else C.Eligible.always in
      Lits.fold_eqn ~ord ~both:true ~sign:true ~eligible (C.lits given)
      |> Iter.flat_map (fun (l,_,sign,pos) ->
          let hd,args = T.as_app l in
          if T.is_const hd && T.has_ho_subterm l then (
            let inf_partners = retrieve_from_extdec_idx !_ext_dec_into_idx (T.as_const_exn hd) in
            Iter.map (fun (into_c,into_t, into_p) -> 
                do_ext_dec given pos l into_c into_p into_t) inf_partners) 
          else Iter.empty)
      |> Iter.filter_map CCFun.id
      |> Iter.to_list)
    else []

  let ext_decompose_pas given =
    if C.length given <= Env.flex_get k_max_lits_ext_dec then ( 
      let which, eligible =
        if Env.flex_get k_ext_dec_lits = `OnlyMax then `Max, C.Eligible.res given 
        else `All, C.Eligible.always in
      Lits.fold_terms ~vars:false ~var_args:false ~fun_bodies:false ~ty_args:false 
        ~ord ~which ~subterms:true ~eligible (C.lits given)
      |> Iter.flat_map (fun (t,p) ->
          let hd, args = T.as_app t in
          if T.is_const hd && T.has_ho_subterm t  then (
            let inf_partners = retrieve_from_extdec_idx !_ext_dec_from_idx (T.as_const_exn hd) in
            Iter.map (fun (from_c,from_t, from_p) -> 
                do_ext_dec from_c from_p from_t given p t) inf_partners) 
          else Iter.empty))
      |> Iter.filter_map CCFun.id
      |> Iter.to_list
    else []

  let ext_eqres_decompose_aux c =
    let eligible = C.Eligible.neg in
    if C.proof_depth c < Env.flex_get k_max_lits_ext_dec then (
      let res = 
        Literals.fold_eqn (C.lits c) ~eligible ~ord ~both:false ~sign:false
        |> Iter.to_list
        |> CCList.filter_map (fun (lhs,rhs,sign,pos) ->
            assert(sign = false);
            let (l_hd, l_args), (r_hd, r_args) = CCPair.map_same T.as_app (lhs,rhs) in
            if not (T.equal l_hd r_hd) && List.length l_args = List.length r_args &&
               List.for_all (fun (s, t) -> Type.equal (Term.ty s) (Term.ty t)) (CCList.combine l_args r_args) &&
               (List.for_all (fun (s, t) -> Term.equal s t) (CCList.combine l_args r_args)) then (
              let new_neq_lits = 
                ((l_hd,r_hd) :: CCList.combine l_args r_args) 
                |> CCList.filter_map (fun (arg_f, arg_i) -> 
                    if not (T.equal arg_f arg_i) then Some (Lit.mk_neq arg_f arg_i)
                    else None) in
              let i, _ = Literals.Pos.cut pos in
              let new_lits = new_neq_lits @ CCArray.except_idx (C.lits c) i in
              let proof =
                Proof.Step.inference [C.proof_parent c] 
                  ~rule:(Proof.Rule.mk "ext_eqres_decompose") in
              let new_c = C.create ~trail:(C.trail c) ~penalty:(C.penalty c) new_lits proof in
              Some new_c) 
            else None) in
      Util.incr_stat stat_ext_dec;
      res
    ) else []

  let ext_eqres_decompose given = 
    Util.with_prof prof_ext_dec ext_eqres_decompose_aux given

  (** Find clauses that [given] may demodulate, add them to set *)
  let backward_demodulate set given =
    Util.enter_prof prof_back_demodulate;
    let renaming = Subst.Renaming.create () in
    (* find clauses that might be rewritten by l -> r *)
    let recurse ~oriented set l r =
      I.retrieve_specializations (!_idx_back_demod,1) (l,0)
      |> Iter.fold
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
           (* registering equations of the form ~ P as negative equations P = true *)
           | Lit.Equation (p, t, true) when T.equal t T.false_ ->
             Congruence.FO.mk_eq cc p T.true_
           | _ -> cc)
        cc (C.lits c)
    in
    let res = CCArray.exists
        (function
          (* making sure we do not catch equations of the form P = false
             that are interpreted as negative equations P = true *)
          | Lit.Equation (l, r, true) when not (T.equal r T.false_) ->
            (* if l=r is implied by the congruence, then the clause is redundant *)
            Congruence.FO.is_eq cc l r
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
        let new_lits = if List.exists Lit.is_trivial new_lits then [Lit.mk_tauto] else new_lits in
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
    (* try to make the terms equal using some positive unit clauses
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
    (* try to equate terms with a positive unit clause that match them *)
    and equate_root clauses t1 t2 =
      try
        UnitIdx.retrieve ~sign:true (!_idx_simpl,1)(t1,0)
        |> Iter.iter
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
    (* try to remove the literal using a negative unit clause *)
    and can_refute s t =
      try
        UnitIdx.retrieve ~sign:false (!_idx_simpl,1) (s,0)
        |> Iter.iter
          (fun (l, r, (_,_,_,c'), subst) ->
             assert (Unif.FO.equal ~subst (l, 1) (s, 0));
             Util.debugf ~section 3 "@[neg_reflect trying to eliminate@ @[%a=%a@]@ with @[%a@]@]"
               (fun k->k T.pp s T.pp t C.pp c');
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
      Util.debug ~section 3 "@[neg_reflect did not simplify the clause @]";
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
              not (Iter.is_empty (Lit.subsumes (lita, sc_a) (litb, sc_b))))
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
    let (c_a, _), (c_b, _) = a,b in
    let w_a = CCArray.fold (fun acc l -> acc + Lit.weight l) 0 c_a in
    let w_b = CCArray.fold (fun acc l -> acc + Lit.weight l) 0 c_b in

    if w_a = w_b && Literals.equal_com c_a c_b then Some (Subst.empty, [])
    else (
      let res = if w_a <= w_b then subsumes_with_ a b else None in
      Util.exit_prof prof_subsumption;
      res
    )

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
          if Term.size a > Term.size u || Term.size b > Term.size v then
            raise Unif.Fail;
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
    let c = if Env.flex_get k_ground_subs_check > 0 then  C.ground_clause c else c in
    let subsumes a b = 
      if not @@ Env.flex_get k_solid_subsumption then subsumes a b else (
        try 
          SS.subsumes a b
        with SolidSubsumption.UnsupportedLiteralKind -> 
          subsumes a b
      ) in
    let res =
      SubsumIdx.retrieve_subsuming_c !_idx_fv c
      |> Iter.exists
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
      Util.debugf ~section 1 "@[<2>@[%a@]@ subsumed by active set@]" (fun k->k C.pp c);
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
    let subsumes a b = 
      if not @@ Env.flex_get k_solid_subsumption then subsumes a b else (
        try 
          SS.subsumes a b
        with SolidSubsumption.UnsupportedLiteralKind -> 
          subsumes a b
      ) in
    let res =
      SubsumIdx.retrieve_subsumed_c !_idx_fv c
      |> Iter.fold
        (fun res c' ->
           if C.trail_subsumes c c'
           then
             let c' = if Env.flex_get k_ground_subs_check > 1 then  C.ground_clause c' else c' in
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
        Iter.of_array_i lits
        |> Iter.filter (fun (_,lit) -> not (Lit.is_constraint lit))
        |> Iter.find_map
          (fun (i,old_lit) ->
             (* negate literal *)
             lits.(i) <- Lit.negate old_lit;
             (* test for subsumption *)
             SubsumIdx.retrieve_subsuming !_idx_fv
               (Lits.Seq.to_form lits) (C.trail c |> Trail.labels)
             |> Iter.filter (fun c' -> C.trail_subsumes c' c)
             |> Iter.find_map
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
              |> Iter.map (fun s -> s, i)
            and subst_remove_lit' =
              Lit.subsumes (lit', 0) (lit, 0)
              |> Iter.map (fun s -> s, j)
            in
            (* potential condensing substitutions *)
            let substs = Iter.append subst_remove_lit subst_remove_lit' in
            Iter.iter
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

  let recognize_injectivity c =
    if C.length c == 2 then (
      let pos_lit = CCArray.find_idx Lit.is_pos (C.lits c) in
      let neg_lit = CCArray.find_idx Lit.is_neg (C.lits c) in
      try 
        let _,pos_lit = CCOpt.get_exn pos_lit in
        let _,neg_lit = CCOpt.get_exn neg_lit in
        match pos_lit with 
        | Equation(x, y, true) ->
          if Term.is_var x && Term.is_var y && not (Term.equal x y) then (
            match neg_lit with 
            | Equation(l,r,false) ->
              let hd_l,hd_r = CCPair.map_same Term.head_term (l,r) in
              if Term.is_const hd_l && Term.is_const hd_r 
                 && Term.equal hd_l hd_r then (
                let covered_l = Term.max_cover l [Some x] in 
                let covered_r = Term.max_cover r [Some y] in
                if Term.equal covered_l covered_r then (
                  let ty = Type.arrow [Term.ty l] (Term.ty x) in
                  let sk_vars = Term.vars covered_l |> Term.VarSet.to_list in
                  let (sk_id, sk_ty), sk_term = Term.mk_fresh_skolem sk_vars ty in
                  let inverse_x = Term.app sk_term [l] in
                  let inverse_lit = [Lit.mk_eq inverse_x x] in
                  let proof = Proof.Step.inference ~rule:(Proof.Rule.mk "inverse recognition") 
                      [C.proof_parent c] in
                  Ctx.declare sk_id sk_ty;
                  let new_clause = C.create ~trail:(C.trail c) ~penalty:(C.penalty c) inverse_lit proof in
                  Util.debugf ~section 1 "Injectivity recognized: %a |---| %a" (fun k -> k C.pp c C.pp new_clause);
                  (* Format.printf "Injectivity recognized: %a |---| %a" C.pp c C.pp new_clause; *)

                  [new_clause]
                ) else []
              ) else []
            | _ -> [] (* predicate *)
          ) else []
        | _ -> assert false
      with Invalid_argument _ -> []
    ) else []

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
    @@ Env.flex_get k_dot_sup_into;
    CCOpt.iter
      (fun file ->
         Signal.once Signals.on_dot_output
           (fun () -> _print_idx ~f:(TermIndex.to_dot pp_leaf) file !_idx_sup_from))
    @@ Env.flex_get k_dot_sup_from;
    CCOpt.iter
      (fun file ->
         Signal.once Signals.on_dot_output
           (fun () -> _print_idx ~f:UnitIdx.to_dot file !_idx_simpl))
    @@ Env.flex_get k_dot_simpl;
    CCOpt.iter
      (fun file ->
         Signal.once Signals.on_dot_output
           (fun () -> _print_idx ~f:(TermIndex.to_dot pp_leaf) file !_idx_back_demod))
    @@ Env.flex_get k_dot_demod_into;
    ()

  let register () =
    let open SimplM.Infix in
    let rw_simplify c =
      canonize_variables c
      >>= demodulate
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

    if not @@ Env.flex_get k_sup_w_pure_vars then (
      Env.Ctx.lost_completeness ()
    );

    if Env.flex_get k_recognize_injectivity then (
      Env.add_unary_inf "recognize injectivity" recognize_injectivity;
    );

    if Env.flex_get k_max_lits_ext_dec != 0 then (
      Env.add_binary_inf "ext_dec_act" ext_decompose_act;
      Env.add_binary_inf "ext_dec_pas" ext_decompose_pas;
      Env.add_unary_inf "ext_eqres_dec" ext_eqres_decompose;
      Env.add_unary_inf "ext_eqfact_dec" ext_eqfact_decompose;
    );

    if Env.flex_get k_complete_ho_unification
    then (
      if (Env.flex_get k_max_infs) = -1 then (
        Env.add_binary_inf "superposition_passive" infer_passive_complete_ho;
        Env.add_binary_inf "superposition_active" infer_active_complete_ho;
        Env.add_unary_inf "equality_factoring" infer_equality_factoring_complete_ho;
        Env.add_unary_inf "equality_resolution" infer_equality_resolution_complete_ho;
      )
      else (
        assert((Env.flex_get k_max_infs) > 0);
        Env.add_binary_inf "superposition_passive" (infer_passive_pragmatic_ho (Env.flex_get k_max_infs));
        Env.add_binary_inf "superposition_active" (infer_active_pragmatic_ho (Env.flex_get k_max_infs));
        Env.add_unary_inf "equality_factoring" (infer_equality_factoring_pragmatic_ho (Env.flex_get k_max_infs));
        Env.add_unary_inf "equality_resolution" (infer_equality_resolution_pragmatic_ho (Env.flex_get k_max_infs));
      );

      if Env.flex_get k_fluidsup then (
        Env.add_binary_inf "fluidsup_passive" infer_fluidsup_passive;
        Env.add_binary_inf "fluidsup_active" infer_fluidsup_active;
      );
      if Env.flex_get k_dupsup then (
        Env.add_binary_inf "dupsup_passive(into)" infer_dupsup_passive;
        Env.add_binary_inf "dupsup_active(from)" infer_dupsup_active;
      );
      if Env.flex_get k_lambdasup != -1 then (
        Env.add_binary_inf "lambdasup_active(from)" infer_lambdasup_from;
        Env.add_binary_inf "lambdasup_passive(into)" infer_lambdasup_into;
      );
      if Env.flex_get k_trigger_bool_inst > 0 then (
        Env.add_unary_inf "trigger_pred_var active" trigger_insantiation;
        Env.add_unary_inf "trigger_pred_var passive" instantiate_with_triggers;
      );


      if (List.exists CCFun.id [Env.flex_get k_fluidsup;
                                Env.flex_get k_dupsup;
                                Env.flex_get k_lambdasup != -1;
                                Env.flex_get k_max_infs = -1]) then (
        if Env.flex_get k_switch_stream_extraction then
          Env.add_generate "stream_queue_extraction" extract_from_stream_queue_fix_stm
        else
          Env.add_generate "stream_queue_extraction" extract_from_stream_queue;
      )
    )
    else (
      Env.add_binary_inf "superposition_passive" infer_passive;
      Env.add_binary_inf "superposition_active" infer_active;
      Env.add_unary_inf "equality_factoring" infer_equality_factoring;
      Env.add_unary_inf "equality_resolution" infer_equality_resolution;
    );
    if not (Env.flex_get k_dont_simplify) then (
      Env.add_rw_simplify rw_simplify;
      Env.add_basic_simplify canonize_variables;
      Env.add_basic_simplify basic_simplify;
      Env.add_active_simplify active_simplify;
      Env.add_backward_simplify backward_simplify
    );
    Env.add_redundant redundant;
    Env.add_backward_redundant backward_redundant;
    if Env.flex_get k_use_semantic_tauto
    then Env.add_is_trivial is_semantic_tautology;
    Env.add_is_trivial is_trivial;
    Env.add_lit_rule "distinct_symbol" handle_distinct_constants;
    setup_dot_printers ();
    ()
end

let _use_semantic_tauto = ref true
let _use_simultaneous_sup = ref true
let _dot_sup_into = ref None
let _dot_sup_from = ref None
let _dot_simpl = ref None
let _dont_simplify = ref false
let _sup_at_vars = ref false
let _sup_at_var_headed = ref true
let _sup_in_var_args = ref true
let _sup_under_lambdas = ref true
let _lambda_demod = ref false
let _demod_in_var_args = ref true
let _dot_demod_into = ref None
let _complete_ho_unification = ref false
let _switch_stream_extraction = ref false
let _ord_in_normal_form = ref false
let _fluidsup_penalty = ref 0
let _fluidsup = ref true
let _dupsup = ref true
let _trigger_bool_inst = ref (-1)
let _recognize_injectivity = ref false
let _sup_with_pure_vars = ref true
let _restrict_fluidsup = ref false

let _lambdasup = ref (-1)
let _max_infs = ref (-1)
let max_lits_ext_dec = ref 0
let _ext_dec_lits = ref `OnlyMax
let _unif_alg = ref `NewJPFull
let _unif_level = ref `Full
let _ground_subs_check = ref 0
let _sup_t_f = ref true
let _solid_subsumption = ref false

let _skip_multiplier = ref 35.0
let _imit_first = ref false
let _max_depth = ref 3
let _max_rigid_imitations = ref 3
let _max_app_projections = ref 1
let _max_elims = ref 1
let _max_identifications = ref 1
let _pattern_decider = ref true
let _fixpoint_decider = ref false
let _solid_decider = ref false
let _solidification_limit = ref 5
let _max_unifs_solid_ff = ref 20
let _use_weight_for_solid_subsumption = ref false

let key = Flex_state.create_key ()

let unif_params_to_def () =
  _max_depth := 2;
  _max_app_projections := 1;
  _max_rigid_imitations := 1;
  _max_identifications := 1;
  _max_elims           := 1;
  _max_infs := -1 

let register ~sup =
  let module Sup = (val sup : S) in
  let module E = Sup.Env in

  E.update_flex_state (Flex_state.add key sup);
  E.flex_add k_trigger_bool_inst !_trigger_bool_inst;
  E.flex_add k_sup_at_vars !_sup_at_vars;
  E.flex_add k_sup_in_var_args !_sup_in_var_args;
  E.flex_add k_sup_under_lambdas !_sup_under_lambdas;
  E.flex_add k_sup_true_false !_sup_t_f;
  E.flex_add k_sup_at_var_headed !_sup_at_var_headed;
  E.flex_add k_fluidsup !_fluidsup;
  E.flex_add k_dupsup !_dupsup;
  E.flex_add k_lambdasup !_lambdasup;
  E.flex_add k_sup_w_pure_vars !_sup_with_pure_vars;
  E.flex_add k_restrict_fluidsup !_restrict_fluidsup;
  E.flex_add k_demod_in_var_args !_demod_in_var_args;
  E.flex_add k_lambda_demod !_lambda_demod;
  E.flex_add k_ext_dec_lits !_ext_dec_lits;
  E.flex_add k_max_lits_ext_dec !max_lits_ext_dec;
  E.flex_add k_use_simultaneous_sup !_use_simultaneous_sup;  
  E.flex_add k_fluidsup_penalty !_fluidsup_penalty;
  E.flex_add k_ground_subs_check !_ground_subs_check;
  E.flex_add k_solid_subsumption !_solid_subsumption;
  E.flex_add k_dot_sup_into !_dot_sup_into;
  E.flex_add k_dot_sup_from !_dot_sup_from;
  E.flex_add k_dot_simpl !_dot_simpl;
  E.flex_add k_dot_demod_into !_dot_demod_into;
  E.flex_add k_recognize_injectivity !_recognize_injectivity;
  E.flex_add k_complete_ho_unification !_complete_ho_unification;
  E.flex_add k_max_infs !_max_infs;
  E.flex_add k_switch_stream_extraction !_switch_stream_extraction;
  E.flex_add k_dont_simplify !_dont_simplify;
  E.flex_add k_use_semantic_tauto !_use_semantic_tauto;

  E.flex_add PragUnifParams.k_max_inferences !_max_infs;
  E.flex_add PragUnifParams.k_skip_multiplier !_skip_multiplier;
  E.flex_add PragUnifParams.k_imit_first !_imit_first;
  E.flex_add PragUnifParams.k_max_depth !_max_depth;
  E.flex_add PragUnifParams.k_max_rigid_imitations !_max_rigid_imitations;
  E.flex_add PragUnifParams.k_max_app_projections !_max_app_projections;
  E.flex_add PragUnifParams.k_max_elims !_max_elims;
  E.flex_add PragUnifParams.k_max_identifications !_max_identifications;
  E.flex_add PragUnifParams.k_pattern_decider !_pattern_decider;
  E.flex_add PragUnifParams.k_fixpoint_decider !_fixpoint_decider;
  E.flex_add PragUnifParams.k_solid_decider !_solid_decider;
  E.flex_add PragUnifParams.k_solidification_limit !_solidification_limit;
  E.flex_add PragUnifParams.k_max_unifs_solid_ff !_max_unifs_solid_ff;
  E.flex_add PragUnifParams.k_use_weight_for_solid_subsumption !_use_weight_for_solid_subsumption;

  let module JPF = JPFull.Make(struct let st = E.flex_state () end) in
  let module JPP = PUnif.Make(struct let st = E.flex_state () end) in
  begin match !_unif_alg with 
    | `OldJP -> E.flex_add k_unif_alg JP_unif.unify_scoped
    | `NewJPFull -> E.flex_add k_unif_alg JPF.unify_scoped
    | `NewJPPragmatic -> E.flex_add k_unif_alg JPP.unify_scoped end




(* TODO: move DOT index printing into the extension *)

let extension =
  let action env =
    let module E = (val env : Env.S) in
    let module Sup = Make(E) in
    register ~sup:(module Sup : S);
    Sup.register();
  in
  { Extensions.default with Extensions.
                         name="superposition";
                         env_actions = [action];
  }

let () =
  Params.add_opts
    [
      "--semantic-tauto", Arg.Bool (fun v -> _use_semantic_tauto := v), " enable/disable semantic tautology check"; 
      "--dot-sup-into", Arg.String (fun s -> _dot_sup_into := Some s), " print superposition-into index into file";
      "--dot-sup-from", Arg.String (fun s -> _dot_sup_from := Some s), " print superposition-from index into file";
      "--dot-demod", Arg.String (fun s -> _dot_simpl := Some s), " print forward rewriting index into file";
      "--dot-demod-into", Arg.String (fun s -> _dot_demod_into := Some s), " print backward rewriting index into file"; 
      "--simultaneous-sup", Arg.Bool (fun b -> _use_simultaneous_sup := b), " enable/disable simultaneous superposition";
      "--dont-simplify", Arg.Set _dont_simplify, " disable simplification rules";
      "--sup-at-vars", Arg.Bool (fun v -> _sup_at_vars := v), " enable/disable superposition at variables under certain ordering conditions";
      "--sup-at-var-headed", Arg.Bool (fun b -> _sup_at_var_headed := b), " enable/disable superposition at variable headed terms";
      "--sup-in-var-args", Arg.Bool (fun b -> _sup_in_var_args := b), " enable/disable superposition in arguments of applied variables";
      "--sup-under-lambdas", Arg.Bool (fun b -> _sup_under_lambdas := b), " enable/disable superposition in bodies of lambda-expressions";
      "--lambda-demod", Arg.Bool (fun b -> _lambda_demod := b), " enable/disable demodulation in bodies of lambda-expressions";
      "--demod-in-var-args", Arg.Bool (fun b -> _demod_in_var_args := b), " enable demodulation in arguments of variables";
      "--complete-ho-unif", Arg.Bool (fun b -> _complete_ho_unification := b), " enable complete higher-order unification algorithm (Jensen-Pietrzykowski)";
      "--switch-stream-extract", Arg.Bool (fun b -> _switch_stream_extraction := b), " in ho mode, switches heuristic of clause extraction from the stream queue";
      "--ord-in-normal-form", Arg.Bool (fun v -> _ord_in_normal_form := v), " compare intermediate terms in calculus rules in beta-normal-eta-long form";
      "--ext-decompose", Arg.Set_int max_lits_ext_dec, " Sets the maximal number of literals clause can have for ExtDec inference.";
      "--ext-decompose-lits", Arg.Symbol (["all";"max"], (fun str -> 
          _ext_dec_lits := if String.equal str "all" then `All else `OnlyMax))
      , " Sets the maximal number of literals clause can have for ExtDec inference.";
      "--fluidsup-penalty", Arg.Int (fun p -> _fluidsup_penalty := p), " penalty for FluidSup inferences";
      "--fluidsup", Arg.Bool (fun b -> _fluidsup :=b), " enable/disable FluidSup inferences (only effective when complete higher-order unification is enabled)";
      "--lambdasup", Arg.Int (fun l -> 
          if l < 0 then 
            raise (Util.Error ("argument parsing", 
                               "lambdaSup argument should be non-negative"));
          _lambdasup := l), 
      " enable LambdaSup -- argument is the maximum number of skolems introduced in an inference";
      "--dupsup", Arg.Bool (fun v -> _dupsup := v), " enable/disable DupSup inferences";
      "--ground-before-subs", Arg.Set_int _ground_subs_check, " set the level of grounding before substitution. 0 - no grounding. 1 - only active. 2 - both.";
      "--solid-subsumption", Arg.Bool (fun v -> _solid_subsumption := v), " set solid subsumption on or off";
      "--recognize-injectivity", Arg.Bool (fun v -> _recognize_injectivity := v), " recognize injectivity axiom and axiomatize corresponding inverse";
      "--sup-with-pure-vars" , Arg.Bool (fun v -> _sup_with_pure_vars := v), " enable/disable superposition to and from pure variable equations";
      "--restrict-fluidsup" , Arg.Bool (fun v -> _restrict_fluidsup := v), " enable/disable restriction of fluidSup to up to two literal or inital clauses";
      "--sup-with-true-false", Arg.Bool (fun v ->( _sup_t_f := v)), " enable/disable superposition, eq-res and eq-fact with true/false";
      "--use-weight-for-solid-subsumption", Arg.Bool (fun v -> _use_weight_for_solid_subsumption := v), 
      " enable/disable superposition to and from pure variable equations";
      "--trigger-bool-inst", Arg.Set_int _trigger_bool_inst
      , " instantiate predicate variables with boolean terms already in the proof state. Argument is the maximal proof depth of predicate variable";
      "--ho-unif-level",
      Arg.Symbol (["full-framework";"full"; "pragmatic-framework";], (fun str ->
          _unif_alg := if (String.equal "full" str) then `OldJP
            else if (String.equal "full-framework" str) then (
              unif_params_to_def ();
              `NewJPFull)
            else if (String.equal "pragmatic-framework" str) then `NewJPPragmatic
            else invalid_arg "unknown argument")), "set the level of HO unification";
      "--ho-imitation-first",Arg.Bool (fun v -> _imit_first:=v), " Use imitation rule before projection rule";
      "--ho-unif-max-depth", Arg.Set_int _max_depth, " set pragmatic unification max depth";
      "--ho-max-app-projections", Arg.Set_int _max_app_projections, " set maximal number of functional type projections";
      "--ho-max-elims", Arg.Set_int _max_elims, " set maximal number of eliminations";
      "--ho-max-identifications", Arg.Set_int _max_identifications, " set maximal number of flex-flex identifications";
      "--ho-skip-multiplier", Arg.Set_float _skip_multiplier, " set maximal number of flex-flex identifications";
      "--ho-max-rigid-imitations", Arg.Set_int _max_rigid_imitations, " set maximal number of rigid imitations";
      "--ho-max-solidification", Arg.Set_int _solidification_limit, " set maximal number of rigid imitations";
      "--ho-max-unifs-solid-flex-flex", Arg.Set_int _max_unifs_solid_ff, " set maximal number of found unifiers for solid flex-flex pairs. -1 stands for finding the MGU";
      "--ho-pattern-decider", Arg.Bool (fun b -> _pattern_decider := b), "turn pattern decider on or off";
      "--ho-solid-decider", Arg.Bool (fun b -> _solid_decider := b), "turn solid decider on or off";
      "--ho-fixpoint-decider", Arg.Bool (fun b -> _fixpoint_decider := b), "turn fixpoint decider on or off";
      "--max-inferences", Arg.Int (fun p -> _max_infs := p), " set maximal number of inferences"];
  Params.add_to_mode "ho-complete-basic" (fun () ->
      _use_simultaneous_sup := false;
      _sup_at_vars := true;
      _sup_in_var_args := false;
      _sup_under_lambdas := false;
      _lambda_demod := false;
      _demod_in_var_args := false;
      _complete_ho_unification := true;
      _ord_in_normal_form := true;
      _sup_at_var_headed := false;
      _unif_alg := `NewJPFull;
      _lambdasup := -1;
      _dupsup := false;
    );
  Params.add_to_mode "ho-pragmatic" (fun () ->
      _use_simultaneous_sup := false;
      _sup_at_vars := true;
      _sup_in_var_args := false;
      _sup_under_lambdas := false;
      _lambda_demod := false;
      _demod_in_var_args := false;
      _complete_ho_unification := true;
      _unif_alg := `NewJPPragmatic;
      _ord_in_normal_form := true;
      _sup_at_var_headed := true;
      _lambdasup := -1;
      _dupsup := false;
      _max_infs := 5;
      _max_depth := 3;
      _max_app_projections := 0;
      _max_identifications := 1;
      _max_elims := 1;
      _fluidsup := false;
    );
  Params.add_to_mode "ho-competitive" (fun () ->
      _use_simultaneous_sup := false;
      _sup_at_vars := true;
      _sup_in_var_args := false;
      _sup_under_lambdas := false;
      _lambda_demod := false;
      _demod_in_var_args := false;
      _complete_ho_unification := true;
      _unif_alg := `NewJPFull;
      _ord_in_normal_form := true;
      _sup_at_var_headed := true;
      _lambdasup := -1;
      _dupsup := false;
      _max_infs := 10;
      _max_depth := 6;
      _max_app_projections := 1;
      _max_identifications := 1;
      _max_elims := 1;
      _fluidsup := false;
    );
  Params.add_to_mode "fo-complete-basic" (fun () ->
      _use_simultaneous_sup := false;
    )
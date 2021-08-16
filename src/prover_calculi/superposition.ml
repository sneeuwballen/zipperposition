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
module HO = Higher_order

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
let stat_orphan_checks = Util.mk_stat "orphan checks"


let prof_demodulate = ZProf.make "sup.demodulate"
let prof_back_demodulate = ZProf.make "sup.backward_demodulate"
let prof_pos_simplify_reflect = ZProf.make "sup.simplify_reflect+"
let prof_neg_simplify_reflect = ZProf.make "sup.simplify_reflect-"
let prof_clc = ZProf.make "sup.contextual_literal_cutting"
let prof_semantic_tautology = ZProf.make "sup.semantic_tautology"
let prof_condensation = ZProf.make "sup.condensation"
let prof_basic_simplify = ZProf.make "sup.basic_simplify"
let prof_subsumption = ZProf.make "sup.subsumption"
let prof_eq_subsumption = ZProf.make "sup.equality_subsumption"
let prof_subsumption_set = ZProf.make "sup.forward_subsumption"
let prof_subsumption_in_set = ZProf.make "sup.backward_subsumption"
let prof_infer_active = ZProf.make "sup.infer_active"
let prof_infer_passive = ZProf.make "sup.infer_passive"
let prof_infer_fluidsup_active = ZProf.make "sup.infer_fluidsup_active"
let prof_infer_fluidsup_passive = ZProf.make "sup.infer_fluidsup_passive"
let prof_infer_equality_resolution = ZProf.make "sup.infer_equality_resolution"
let prof_infer_equality_factoring = ZProf.make "sup.infer_equality_factoring"
let prof_queues = ZProf.make "sup.queues"

let k_sup_at_vars = Flex_state.create_key ()
let k_sup_in_var_args = Flex_state.create_key ()
let k_sup_under_lambdas = Flex_state.create_key ()
let k_sup_at_var_headed = Flex_state.create_key ()
let k_sup_from_var_headed = Flex_state.create_key ()
let k_fluidsup = Flex_state.create_key ()
let k_subvarsup = Flex_state.create_key ()
let k_dupsup = Flex_state.create_key ()
let k_lambdasup = Flex_state.create_key ()
let k_demod_in_var_args = Flex_state.create_key ()
let k_lambda_demod = Flex_state.create_key ()
let k_quant_demod = Flex_state.create_key ()
let k_use_simultaneous_sup = Flex_state.create_key ()
let k_unif_alg = Flex_state.create_key ()
let k_unif_module : (module UnifFramework.US) Flex_state.key = Flex_state.create_key ()
let k_fluidsup_penalty = Flex_state.create_key ()
let k_dupsup_penalty = Flex_state.create_key ()
let k_ground_subs_check = Flex_state.create_key ()
let k_solid_subsumption = Flex_state.create_key ()
let k_dot_sup_into = Flex_state.create_key ()
let k_dot_sup_from = Flex_state.create_key ()
let k_dot_simpl = Flex_state.create_key ()
let k_dot_demod_into = Flex_state.create_key ()
let k_recognize_injectivity = Flex_state.create_key ()
let k_ho_basic_rules = Flex_state.create_key ()
let k_max_infs = Flex_state.create_key ()
let k_switch_stream_extraction = Flex_state.create_key ()
let k_dont_simplify = Flex_state.create_key ()
let k_use_semantic_tauto = Flex_state.create_key ()
let k_restrict_fluidsup = Flex_state.create_key ()
let k_check_sup_at_var_cond = Flex_state.create_key ()
let k_restrict_hidden_sup_at_vars = Flex_state.create_key ()
let k_bool_demod = Flex_state.create_key ()
let k_immediate_simplification = Flex_state.create_key ()
let k_local_rw = Flex_state.create_key ()
let k_destr_eq_res = Flex_state.create_key ()
let k_rw_with_formulas = Flex_state.create_key ()
let k_pred_var_eq_fact = Flex_state.create_key ()
let k_force_limit = Flex_state.create_key ()
let k_formula_simplify_reflect = Flex_state.create_key ()
let k_strong_sr = Flex_state.create_key ()
let k_superpose_w_formulas = Flex_state.create_key ()



let _NO_LAMSUP = -1

let get_unif_module (module E : Env.S) : (module UnifFramework.US) = E.flex_get k_unif_module



module Make(Env : Env.S) : S with module Env = Env = struct
  module Env = Env
  module Ctx = Env.Ctx
  module C = Env.C
  module PS = Env.ProofState
  module I = PS.TermIndex
  module TermIndex = PS.TermIndex
  module SubsumIdx = PS.SubsumptionIndex
  module UnitIdx = PS.UnitIndex
  module Stm = Env.Stm
  module StmQ = Env.StmQ

  (** {6 Stream queue} *)
  let _cc_simpl = ref (Congruence.FO.create ~size:256 ())


  (** {6 Index Management} *)

  let _idx_sup_into = ref (TermIndex.empty ())
  let _idx_lambdasup_into = ref (TermIndex.empty ())
  let _idx_fluidsup_into = ref (TermIndex.empty ())
  let _idx_subvarsup_into = ref (TermIndex.empty ())
  let _idx_dupsup_into = ref (TermIndex.empty ())
  let _idx_sup_from = ref (TermIndex.empty ())
  let _idx_back_demod = ref (TermIndex.empty ())
  let _idx_fv = ref (SubsumIdx.empty ())
  (* let _idx_fv = ref (SubsumIdx.of_signature (Ctx.signature()) ()) *)

  let _idx_simpl = ref (UnitIdx.empty ())
  
  let idx_sup_into () = !_idx_sup_into
  let idx_sup_from () = !_idx_sup_from
  let idx_fv () = !_idx_fv

  (* Beta-Eta-Normalizes terms before comparing them. Note that the Clause
     module calls Ctx.ord () independently, but clauses should be normalized
     independently by simplification rules. *) 
  let ord =
    Ctx.ord ()

  let force_getting_cl streams =    
    let rec aux ((clauses, streams) as acc) = function 
      | [] -> acc
      | (penalty, parents, stm) :: xs ->
          let rec drip_stream i stm =
            let mk_stm stm = Stm.make ~penalty ~parents stm in
            if i = 0 then aux (clauses, (mk_stm stm) :: streams) xs
            else (
              match stm() with
              | OSeq.Nil -> aux acc xs
              | OSeq.Cons((Some cl), stm') ->
                aux (cl::clauses, (mk_stm stm') :: streams) xs
              | OSeq.Cons(None, stm') ->
                drip_stream (i-1) stm'
            )
          in

          (* let limit = max 1 ((Env.flex_get StreamQueue.k_guard) / ) in *)
          let limit = Env.flex_get k_force_limit in
          drip_stream limit stm
    in
    aux ([], []) streams
  
  let has_bad_occurrence_elsewhere c var pos =
    assert(T.is_var var);
    Lits.fold_terms ~ord ~subterms:true ~eligible:C.Eligible.always ~which:`All
      (C.lits c)
    |> Iter.exists (fun (t, pos') -> 
      not (Position.equal pos pos') &&
      (* variable appears at a prefix position
        somewhere else (pos ≠ pos') *)
      match T.view t with
      | T.App(hd, _) -> T.equal hd var
      | _ -> false
    )

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
    let sup_at_var_headed = Env.flex_get k_sup_at_var_headed in
    let sup_from_var_headed = Env.flex_get k_sup_from_var_headed in
    let fluidsup = Env.flex_get k_fluidsup in
    let subvarsup = Env.flex_get k_subvarsup in
    let dupsup = Env.flex_get k_dupsup in
    let lambdasup = Env.flex_get k_lambdasup in
    let demod_in_var_args = Env.flex_get k_demod_in_var_args in
    let lambda_demod = Env.flex_get k_lambda_demod in
    let module TPSet = SClause.TPSet in


    _idx_sup_into :=
      Lits.fold_terms ~vars:sup_at_vars ~var_args:sup_in_var_args ~fun_bodies:sup_under_lambdas 
        ~ty_args:false ~ord ~which:`Max ~subterms:true  ~eligible:(C.Eligible.res c) (C.lits c)
      |> Iter.append (TPSet.to_iter @@ C.eligible_subterms_of_bool c)
      |> Iter.sort_uniq ~cmp:(fun (_, p1) (_, p2) -> Position.compare p1 p2)
      |> Iter.filter (fun (t, _) ->
          (* Util.debugf ~section 3 "@[ Filtering vars %a,1  @]" (fun k-> k T.pp t); *)
          (not (T.is_var t) || T.is_ho_var t))
      (* TODO: could exclude more variables from the index:
         they are not needed if they occur with the same args everywhere in the clause *)
      |> Iter.filter (fun (t, _) ->
          (* Util.debugf ~section 3 "@[ Filtering vars %a,2  @]" (fun k-> k T.pp t); *)
          sup_at_var_headed || not (T.is_var (T.head_term t)))
      |> Iter.fold
        (fun tree (t, pos) ->
           Util.debugf ~section 2 "inserting(into):@[@[%a@]|@[%a]@]" (fun k-> k C.pp c Term.pp t);
           let with_pos = C.WithPos.({term=t; pos; clause=c;}) in
           f tree t with_pos)
        !_idx_sup_into;

    (* index subterms that can be rewritten by FluidSup *)
    if fluidsup then
      _idx_fluidsup_into :=
        Lits.fold_terms ~vars:true ~var_args:false ~fun_bodies:false
          ~ty_args:false ~ord ~which:`Max ~subterms:true
          ~eligible:(C.Eligible.res c) (C.lits c)
        (* TODO(BOOL): How is this going to be extended for Boolean reasoning? *)
        |> Iter.filter (fun (t, _) -> is_fluid_or_deep c t) 
        |> Iter.fold
          (fun tree (t, pos) ->
             let with_pos = C.WithPos.({term=t; pos; clause=c;}) in
             f tree t with_pos)
          !_idx_fluidsup_into;

     (* index subterms that can be rewritten by FluidSup *)
    if subvarsup then
      _idx_subvarsup_into :=
        Lits.fold_terms ~vars:true ~var_args:false ~fun_bodies:false
          ~ty_args:false ~ord ~which:`Max ~subterms:true
          ~eligible:(C.Eligible.res c) (C.lits c)
        (* TODO(BOOL): How is this going to be extended for Boolean reasoning? *)
        |> Iter.filter (fun (t, pos) ->
          match T.view t with
          | T.Var _ -> has_bad_occurrence_elsewhere c t pos
          | T.App(hd, [_]) when T.is_var hd -> has_bad_occurrence_elsewhere c hd pos
          | _ -> false
        ) 
        |> Iter.fold
          (fun tree (t, pos) ->
             let with_pos = C.WithPos.({term=t; pos; clause=c;}) in
             f tree t with_pos)
          !_idx_subvarsup_into;

    if dupsup then 
      _idx_dupsup_into :=
        Lits.fold_terms ~vars:false ~var_args:false ~fun_bodies:false
          ~ty_args:false ~ord ~which:`Max ~subterms:true
          ~eligible:(C.Eligible.res c) (C.lits c)
        (* TODO(BOOL): How is this going to be extended for Boolean reasoning? *)
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
        (* TODO(BOOL): Maybe add bool stuff to LambdaSup *)
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
      Lits.fold_eqn ~ord ~both:true
        ~eligible:(C.Eligible.param c) (C.lits c)
      |> Iter.filter( (fun (_,r,sign,_) -> sign || T.equal r T.false_))
      |> Iter.filter((fun (l, _, _, _) ->
        (Env.flex_get k_superpose_w_formulas) || 
        begin match T.view l with
        | T.AppBuiltin((Eq|Neq), _) -> false
        | _ -> not (T.is_formula l) end
      ))
      |> Iter.filter(fun (l, _, _, _) -> 
          sup_from_var_headed || not (T.is_app_var l))
      |> Iter.fold
        (fun tree (l, r, sign, pos) ->
           assert (sign || T.equal r T.false_);
           let with_pos = C.WithPos.({term=l; pos; clause=c;}) in
           Util.debugf ~section 2 "inserting(from):@[@[%a@]|@[%a]@]" (fun k-> k C.pp c Term.pp l);
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
           match T.view t with 
           | T.AppBuiltin(hd, [_;body]) ->
             let tree = 
              if Builtin.is_quantifier hd && Env.flex_get k_quant_demod then (
               let _,unfolded = T.open_fun body in
               let pos = P.(append pos ((P.arg 1 (P.body P.stop)))) in
               let with_pos = C.WithPos.( {term=unfolded; pos; clause=c} ) in
               f tree unfolded with_pos
              ) else tree in
             let with_pos = C.WithPos.( {term=t; pos; clause=c} ) in
             f tree t with_pos
           | _ ->
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
        (* do not use formulas for rewriting... can have adverse
            effects on lazy cnf *)
        if (not (Env.flex_get k_rw_with_formulas ))&&
            (T.is_appbuiltin l || (T.is_appbuiltin r && not @@ T.is_true_or_false r) ) then idx
        else (
          begin match Ordering.compare ord l r with
            | Comparison.Gt ->
              f idx (l,r,true,c)
            | Comparison.Lt ->
              f idx (r,l,true,c)
            | Comparison.Incomparable ->
              let idx = f idx (l,r,true,c) in
              f idx (r,l,true,c)
            | Comparison.Eq -> idx  (* no modif *)
          end)
      | [| Lit.Equation (l,r,false) |] -> f idx (l,r,false,c)
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

  (** {5 Inference Rules} *)

  (* ----------------------------------------------------------------------
   * Superposition rule
   * ---------------------------------------------------------------------- *)

  type supkind =
    | Classic
    | FluidSup
    | LambdaSup
    | DupSup
    | SubVarSup

  let kind_to_str = function
    | Classic -> "sup"
    | FluidSup -> "fluidSup"
    | LambdaSup -> "lambdaSup"
    | DupSup -> "dupSup"
    | SubVarSup -> "subVarSup"

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
    if Env.flex_get k_check_sup_at_var_cond then (
      let open SupInfo in
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
        let unique_args_of_var c =
          C.lits c
          |> Lits.fold_terms ~vars:true ~ty_args:false ~which:`All ~ord ~subterms:true ~eligible:(fun _ _ -> true)
          |> Iter.fold_while
            (fun unique_args (t,_) ->
               if Term.equal (fst (T.as_app t)) var
               then (
                 if CCOpt.equal (CCList.equal T.equal) unique_args (Some (snd (T.as_app t)))
                 then (unique_args, `Continue) (* found the same arguments of var again *)
                 else (None, `Stop) (* different arguments of var found *)
               ) else (unique_args, `Continue) (* this term doesn't have var as head *)
            )
            None
        in
        let unique_vars =
          if Env.flex_get Higher_order.k_prune_arg_fun != `NoPrune
          then None
          else unique_args_of_var info.passive in
        match unique_vars with
        | Some _ ->
          Util.debugf ~section 5
            "Variable %a has same args everywhere in %a"
            (fun k->k T.pp var C.pp info.passive);
          false (* If var occurs with the same arguments everywhere, we don't need sup at vars *)
        | None ->
          (* Check whether Cσ is >= C[var -> replacement]σ *)
          (* This is notoriously hard to implement due to the scope mechanism.
          Especially note that var may be of polymorphic type and subst might
          map to var, which can easily cause cyclic substitutions. *)
          let passive'_lits = Lits.apply_subst renaming subst (C.lits info.passive, info.scope_passive) in
          let fresh_var = HVar.fresh ~ty:(T.ty var) () in
          let subst_fresh_var = US.FO.bind US.empty (T.as_var_exn var, info.scope_passive) (T.var fresh_var, info.scope_passive) in
          let passive_fresh_var = Lits.apply_subst Subst.Renaming.none (US.subst subst_fresh_var) (C.lits info.passive, info.scope_passive) in
          let subst_replacement = Unif.FO.bind subst (fresh_var, info.scope_passive) (replacement, info.scope_active) in
          let passive_t'_lits = Lits.apply_subst renaming subst_replacement (passive_fresh_var, info.scope_passive) in
          if Lits.compare_multiset ~ord passive'_lits passive_t'_lits = Comp.Gt
          then (
            Util.debugf ~section 3
              "Sup at var condition is not fulfilled because: %a >= %a"
              (fun k->k Lits.pp passive'_lits Lits.pp passive_t'_lits);
            false
          )
          else true (* If Cσ is either <= or incomparable to C[var -> replacement]σ, we need sup at var.*)
      )
    ) 
    else false (* if k_check_sup_at_var_cond is false, never allow superposition at variable headed terms *)


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
              | T.App (f, ss), T.App (g, tt) 
                when List.length ss >= List.length args 
                      && List.length tt >= List.length args ->	
                let s_args = Array.of_list ss in	
                let t_args = Array.of_list tt in
                let sub_s_args = 
                  Array.sub s_args (Array.length s_args - List.length args) (List.length args)
                  |> CCArray.to_list in
                let sub_t_args =
                  Array.sub t_args (Array.length t_args - List.length args) (List.length args)
                  |> CCArray.to_list in
                if	
                  Array.length s_args >= List.length args	
                  && Array.length t_args >= List.length args	
                  (* Check whether the last argument(s) of s and t are equal *)	
                  && List.for_all (fun (s,t) -> T.equal s t) (List.combine sub_s_args sub_t_args) 
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
    let module TPS = SClause.TPSet in
    Util.incr_stat stat_superposition_call;
    let sc_a = info.scope_active in
    let sc_p = info.scope_passive in
    assert (InnerTerm.DB.closed (info.s:>InnerTerm.t));
    assert (info.sup_kind == LambdaSup || InnerTerm.DB.closed (info.u_p:T.t:>InnerTerm.t));
    assert (not(T.is_var info.u_p) || T.is_ho_var info.u_p || info.sup_kind = FluidSup);
    assert (Env.flex_get k_sup_at_var_headed || info.sup_kind = FluidSup || 
            info.sup_kind = DupSup || info.sup_kind = SubVarSup || not (T.is_var (T.head_term info.u_p)));
    let active_idx = Lits.Pos.idx info.active_pos in
    let shift_vars = if info.sup_kind = LambdaSup then 0 else -1 in
    let passive_idx, passive_lit_pos = Lits.Pos.cut info.passive_pos in
    let bool_inference =
      (* optimization -- if no selected subterms, do not enter eligible_subterms_of_bool *)
      not (CCList.is_empty (C.bool_selected info.passive)) &&
      TPS.mem (info.u_p, info.passive_pos) (C.eligible_subterms_of_bool info.passive)
    in

    assert(Array.for_all Literal.no_prop_invariant (C.lits info.passive));
    assert(Array.for_all Literal.no_prop_invariant (C.lits info.active));
    try
      if (info.sup_kind = LambdaSup && US.has_constr info.subst) then (
        raise (ExitSuperposition "Might sneak in bound vars through constraints.")
      );

      let renaming = S.Renaming.create () in
      let us = info.subst in
      let subst = US.subst us in
      let lambdasup_vars =
        if (info.sup_kind = LambdaSup) then (
          Term.Seq.subterms ~include_builtin:true info.u_p |> Iter.filter Term.is_var |> Term.Set.of_iter)
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
             not @@ T.DB.is_closed @@  Subst.FO.apply ~shift_vars renaming subst (v,sc_p))
           lambdasup_vars) then (
        let msg = "LambdaSup: an into free variable sneaks in bound variable" in
        Util.debugf ~section 3 "%s" (fun k->k msg);
        raise @@ ExitSuperposition(msg);
      );

      if info.sup_kind = FluidSup && 
         Term.equal (Lambda.eta_reduce @@ Lambda.snf @@ t') 
           (Lambda.eta_reduce @@ Lambda.snf @@ S.FO.apply ~shift_vars renaming subst (info.s, sc_a)) then (
        let msg = "Passive literal is trivial after substitution" in
        Util.debugf ~section 3 "%s" (fun k->k msg);
        raise @@ ExitSuperposition(msg);
      );


      (
        let exit_negative_tl =
          ExitSuperposition ("negative literal must paramodulate " ^
                            "into top-level positive position")
        in
        let exit_double_sup =
          ExitSuperposition ("superposition could be performed in a different order")
        in
        match info.passive_pos with
        | P.Arg(_, P.Left P.Stop)
        | P.Arg(_, P.Right P.Stop) ->
          if T.equal t' T.false_ then (
            if (not (Env.flex_get k_superpose_w_formulas) && 
                not (Lit.is_positivoid (info.passive_lit))) ||
                (Env.flex_get k_superpose_w_formulas && 
                 not (T.is_appbuiltin info.s))
            then (raise exit_negative_tl)) 
          else if Lit.is_positivoid info.passive_lit &&
                    (* active clause will take the role of passive and that is how
                       we can compute the resolvent *)
                    C.compare info.active info.passive < 0 then (
            raise exit_double_sup
          );
        | _ ->
          if T.equal t' T.false_ && (
            not (Env.flex_get k_superpose_w_formulas) ||
            not @@ T.is_appbuiltin info.s
          ) then 
            raise @@ exit_negative_tl);


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
        let vars_act = CCArray.except_idx (C.lits info.active) active_idx
                     |> CCArray.of_list |> Literals.vars |> T.VarSet.of_list in
        let vars_pas = C.lits info.passive |> Literals.vars |> T.VarSet.of_list in
        let dbs = ref [] in
        let vars_bound_to_closed_terms var_set scope =
          T.VarSet.iter (fun v -> 
              match Subst.FO.get_var subst ((v :> InnerTerm.t HVar.t),scope) with
              | Some (t,_) -> dbs := T.DB.unbound t @ !dbs (* hack *)
              | None -> ()) var_set in

        vars_bound_to_closed_terms vars_act sc_a;
        vars_bound_to_closed_terms vars_pas sc_p;

        if Util.Int_set.cardinal (Util.Int_set.of_list !dbs)  > Env.flex_get k_lambdasup   then (
          let msg = "Too many skolems will be introduced for LambdaSup." in
          Util.debugf ~section 3 "%s"  (fun k->k msg);
          raise (ExitSuperposition msg);
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
           (* if it was an inference into selected Bool position, 
              we do not reevaluate BoolSelection on the new literal set
              -- in 99% of cases it is selected again,
              and if it is Bool selected subterm position then for we do not
              check ordering restrictions*)
        (not bool_inference &&
         not (Lit.Pos.is_max_term ~ord passive_lit' passive_lit_pos)) ||
        (not bool_inference &&
         not (BV.get (C.eligible_res (info.passive, sc_p) subst) passive_idx)) ||
        not (C.is_eligible_param (info.active, sc_a) subst ~idx:active_idx)
      ) then (
        let c1 = O.compare ord s' t' = Comp.Lt in
        let c2 = not bool_inference &&
                 not (Lit.Pos.is_max_term ~ord passive_lit' passive_lit_pos)in
        let c3 = not bool_inference &&
                 not (BV.get (C.eligible_res (info.passive, sc_p) subst) passive_idx) in
        let c4 = not (C.is_eligible_param (info.active, sc_a) subst ~idx:active_idx) in        
        raise (ExitSuperposition (
          CCFormat.sprintf "bad ordering conditions %b %b %b %b" c1 c2 c3 c4)));
      (* Check for superposition at a variable *)
      if info.sup_kind != FluidSup then
        if not @@ Env.flex_get k_sup_at_vars then(
          if (T.is_var info.u_p) then raise (ExitSuperposition ("sup at var position"));
        )
        else if T.is_var info.u_p && not (sup_at_var_condition info info.u_p info.t) then (
          Util.debugf ~section 3 "superposition at variable" (fun k->k);
          raise (ExitSuperposition "superposition at variable");
        );

      (* Check for hidden superposition at a variable *)	
      if Env.flex_get k_restrict_hidden_sup_at_vars then (	
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
      (* apply substitution to other literals *)
      (* Util.debugf 1 "Before unleak: %a, after unleak: %a"
         (fun k -> k Subst.pp subst Subst.pp subst'); *)
      let new_lits =
        new_passive_lit ::
        c_guard @
        Lit.apply_subst_list renaming subst' (lits_a, sc_a) @
        Lit.apply_subst_list renaming subst' (lits_p, sc_p)
      in
      let pos_enclosing_up = Position.until_first_fun passive_lit_pos in
      let fun_context_around_up =  Subst.FO.apply renaming subst' 
          (Lit.Pos.at info.passive_lit pos_enclosing_up, sc_p) in
      let vars = Iter.append (T.Seq.vars fun_context_around_up) (T.Seq.vars t')
                 |> Term.VarSet.of_iter
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
              (fun sk sk_v acc -> Term.replace ~old:sk ~by:sk_v acc)
              sk_with_vars t ) lit) new_lits in
      
      let subst_is_ho = 
        Subst.codomain subst
        |> Iter.exists (fun (t,_) -> 
            Iter.exists (fun t -> T.is_fun t || T.is_comb t) 
              (T.Seq.subterms ~include_builtin:true (T.of_term_unsafe t))) in

      let rule =
        let r = kind_to_str info.sup_kind in
        let sign = if Lit.is_positivoid passive_lit' then "+" else "-" in
        Proof.Rule.mk (r ^ sign)
      in
      CCList.iter (fun (sym,ty) -> Ctx.declare sym ty) !skolem_decls;
      let tags = (if subst_is_ho || info.sup_kind != Classic 
                  then [Proof.Tag.T_ho] else []) 
                  @ Unif_subst.tags us in
      let proof =
        Proof.Step.inference ~rule ~tags
          [C.proof_parent_subst renaming (info.active,sc_a) subst';
           C.proof_parent_subst renaming (info.passive,sc_p) subst']
      and penalty =
        let pen_a = C.penalty info.active in
        let pen_b = C.penalty info.passive in
        let max_d = max (C.proof_depth info.active) (C.proof_depth info.passive) in

        (if pen_a == 1 && pen_b == 1 then 1 else pen_a + pen_b)
        + (if info.sup_kind == Classic && T.is_var info.s then 1 else 0) (* superposition from app var = bad, unless we are superposing into a formula *)
        + (if info.sup_kind == Classic && T.is_app_var info.s 
           then (if T.is_var (T.head_term info.t) then 2*max_d else max (max_d - 2) 0)
           else 0)
        + (if info.sup_kind == FluidSup then Env.flex_get k_fluidsup_penalty else 0)
        + (if info.sup_kind == DupSup then Env.flex_get k_dupsup_penalty else 0)
        + (if info.sup_kind == LambdaSup then 1 else 0)

      in
      let new_clause = C.create ~trail:new_trail ~penalty new_lits proof in
      (* Format.printf "LS: %a\n" C.pp new_clause;  *)
      Util.debugf ~section 1 "@[... ok, conclusion@ @[%a@]@]" (fun k->k C.pp new_clause);
      if (not (List.for_all (Lit.for_all Term.DB.is_closed) new_lits)) then (
        CCFormat.printf "@[<2>sup, kind %s(%d)@ (@[<2>%a[%d]@ @[s=%a@]@ @[t=%a, t'=%a@]@])@ \
         (@[<2>%a[%d]@ @[passive_lit=%a@]@ @[p=%a@]@])@ with subst=@[%a@]@]"
         (kind_to_str info.sup_kind) (Term.Set.cardinal lambdasup_vars) C.pp info.active sc_a T.pp info.s T.pp info.t
            T.pp t' C.pp info.passive sc_p Lit.pp info.passive_lit
            Position.pp info.passive_pos US.pp info.subst;
        assert false;
      );
      assert(Array.for_all Literal.no_prop_invariant (C.lits new_clause));
      if not (C.lits new_clause |> Literals.vars_distinct) then (
        CCFormat.printf "a:@[%a@]@." C.pp info.active;
        CCFormat.printf "p:@[%a@]@." C.pp info.passive;
        CCFormat.printf "r:@[%a@]@." C.pp new_clause;
        CCFormat.printf "sub:@[%a@]@." Subst.pp subst';
        assert false;
      );
      Some new_clause
    with ExitSuperposition reason ->
      Util.debugf ~section 1 "... cancel, %s" (fun k->k reason);
      None

  (* simultaneous superposition: when rewriting D with C \lor s=t,
      replace s with t everywhere in D rather than at one place. *)
  let do_simultaneous_superposition info =
    let open SupInfo in
    let module P = Position in
    let module TPS = SClause.TPSet in
    Util.incr_stat stat_superposition_call;
    let sc_a = info.scope_active in
    let sc_p = info.scope_passive in
    Util.debugf ~section 2
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
    let bool_inference = 
      TPS.mem (info.u_p, info.passive_pos) (C.eligible_subterms_of_bool info.passive)
    in
    let shift_vars = if info.sup_kind = LambdaSup then 0 else -1 in
    try     
      let renaming = S.Renaming.create () in
      let us = info.subst in
      let subst = US.subst us in
      let t' = S.FO.apply ~shift_vars renaming subst (info.t, sc_a) in
      (let exit_negative_tl =
          ExitSuperposition ("negative literal must paramodulate " ^
                            "into top-level positive position")
        in
        let exit_double_sup =
          ExitSuperposition ("superposition could be performed in a different order")
        in
        match info.passive_pos with
        | P.Arg(_, P.Left P.Stop)
        | P.Arg(_, P.Right P.Stop) ->
          if T.equal t' T.false_ && not (Lit.is_positivoid (info.passive_lit)) then (
            raise exit_negative_tl) 
          else if Lit.is_positivoid info.passive_lit &&
                    (* active clause will take the role of passive and that is how
                       we can compute the resolvent *)
                    C.compare info.active info.passive < 0 then (
            raise exit_double_sup
          );
        | _ ->
          if T.equal t' T.false_ then 
            raise @@ exit_negative_tl);
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
        (not bool_inference &&
         not (Lit.Pos.is_max_term ~ord passive_lit' passive_lit_pos)) ||
        (not bool_inference &&
         not (BV.get (C.eligible_res (info.passive, sc_p) subst) passive_idx)) ||
        not (C.is_eligible_param (info.active, sc_a) subst ~idx:active_idx)
      ) then raise (ExitSuperposition "bad ordering conditions");
      (* Check for superposition at a variable *)
      if info.sup_kind != FluidSup then
        if not @@ Env.flex_get k_sup_at_vars then(
          if (T.is_var info.u_p) then raise (ExitSuperposition ("sup at var position"));
        )
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
        let sign = if Lit.is_positivoid passive_lit' then "+" else "-" in
        Proof.Rule.mk ("s_" ^ r ^ sign)
      in
      let subst_is_ho = 
        Subst.codomain subst
        |> Iter.exists (fun (t,_) -> 
            Iter.exists (fun t -> T.is_fun t || T.is_comb t) 
              (T.Seq.subterms ~include_builtin:true (T.of_term_unsafe t))) in
      let tags = (if subst_is_ho then [Proof.Tag.T_ho] else []) @ Unif_subst.tags us in
      let proof =
        Proof.Step.inference ~rule ~tags
          [C.proof_parent_subst renaming (info.active,sc_a) subst;
           C.proof_parent_subst renaming (info.passive,sc_p) subst]
      and penalty =
        let pen_a = C.penalty info.active in
        let pen_b = C.penalty info.passive in
        (if pen_a == 1 && pen_b == 1 then 1 else pen_a + pen_b + 1)
        + (if T.is_var s' then 2 else 0) (* superposition from var = bad *)
        + (if US.has_constr info.subst then 1 else 0)
      in
      let new_clause = C.create ~trail:new_trail ~penalty new_lits proof in
      Util.debugf ~section 2 "@[... ok, conclusion@ @[%a@]@]" (fun k->k C.pp new_clause);
      assert(C.lits new_clause |> Literals.vars_distinct);
      Some new_clause
    with ExitSuperposition reason ->
      Util.debugf ~section 2 "@[... cancel, %s@]" (fun k->k reason);
      None

  (* choose between regular and simultaneous superposition *)
  let do_superposition info =
    let open SupInfo in
    assert (info.sup_kind=DupSup || info.sup_kind=SubVarSup || Type.equal (T.ty info.s) (T.ty info.t));
    assert (info.sup_kind=DupSup || info.sup_kind=SubVarSup ||
            Unif.Ty.equal ~subst:(US.subst info.subst)
              (T.ty info.s, info.scope_active) (T.ty info.u_p, info.scope_passive));
    let renaming = Subst.Renaming.create () in
    let shift_vars = if info.sup_kind = LambdaSup then 0 else -1 in
    let s = Subst.FO.apply ~shift_vars renaming (US.subst info.subst) (info.s, info.scope_active) in
    let u_p = Subst.FO.apply ~shift_vars renaming (US.subst info.subst) (info.u_p, info.scope_passive) in
    let norm t = T.normalize_bools @@ Lambda.eta_expand @@ Lambda.snf t in
    if info.sup_kind != SubVarSup && 
       not (Term.equal (norm @@ s) (norm @@ u_p) || US.has_constr info.subst) then (
        CCFormat.printf "@[<2>sup, kind %s@ (@[<2>%a[%d]@ @[s=%a@]@ @[t=%a@]@])@ \
          (@[<2>%a[%d]@ @[passive_lit=%a@]@ @[p=%a@]@])@ with subst=@[%a@]@].\n"
              (kind_to_str info.sup_kind) C.pp info.active info.scope_active T.pp info.s T.pp info.t
              C.pp info.passive info.scope_passive Lit.pp info.passive_lit
              Position.pp info.passive_pos US.pp info.subst;
        CCFormat.printf "orig_s:@[%a@]@." T.pp info.s;
        CCFormat.printf "norm_s:@[%a@]@." T.pp (norm s);
        CCFormat.printf "orig_u_p:@[%a@]@." T.pp info.u_p;
        CCFormat.printf "norm_u_p:@[%a@]@." T.pp (norm u_p);

        assert false;
    );
    if Env.flex_get k_use_simultaneous_sup && info.sup_kind != LambdaSup && info.sup_kind != DupSup
    then do_simultaneous_superposition info
    else do_classic_superposition info

  let infer_active_aux ~retrieve_from_index ~process_retrieved clause =
    let _span = ZProf.enter_prof prof_infer_active in
    (* no literal can be eligible for paramodulation if some are selected.
       This checks if inferences with i-th literal are needed? *)
    let eligible = C.Eligible.param clause in
    (* do the inferences where clause is active; for this,
       we try to rewrite conditionally other clauses using
       non-minimal sides of every positive literal *)
    let new_clauses =
      Lits.fold_eqn ~ord ~both:true ~eligible (C.lits clause)
      |> Iter.filter (fun (_,t,sign,_) -> sign || T.equal t T.false_)
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
    ZProf.exit_prof _span;
    new_clauses

  let infer_passive_aux ~retrieve_from_index ~process_retrieved clause =
    let _span = ZProf.enter_prof prof_infer_passive in
    (* perform inference on this lit? *)
    let eligible = C.Eligible.(res clause) in
    (* do the inferences in which clause is passive (rewritten),
       so we consider both negative and positive literals *)
    let module TPSet = SClause.TPSet in
    let new_clauses =
      Lits.fold_terms ~vars:(Env.flex_get k_sup_at_vars) 
        ~var_args:(Env.flex_get k_sup_in_var_args)
        ~fun_bodies:(Env.flex_get k_sup_under_lambdas) 
        ~subterms:true ~ord ~which:`Max ~eligible ~ty_args:false 
        (C.lits clause)
      |> Iter.append (TPSet.to_iter (C.eligible_subterms_of_bool clause))
      |> Iter.sort_uniq ~cmp:(fun (_,p1) (_,p2) -> Position.compare p1 p2)
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
    ZProf.exit_prof _span;
    new_clauses

  let infer_active clause =
    infer_active_aux
      ~retrieve_from_index:(I.retrieve_unifiables)
      ~process_retrieved:(fun do_sup (u_p, with_pos, subst) -> do_sup u_p with_pos subst)
      clause

  let infer_lambdasup_from clause =
    (* no literal can be eligible for paramodulation if some are selected.
       This checks if inferences with i-th literal are needed? *)
    let eligible = C.Eligible.param clause in
    (* do the inferences where clause is active; for this,
       we try to rewrite conditionally other clauses using
       non-minimal sides of every positive literal *)
    Lits.fold_eqn ~ord  ~both:true ~eligible (C.lits clause)
    |> Iter.filter (fun (_,t,sign,_) -> sign || T.equal t T.false_)
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
      ~retrieve_from_index:(I.retrieve_unifiables)
      ~process_retrieved:(fun do_sup (u_p, with_pos, subst) -> do_sup u_p with_pos subst)
      clause

  let infer_lambdasup_into clause =
    let _span = ZProf.enter_prof prof_infer_active in
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
    ZProf.exit_prof _span;
    new_clauses
  
  let infer_active_complete_ho clause =
    let inf_res = infer_active_aux
        ~retrieve_from_index:(I.retrieve_unifiables_complete ~unif_alg:(Env.flex_get k_unif_alg))
        ~process_retrieved:(fun do_sup (u_p, with_pos, substs) ->
            (* let penalty = max (C.penalty clause) (C.penalty with_pos.C.WithPos.clause) in *)
            (* /!\ may differ from the actual penalty (by -2) *)
            let parents = [clause; with_pos.clause] in
            let p = max (C.penalty clause) (C.penalty with_pos.clause) in
            Some (p, parents, OSeq.map (CCOpt.flat_map (do_sup u_p with_pos)) substs))
        clause
    in

    if Env.should_force_stream_eval () then (
      Env.get_finite_infs (List.map (fun (_,_,x) -> x) inf_res)
    ) else(
      let clauses, streams = force_getting_cl inf_res in
      StmQ.add_lst (Env.get_stm_queue ()) streams; 
      clauses
    )

  let infer_passive_complete_ho clause =
    let inf_res = infer_passive_aux
        ~retrieve_from_index:(I.retrieve_unifiables_complete ~unif_alg:(Env.flex_get k_unif_alg))
        ~process_retrieved:(fun do_sup (u_p, with_pos, substs) ->
            (* let penalty = max (C.penalty clause) (C.penalty with_pos.C.WithPos.clause) in *)
            (* /!\ may differ from the actual penalty (by -2) *)
            let parents = [clause; with_pos.clause] in
            let p = max (C.penalty clause) (C.penalty with_pos.clause) in
            Some (p, parents, OSeq.map (CCOpt.flat_map (do_sup u_p with_pos)) substs))
        clause
    in
    
    if Env.should_force_stream_eval () then (
      Env.get_finite_infs (List.map (fun (_,_,x) -> x) inf_res)
    ) else (
      let clauses, streams = force_getting_cl inf_res in
      StmQ.add_lst (Env.get_stm_queue ()) streams; 
      clauses
    )

  
  (* ----------------------------------------------------------------------
   * FluidSup rule (Superposition at applied variables)
   * ---------------------------------------------------------------------- *)

  let infer_fluidsup_active clause =
    let _span = ZProf.enter_prof prof_infer_fluidsup_active in
    (* no literal can be eligible for paramodulation if some are selected.
       This checks if inferences with i-th literal are needed? *)
    let eligible = C.Eligible.param clause in
    (* do the inferences where clause is active; for this,
       we try to rewrite conditionally other clauses using
       non-minimal sides of every positive literal *)
    let new_clauses =
      if fluidsup_applicable clause then
        Lits.fold_eqn ~ord ~both:true ~eligible (C.lits clause)
        |> Iter.filter (fun (_,t,sign,_) -> sign || T.equal t T.false_)
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
                  Iter.cons (penalty,res,[clause;with_pos.clause]) acc
               )
               Iter.empty
          )
        |> Iter.to_rev_list
      else []
    in
    if Env.should_force_stream_eval () then (
      Env.get_finite_infs (List.map (fun (_,x,_) -> x) new_clauses) 
    ) else (
      let stm_res = List.map (fun (p,s,parents) -> Stm.make ~penalty:p ~parents (s)) new_clauses in
      StmQ.add_lst (Env.get_stm_queue ()) stm_res;
      ZProf.exit_prof _span;
      [])

  let infer_fluidsup_passive clause =
    let _span = ZProf.enter_prof prof_infer_fluidsup_passive in
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
                  Iter.cons (penalty,res,[clause;with_pos.clause]) acc
               )
               Iter.empty
          )
        |> Iter.to_rev_list
      else []
    in
    if Env.should_force_stream_eval () then (
      Env.get_finite_infs (List.map (fun (_,x,_) -> x) new_clauses)
    ) else (
      let stm_res = List.map (fun (p,s,parents) -> Stm.make ~penalty:p ~parents (s)) new_clauses in
      StmQ.add_lst (Env.get_stm_queue ()) stm_res;
      ZProf.exit_prof _span;
      []
    )

  (* ----------------------------------------------------------------------
   * DupSup rule (Lightweight superposition at applied variables)
   * ---------------------------------------------------------------------- *)

  let infer_dupsup_active clause =
    let _span = ZProf.enter_prof prof_infer_active in
    let eligible = C.Eligible.param clause in
    let new_clauses =
      Lits.fold_eqn ~ord ~both:true ~eligible (C.lits clause)
      |> Iter.filter (fun (_,t,sign,_) -> sign || T.equal t T.false_)
      |> Iter.flat_map
        (fun (s, t, _, s_pos) ->
           I.fold !_idx_dupsup_into
             (fun acc u_p with_pos ->
                assert (T.is_var (T.head_term u_p));
                assert (T.DB.is_closed u_p);
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
                  let var_z = HVar.fresh ~ty:(Type.arrow (arg_types @ [T.ty t]) (T.ty u_p)) () in
                  let db_args = List.mapi (fun i ty -> T.bvar ~ty (n-1-i)) arg_types in
                  let term_w,term_z = T.var var_w, T.var var_z in
                  let w_db = T.app term_w db_args in
                  let z_db = T.app term_z (db_args @ [w_db]) in
                  let y_subst_val = T.fun_l arg_types z_db in
                  assert (T.DB.is_closed y_subst_val);
                  let subst_y = US.FO.bind (US.empty) (var_up, scope_passive) (y_subst_val, scope_passive) in
                  let w_args = T.app term_w args_up in
                  let w_args = Subst.FO.apply Subst.Renaming.none (US.subst subst_y) (w_args,scope_passive) in
                  let z_args = T.app term_z (args_up @ [t]) in
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
                    + (Env.flex_get k_fluidsup_penalty / 3) in
                  (* /!\ may differ from the actual penalty (by -2) *)
                  Iter.cons (penalty,res,[clause; with_pos.clause]) acc
                ))
             Iter.empty
        )
      |> Iter.to_rev_list
    in
    if Env.should_force_stream_eval () then (
      Env.get_finite_infs (List.map (fun (_,x,_) -> x) new_clauses)
    ) else (
      let stm_res = List.map (fun (p,s,parents) -> Stm.make ~penalty:p ~parents ( s)) new_clauses in
      StmQ.add_lst (Env.get_stm_queue ()) stm_res;
      ZProf.exit_prof _span;
      []
    )

  let infer_dupsup_passive clause =
    let _span = ZProf.enter_prof prof_infer_fluidsup_passive in
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
                        + ((Env.flex_get k_fluidsup_penalty) / 3) in
                      (* /!\ may differ from the actual penalty (by -2) *)
                      Iter.cons (penalty,res,[clause;with_pos.clause]) acc))
                | _ -> acc)
             Iter.empty
        )
      |> Iter.to_rev_list
    in
    if Env.should_force_stream_eval () then (
      Env.get_finite_infs (List.map (fun (_,x,_) -> x) new_clauses)
    ) else (
      let stm_res = List.map (fun (p,s,parents) -> Stm.make ~penalty:p ~parents (s)) new_clauses in
      StmQ.add_lst (Env.get_stm_queue ()) stm_res;
      ZProf.exit_prof _span;
      [])

  (* ----------------------------------------------------------------------
   * SubVarSup rules
   * ---------------------------------------------------------------------- *)

  let do_subvarsup ~active_pos ~passive_pos =
    (* Variable names in each clause renamed apart *)
    let renaming = Subst.Renaming.create () in
    let rename_term t = Subst.FO.apply renaming Subst.empty t in
    let sc_a, sc_p = 0, 1 in

    let cl_a, cl_p = 
      C.apply_subst ~renaming (active_pos.C.WithPos.clause,sc_a) Subst.empty,
      C.apply_subst ~renaming (passive_pos.C.WithPos.clause,sc_p) Subst.empty in
    let s,t = 
      match Lits.View.get_eqn (C.lits cl_a) active_pos.C.WithPos.pos with
      | Some(l,r,_) -> l,r
      | _ -> invalid_arg "active lit must be an equation" in

    assert(T.is_var t || T.is_app_var t || T.is_comb t);

    let u_p = rename_term (passive_pos.C.WithPos.term,sc_p) in
    let var,args =
      match T.view u_p with 
      | T.Var v -> v,[]
      | T.App(hd, [x]) when T.is_var hd ->
        T.as_var_exn hd, [x]
      | _ -> invalid_arg "u_p must be of the form y or y s" in
    

    let z_ty = Type.arrow [T.ty t] (HVar.ty var) in
    let z = T.app (T.var @@ HVar.fresh ~ty:z_ty ()) [t] in
    let subst = Subst.FO.bind' Subst.empty (var, 0) (z,0) in
    
    let passive_lit,_ = Lits.Pos.lit_at (C.lits cl_p) passive_pos.C.WithPos.pos in

    let sup_info = 
      SupInfo.{
        active = cl_a; active_pos=active_pos.C.WithPos.pos; scope_active=0; s; t=T.app z args;
        passive = cl_p; passive_pos=passive_pos.C.WithPos.pos; scope_passive=0;
        passive_lit; u_p; subst = US.of_subst subst; sup_kind = SubVarSup} in
    do_superposition sup_info
  
  let infer_subvarsup_active clause =
    let eligible = C.Eligible.param clause in
    Lits.fold_eqn ~ord ~both:true ~eligible (C.lits clause)
    |> Iter.filter (fun (_,t,sign,_) -> sign || T.equal t T.false_)
    |> Iter.filter(fun (_,t,_,_) -> T.is_var t || T.is_app_var t || T.is_comb t) 
    |> Iter.flat_map
      (fun (s, t, _, s_pos) ->
          (* rewrite clauses using s *)
          I.fold !_idx_subvarsup_into
            (fun acc _ with_pos ->
              let active_pos = C.WithPos.{term=s; pos=s_pos; clause} in
              match do_subvarsup ~passive_pos:with_pos ~active_pos with
              | Some c -> 
                Util.debugf ~section 2 "svs: @[%a@]@. @[%a@]. @[%a@]@." 
                  (fun k -> k C.pp clause C.pp with_pos.clause C.pp c);
                Iter.cons c acc
              | None -> acc)
            Iter.empty
      )
    |> Iter.to_rev_list


  let infer_subvarsup_passive clause =
    let eligible = C.Eligible.(res clause) in
    Lits.fold_terms ~vars:true ~var_args:false ~fun_bodies:false ~subterms:true ~ord
      ~which:`Max ~eligible ~ty_args:false (C.lits clause)
    |> Iter.filter( fun (t,pos) -> 
        match T.view t with 
        | T.Var _ -> has_bad_occurrence_elsewhere clause t pos
        | T.App(hd, [x]) -> has_bad_occurrence_elsewhere clause hd pos
        | _ -> false)
    |> Iter.flat_map
      (fun (u_p, passive_pos) ->
          I.fold !_idx_sup_from
            (fun acc _ with_pos ->
              let passive_pos = C.WithPos.{term=u_p; pos=passive_pos; clause} in
              match Lits.View.get_eqn (C.lits with_pos.C.WithPos.clause) with_pos.C.WithPos.pos with
              | Some(l,r,_) when T.is_var r || T.is_app_var r || T.is_comb r->
                begin match do_subvarsup ~passive_pos ~active_pos:with_pos with
                | Some c -> 
                  Util.debugf ~section 2 "svs: @[%a@]@. @[%a@]. @[%a@]@." 
                    (fun k -> k C.pp with_pos.clause C.pp clause C.pp c);
                  Iter.cons c acc
                | None -> acc end
              | _ -> acc)
            Iter.empty
      )
    |> Iter.to_rev_list


  (* ----------------------------------------------------------------------
   * Equality Resolution rule
   * ---------------------------------------------------------------------- *)

  let infer_equality_resolution_aux ~unify ~iterate_substs clause =
    let _span = ZProf.enter_prof prof_infer_equality_resolution in
    let eligible = C.Eligible.filter (fun lit -> not @@ Lit.is_predicate_lit lit) in
    (* iterate on those literals *)
    let new_clauses =
      Lits.fold_eqn ~sign:false ~ord ~both:false ~eligible (C.lits clause)
      |> Iter.filter_map
        (fun (l, r, _, l_pos) ->
          let do_eq_res us =
            let pos = Lits.Pos.idx l_pos in
            (* CCFormat.printf "trying %d@." pos; *)
            let eligible = BV.get (C.eligible_res_no_subst clause) pos in
            if eligible  
            (* subst(lit) is maximal, we can do the inference *)
            then (
              Util.incr_stat stat_equality_resolution_call;
              let renaming = Subst.Renaming.create () in
              let subst = US.subst us in
              let rule = Proof.Rule.mk "eq_res" in
              let new_lits = CCArray.except_idx (C.lits clause) pos in
              let new_lits = Lit.apply_subst_list renaming subst (new_lits,0) in
              let c_guard = Literal.of_unif_subst renaming us in
              let subst_is_ho = 
                  Subst.codomain subst
                  |> Iter.exists (fun (t,_) -> 
                      Iter.exists (fun t -> T.is_fun t || T.is_comb t) 
                        (T.Seq.subterms ~include_builtin:true (T.of_term_unsafe t))) in
              let tags = (if subst_is_ho then [Proof.Tag.T_ho] else []) @ Unif_subst.tags us in
              let trail = C.trail clause in
              let penalty = if C.penalty clause = 1 then 1 else C.penalty clause + 1 in
              let proof = Proof.Step.inference ~rule ~tags
                  [C.proof_parent_subst renaming (clause,0) subst] in
              let new_clause = C.create ~trail ~penalty (c_guard@new_lits) proof in
              (* CCFormat.printf "success: @[%a@]@." C.pp new_clause; *)
              Util.debugf ~section 2 "@[<hv2>equality resolution on@ @[%a@]@ yields @[%a@],\n subst @[%a@]@]"
                (fun k->k C.pp clause C.pp new_clause US.pp us);
              Some new_clause
            ) else None
          in
          let substs = unify (l, 0) (r, 0) in
          iterate_substs substs do_eq_res
        )
      |> Iter.to_rev_list
    in
    ZProf.exit_prof _span;
    new_clauses

  let infer_equality_resolution c =
    infer_equality_resolution_aux
      ~unify:(fun l r -> 
        try Some (Unif.FO.unify_full l r) 
        with Unif.Fail -> None)
      ~iterate_substs:(fun substs do_eq_res -> CCOpt.flat_map do_eq_res substs) c

  let infer_equality_resolution_complete_ho clause =
    let inf_res = infer_equality_resolution_aux
        ~unify:(Env.flex_get k_unif_alg)
        ~iterate_substs:(fun substs do_eq_res -> Some (OSeq.map (CCOpt.flat_map do_eq_res) substs))
        clause
    in
    if Env.should_force_stream_eval () then (
      Env.get_finite_infs inf_res
    ) else (
      let cls, stm_res = force_getting_cl (List.map (fun stm -> 
        C.penalty clause, [clause], stm)  inf_res) in
      StmQ.add_lst (Env.get_stm_queue ()) stm_res; 
      cls)

  (* ----------------------------------------------------------------------
   * Equality Factoring rule
   * ---------------------------------------------------------------------- *)

  module EqFactInfo = struct
    type t = {
      clause : C.t;
      active_idx : int;
      is_pred_var_eq_fact : bool;
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
    let s = info.s and t = info.t and v = info.v and idx = info.active_idx in
    let us = info.subst in
    (* check whether subst(lit) is maximal, and not (subst(s) < subst(t)) *)
    let renaming = S.Renaming.create () in
    let subst = US.subst us in

    if ((Env.flex_get k_pred_var_eq_fact 
          && info.is_pred_var_eq_fact 
          && C.proof_depth info.clause < 2
          && not (T.is_true_or_false t)) ||
        (O.compare ord (S.FO.apply renaming subst (s, info.scope)) 
        (S.FO.apply renaming subst (t, info.scope)) <> Comp.Lt
        && CCList.for_all (fun (c, i) -> i = idx) (C.selected_lits info.clause)
        && CCList.is_empty (C.bool_selected info.clause)
        && C.is_maxlit (info.clause,info.scope) subst ~idx))
    then (
      let subst_is_ho = 
        Subst.codomain subst
        |> Iter.exists (fun (t,_) -> 
            Iter.exists (fun t -> T.is_fun t || T.is_comb t) 
              (T.Seq.subterms ~include_builtin:true (T.of_term_unsafe t))) in
      let tags = (if subst_is_ho then [Proof.Tag.T_ho] else []) @ Unif_subst.tags us in
      Util.incr_stat stat_equality_factoring_call;
      let proof =
        Proof.Step.inference
          ~rule:(Proof.Rule.mk"eq_fact") ~tags
          [C.proof_parent_subst renaming (info.clause,0) subst]
      (* new_lits: literals of the new clause. remove active literal
         and replace it by a t!=v one, and apply subst *)
      and new_lits = CCArray.except_idx (C.lits info.clause) idx in
      let new_lits = Lit.apply_subst_list renaming subst (new_lits,info.scope) in
      let c_guard = Literal.of_unif_subst renaming us in
      let lit' = Lit.mk_neq
          (S.FO.apply renaming subst (t, info.scope))
          (S.FO.apply renaming subst (v, info.scope))
      in
      let new_lits = lit' :: c_guard @ new_lits in
      let penalty = if C.penalty info.clause = 1 then 1 else C.penalty info.clause + 1 in
      let new_clause =
        C.create ~trail:(C.trail info.clause) ~penalty new_lits proof
      in
      Util.debugf ~section 3 "@[<hv2>equality factoring on@ @[%a@]@ yields @[%a@]@]"
        (fun k->k C.pp info.clause C.pp new_clause);

      Some new_clause
    ) else(None)

  let infer_equality_factoring_aux ~unify ~iterate_substs clause =
    let _span = ZProf.enter_prof prof_infer_equality_factoring in
    let eligible = C.Eligible.(filter Lit.eqn_sign) in
    (* find root terms that are unifiable with s and are not in the
       literal at s_pos. Calls [k] with a position and substitution *)
    let find_unifiable_lits idx s _s_pos k =
      Array.iteri
        (fun i lit ->
           match lit with
           | _ when i = idx -> () (* same index *)
           | Lit.Equation (u, v, sign) ->
             if sign then (
               k (u, v, unify (s,0) (u,0));
               k (v, u, unify (s,0) (v,0))
             )
           | _ -> () (* ignore other literals *)
        ) (C.lits clause)
    in
    (* try to do inferences with each positive literal *)
    let new_clauses =
      Lits.fold_eqn ~ord ~both:true ~eligible (C.lits clause)
      |> Iter.flat_map
        (fun (s, t, _, s_pos) -> (* try with s=t *)
           let active_idx = Lits.Pos.idx s_pos in
           find_unifiable_lits active_idx s s_pos
           |> Iter.filter_map
             (fun (u,v,substs) ->
               let is_pred_var_eq_fact =
                (T.is_app_var s && Type.is_prop (T.ty s))
                || (T.is_app_var u && Type.is_prop (T.ty u))
               in
               iterate_substs substs
                 (fun subst ->
                     let info = EqFactInfo.({
                         clause; s; t; u; v; active_idx; subst; scope=0; is_pred_var_eq_fact;
                       }) in
                   do_eq_factoring info)))
      |> Iter.to_rev_list
    in
    ZProf.exit_prof _span;
    new_clauses

  let infer_equality_factoring c =
    infer_equality_factoring_aux
      ~unify:(fun s t ->
        try Some (Unif.FO.unify_full s t) 
        with Unif.Fail -> 
      None)
      ~iterate_substs:(fun subst do_eq_fact -> CCOpt.flat_map do_eq_fact subst) c

  let infer_equality_factoring_complete_ho clause =
    let inf_res = infer_equality_factoring_aux
        ~unify:(Env.flex_get k_unif_alg)
        ~iterate_substs:(fun substs do_eq_fact -> Some (OSeq.map (CCOpt.flat_map do_eq_fact) substs))
        clause
    in
    if Env.should_force_stream_eval () then (
      Env.get_finite_infs inf_res
    ) else (
      let cls, stm_res = force_getting_cl (List.map (fun stm -> 
        C.penalty clause, [clause], stm)  inf_res) in
      StmQ.add_lst (Env.get_stm_queue ()) stm_res;
      cls)

  (* ----------------------------------------------------------------------
   * extraction of a clause from the stream queue (HO feature)
   * ---------------------------------------------------------------------- *)

  let extract_from_stream_queue ~full () =
    let _span = ZProf.enter_prof prof_queues in
    let cl =
      if full then
        StmQ.take_fair_anyway (Env.get_stm_queue ())
      else
        StmQ.take_stm_nb (Env.get_stm_queue ())
    in
    let opt_res = CCOpt.sequence_l (List.filter CCOpt.is_some cl)  in
    ZProf.exit_prof _span;
    match opt_res with
    | None -> []
    | Some l ->  l

  let extract_from_stream_queue_fix_stm ~full () =
    let _span = ZProf.enter_prof prof_queues in
    let cl =
      if full then
        StmQ.take_fair_anyway (Env.get_stm_queue ())
      else
        StmQ.take_stm_nb_fix_stm (Env.get_stm_queue ())
    in
    let opt_res = CCOpt.sequence_l (List.filter CCOpt.is_some cl) in
    ZProf.exit_prof _span;
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

  type demod_state = {
    mutable demod_clauses: (C.t * Subst.t * Scoped.scope) list; (* rules used *)
    mutable demod_sc: Scoped.scope; (* current scope *)
  }

  (** Compute normal form of term w.r.t active set. Clauses used to
      rewrite are added to the clauses hashset. *)
  let demod_nf (st:demod_state) c t : T.t =
    (* compute normal form of subterm. If `toplevel` is true, we need an extra check 
      whether the rewriting clause is smaller than the rewritten clause. *)
    let rec reduce_at_root ~toplevel t k =
      (* find equations l=r that match subterm *)
      let cur_sc = st.demod_sc in
      assert (cur_sc > 0);
      let step =
        UnitIdx.retrieve ~sign:true (!_idx_simpl, cur_sc) (t, 0)
        |> Iter.find
          (fun (l, r, (_,_,sign,unit_clause), subst) ->
             let expand_quant = not @@ Env.flex_get Combinators.k_enable_combinators in
             let norm t = Lambda.eta_reduce ~expand_quant @@ Lambda.snf t in
             let norm_b t = T.normalize_bools @@ norm t in
             (* r' is the subterm is going to be rewritten into *)
             let r' = norm @@ Subst.FO.apply Subst.Renaming.none subst (r,cur_sc) in
             assert (C.is_unit_clause unit_clause);
             (* NOTE: The conditions of Schulz's "braniac" paper cannot be 
                justified by the standard redundancy criterion.
                Instead, we check whether the rewriting clause is smaller 
                than the rewritten clause. *)
             (* Only perform the rewrite if: *)
             if (* - The rewriting clause is positive *)
                sign &&
                (* - The clause trails are compatible *)
                C.trail_subsumes unit_clause c &&
                (* - The clauses are distinct *)
                not (C.equal unit_clause c) &&
                (* - The rewriting clause is smaller than the rewritten clause *)
                (not toplevel ||
                C.lits c |> CCArray.exists (fun lit -> Lit.Seq.terms lit |> 
                  Iter.exists (fun s -> O.compare ord s t == Comp.Gt)) ||
                C.lits c |> CCArray.exists (fun lit -> match Literal.View.as_eqn lit with
                    | Some (litl, litr, true) -> 
                      T.equal t litl && O.compare ord litr r' == Comp.Gt || 
                      T.equal t litr && O.compare ord litl r' == Comp.Gt
                    | Some (litl, litr, false) -> T.equal t litl || T.equal t litr
                    | None -> false)
                ) &&
                (* - subst(l) > subst(r) *)
                (O.compare ord
                   (S.FO.apply Subst.Renaming.none subst (l,cur_sc))
                   (S.FO.apply Subst.Renaming.none subst (r,cur_sc)) = Comp.Gt)
             then (
               Util.debugf ~section 3
                 "@[<hv2>demod(%d):@ @[<hv>t=%a[%d],@ l=%a[%d],@ r=%a[%d]@],@ subst=@[%a@]@]"
                 (fun k->k (C.id c) T.pp t 0 T.pp l cur_sc T.pp r cur_sc S.pp subst);

               let t' = Lambda.eta_expand @@ norm_b t in
               let l' = Lambda.eta_expand @@ norm_b @@ Subst.FO.apply Subst.Renaming.none subst (l,cur_sc) in               
               (* sanity checks *)
               assert (Type.equal (T.ty l) (T.ty r));
               assert (T.equal l' t');
               st.demod_clauses <-
                 (unit_clause,subst,cur_sc) :: st.demod_clauses;
               st.demod_sc <- 1 + st.demod_sc; (* allocate new scope *)
               Util.incr_stat stat_demodulate_step; (* reduce [rhs] in current scope [cur_sc] *)
               assert (cur_sc < st.demod_sc);
               (* bind variables not occurring in [rhs] to fresh ones *)
               let subst = 
                 (InnerTerm.Seq.vars (r :> InnerTerm.t)) 
                 |> Iter.fold (fun subst v -> 
                     if S.mem subst (v, cur_sc) 
                     then subst 
                     else S.bind subst (v, cur_sc) 
                         (InnerTerm.var (HVar.fresh ~ty:(HVar.ty v) ()), cur_sc)) 
                   subst in
               Util.debugf ~section 2
                 "@[<2>demod(%d):@ rewrite `@[%a@]`@ into `@[%a@]`@ resulting `@[%a@]`@ nf `@[%a@]` using %a[%d]@]"
                 (fun k->k (C.id c) T.pp t T.pp r T.pp r' T.pp (Lambda.snf r') Subst.pp subst cur_sc);
               Some r'
             ) else (
             
             Util.debugf ~section 2 "demodulation of @[%a@] using @[%a@]=@[%a@] failed@."
              (fun k -> k T.pp t T.pp l T.pp r);

             None))
      in
      begin match step with
        | None -> k t (* not found any match, normal form found *)
        | Some r' ->
          (* NOTE: we retraverse the term several times, but this is simpler *)
          normal_form ~toplevel r' k (* done one rewriting step, continue *)
      end
    (* rewrite innermost-leftmost of [subst(t,scope)]. The initial scope is
       0, but then we normal_form terms in which variables are really the variables
       of the RHS of a previously applied rule (in context !sc); all those
       variables are bound to terms in context 0 
       
       when rewriting under quantifiers whose bodies are lambdas, we need to
       force lambda rewriting (force_lam_rw) to rewrite the quantifier body
       *)
    and normal_form ~toplevel t k =
      match T.view t with
      | T.Const _ -> reduce_at_root ~toplevel t k
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
               reduce_at_root ~toplevel t' k)
        else reduce_at_root ~toplevel t k
      | T.Fun (ty_arg, body) ->
        (* reduce under lambdas *)
        if Env.flex_get k_lambda_demod
        then normal_form ~toplevel:false body
              (fun body' ->
                let u = if T.equal body body' then t else T.fun_ ty_arg body' in
                reduce_at_root ~toplevel u k)
        else reduce_at_root ~toplevel t k (* TODO: DemodExt *)
      | T.Var _ | T.DB _ -> k t
      | T.AppBuiltin(Builtin.(ForallConst|ExistsConst) as hd, [_; body]) ->
        if not (Env.flex_get k_quant_demod) then (
          reduce_at_root ~toplevel t k
        ) else (
          let mk_quant = if hd = ForallConst then T.Form.forall else T.Form.exists in
          let vars,unfolded = T.open_fun body in
          normal_form ~toplevel:false unfolded
            (fun unfolded' ->
              let u = 
                if T.equal unfolded unfolded' then t 
                else mk_quant (T.fun_l vars unfolded') in
              reduce_at_root ~toplevel u k))
      | T.AppBuiltin (b, l) ->
        normal_form_l l
          (fun l' ->
             let u =
               if T.same_l l l' then t else T.app_builtin ~ty:(T.ty t) b l'
             in
             reduce_at_root ~toplevel u k)
    and normal_form_l l k = match l with
      | [] -> k []
      | t :: tail ->
        normal_form ~toplevel:false t
          (fun t' ->
             normal_form_l tail
               (fun l' -> k (t' :: l')))
    in
    normal_form ~toplevel:true t (fun t->t)

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

    (* demodulate every literal *)
    let demod_lit i lit = Lit.map (fun t -> demod_nf st c t) lit in
    let lits = Array.mapi demod_lit (C.lits c) in
    if CCList.is_empty st.demod_clauses then (
      (* no rewriting performed *)
      (* Util.debugf ~section 1 "did not demod @[%a@]@." (fun k -> k C.pp c); *)
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
      assert(C.lits new_c |> Literals.vars_distinct);
      SimplM.return_new new_c
    )

  let demodulate c = 
    assert (Term.VarSet.for_all (fun v -> HVar.id v >= 0) (Literals.vars (C.lits c) |> Term.VarSet.of_list));
    ZProf.with_prof prof_demodulate demodulate_ c

  let local_rewrite c =
    try
      assert(Env.flex_get k_local_rw != `Off);

    let neqs, others =
      CCArray.fold_left (fun (neq_map, others) lit ->
        match lit with
        | Literal.Equation(lhs,rhs,sign) ->
          (* NOTE: based on the representation of the literals! *)
          if sign && T.is_true_or_false rhs && (not (T.is_var lhs)) then (
            let negate t = if T.equal t T.true_ then T.false_ else T.true_ in
            (T.Map.add lhs (negate rhs) neq_map, others)
          ) else if not sign then (
            match Ordering.compare ord lhs rhs with
            | Gt -> (T.Map.add lhs rhs neq_map, others)
            | Lt -> (T.Map.add rhs lhs neq_map, others)
            | _ -> ((neq_map), lit::others)
          ) else ((neq_map), lit::others)
        | _ -> ((neq_map), lit::others)
      ) ((Term.Map.empty),[]) (C.lits c) in
    
    let normalize ~restrict ~neqs t =
        let only_green_ctx = Env.flex_get k_local_rw == `GreenContext in
        
        let rec aux ~top t =
          (match T.Map.get t neqs with
          | Some t' when not restrict || not top -> 
            assert(Type.equal (T.ty t) (T.ty t'));
            aux ~top t'
          | _ ->
            begin
              match T.view t with
              | T.App(hd, args) when not (T.is_var hd) || not only_green_ctx ->
                let hd' = aux ~top:false hd in
                let args' = List.map (aux ~top:false) args in
                if T.equal hd hd' && T.same_l args args' then t
                else aux ~top:false (T.app hd' args')
              | T.AppBuiltin(hd, args) ->
                let args' = List.map (aux ~top:false) args in
                if T.same_l args args' then t
                else aux ~top:false (T.app_builtin ~ty:(T.ty t) hd args')
              | T.Fun _ when not only_green_ctx ->
                let pref,body = T.open_fun t in
                let body' = aux ~top:false body in
                if T.equal body body' then t
                else T.fun_l pref body'
              | _ -> t (* do not rewrite under lambdas *)
            end) in
        aux ~top:true t in

      let rewritten = ref false in
      let new_lits =
        CCArray.map (function
        | Lit.Equation(lhs,rhs,sign) as l ->
          if sign && T.is_true_or_false rhs then (
            let lhs' = normalize ~restrict:true ~neqs lhs in
            if not (T.equal lhs lhs') then (
              rewritten := true;
              Lit.mk_lit lhs' rhs sign
            ) else l
          ) else if not sign then (
            let lhs', rhs' = 
              match Ordering.compare ord lhs rhs with
              | Gt -> normalize ~restrict:true ~neqs lhs, normalize ~restrict:false ~neqs rhs
              | Lt -> normalize ~restrict:false ~neqs lhs, normalize ~restrict:true ~neqs rhs
              | _ -> normalize ~restrict:false ~neqs lhs, normalize ~restrict:false ~neqs rhs 
            in
            if not (T.equal lhs lhs') || not (T.equal rhs rhs') then (
              rewritten := true;
              Lit.mk_lit lhs' rhs' sign
            ) else l
          ) else (
            let lhs',rhs' = normalize ~restrict:false ~neqs lhs, normalize ~restrict:false ~neqs rhs in
            if not (T.equal lhs lhs') || not (T.equal rhs rhs') then (
                rewritten := true;
                Lit.mk_lit lhs' rhs' sign
            ) else l
          )
        | x -> x) (C.lits c) in
      
      if not !rewritten then SimplM.return_same c
      else (
        let new_lits = CCArray.to_list new_lits in
        let proof = Proof.Step.simp [C.proof_parent c] ~rule:(Proof.Rule.mk "local_rewriting") in
        let new_c = C.create ~trail:(C.trail c) ~penalty:(C.penalty c) new_lits proof in
        Util.debugf ~section 2 "local_rw(@[%a@]):@.@[%a@]@." (fun k -> k C.pp c C.pp new_c);
        SimplM.return_new new_c
      )
    with Invalid_argument err ->
      CCFormat.printf "err in local_rw:@[%s@]@." err;
      CCFormat.printf "proof: @[%a@]@." Proof.S.pp_tstp (C.proof c);
      CCFormat.printf "local_rw: @[%a@]@." C.pp c;
      Format.print_flush ();
      assert false

    

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

  (** Find clauses that [given] may demodulate, add them to set *)
  let backward_demodulate set given =
    let _span = ZProf.enter_prof prof_back_demodulate in
    let renaming = Subst.Renaming.create () in
    (* find clauses that might be rewritten by l -> r *)
    let recurse ~oriented set l r =
      I.retrieve_specializations (!_idx_back_demod,1) (l,0)
      |> Iter.fold
        (fun set (_t',with_pos,subst) ->
           let c = with_pos.C.WithPos.clause in
           (* subst(l) matches t' and is > subst(r), very likely to rewrite! *)
           if (C.trail_subsumes c given && (oriented ||
                                            O.compare ord
                                              (S.FO.apply renaming subst (l,0))
                                              (S.FO.apply renaming subst (r,0)) = Comp.Gt
                                           )
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
    ZProf.exit_prof _span;
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
           (* NOTE: Based on the representation of the literals *)
           | Lit.Equation (l, r, true) when T.equal r T.false_ ->
             Congruence.FO.mk_eq cc l T.true_
           | Lit.Equation (l, r, false) ->
             Congruence.FO.mk_eq cc l r
           | _ -> cc)
        cc (C.lits c)
    in
    let res = CCArray.exists
        (function
          (* NOTE: Based on the representation of the literals *)
          | Lit.Equation (l, r, _) as lit when Lit.is_positivoid lit ->
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
       CCArray.exists Lit.is_negativoid (C.lits c) &&
       CCArray.exists Lit.is_positivoid (C.lits c)
    then is_semantic_tautology_real c
    else false

  let is_semantic_tautology c =
    ZProf.with_prof prof_semantic_tautology is_semantic_tautology_ c

  let var_in_subst_ us v sc =
    S.mem (US.subst us) ((v:T.var:>InnerTerm.t HVar.t),sc)

  let basic_simplify c =
    if C.get_flag flag_simplified c
    then SimplM.return_same c
    else (
      let _span = ZProf.enter_prof prof_basic_simplify in
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
      if Env.flex_get k_destr_eq_res then (
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
                assert(not (T.is_true_or_false r));
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
              | _ -> ())
          lits
      );
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
        ZProf.exit_prof _span;
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
        ZProf.exit_prof _span;
        SimplM.return_new new_clause
      )
    )

  let handle_distinct_constants lit =
    match lit with
    | Lit.Equation (l, r, sign) when T.is_const l && T.is_const r ->
      assert(not (T.is_true_or_false r));
      let s1 = T.head_exn l and s2 = T.head_exn r in
      if ID.is_distinct_object s1 && ID.is_distinct_object s2
      then
        if sign = (ID.equal s1 s2)
        then Some (Lit.mk_tauto,[],[Proof.Tag.T_distinct])  (* "a" = "a", or "a" != "b" *)
        else Some (Lit.mk_absurd,[],[Proof.Tag.T_distinct]) (* "a" = "b" or "a" != "a" *)
      else None
    | _ -> None

  exception FoundMatch of T.t * C.t * S.t

  let formula_simplify_reflect c =
    let q_sc,idx_sc = 0,1 in
    let used_units = ref C.ClauseSet.empty in

    let do_sr t =
      let simplify ~sign lhs rhs =
        let (<+>) = CCOpt.(<+>) in
        let top_level ~sign ~repl lhs rhs = 
          UnitIdx.retrieve ~sign (!_idx_simpl, idx_sc) (lhs, q_sc)
          |> Iter.find_map (fun (_, rhs', (_,_,_,c'), subst) ->
              if C.trail_subsumes c' c then (
                try
                  ignore(Unif.FO.matching ~subst ~pattern:(rhs', idx_sc) (rhs, q_sc));
                  used_units := C.ClauseSet.add c' !used_units;
                  Some repl
                with _ -> None
              ) else None)
        in
        let nested lhs rhs =
          T.Seq.common_contexts lhs rhs
          |> Iter.find_map (fun (lhs, rhs) ->
            top_level ~sign:true ~repl:(if sign then T.true_ else T.false_) lhs rhs
            <+> top_level ~sign:true ~repl:(if sign then T.true_ else T.false_) rhs lhs)
        in

        top_level ~sign ~repl:T.true_ lhs rhs
        <+> top_level ~sign:(not sign) ~repl:T.false_ lhs rhs
        <+> top_level ~sign ~repl:T.true_ rhs lhs
        <+> top_level ~sign:(not sign) ~repl:T.false_ rhs lhs
        <+> nested lhs rhs
      in

      let rec aux t =
        match T.view t with
        | T.App(hd, args) ->
          let args' = List.map aux args in
          if T.same_l args args' then t
          else T.app hd args'
        | T.AppBuiltin((Eq|Neq|Equiv|Xor) as hd, ([_; x; y]|[x;y]))
          when Type.is_prop (T.ty t) && T.DB.is_closed x && T.DB.is_closed y ->
          let (x',y') = CCPair.map_same aux (x,y) in
          assert(Type.equal (T.ty x) (T.ty x'));
          assert(Type.equal (T.ty y) (T.ty y'));
          let sign = Builtin.equal Eq hd || Builtin.equal Equiv hd in
          begin match simplify ~sign x' y' with
          | Some t -> t
          | None ->
            if (not ((T.equal x x') && (T.equal y y'))) then (
              if (Builtin.equal hd Eq) then T.Form.eq x' y'
              else if (Builtin.equal hd Neq) then T.Form.neq x' y'
              else T.app_builtin ~ty:(T.ty t) hd [x'; y']
            ) else t 
          end
        | T.AppBuiltin(hd, args) when not (Builtin.is_quantifier hd) ->
          let args' = List.map aux args in
          if T.same_l args args' then t
          else T.app_builtin ~ty:(T.ty t) hd args'
        | T.Fun(ty, body) ->
          let body' = aux body in
          if T.equal body body' then t
          else T.fun_ ty body'
        | _ -> t
      in
      aux t
    in

    if Env.flex_get k_formula_simplify_reflect && !Lazy_cnf.enabled then(
      let lits = List.map (function 
        | Lit.Equation(lhs,rhs,sign) ->
          Lit.mk_lit (do_sr lhs) (do_sr rhs) sign
        | x -> x
      ) (CCArray.to_list @@ C.lits c) in
      if (not @@ C.ClauseSet.is_empty !used_units) then (
        let parents = List.map C.proof_parent (C.ClauseSet.to_list !used_units) in
        let proof =
          Proof.Step.simp ~rule:(Proof.Rule.mk "inner_simplify_reflect")
            ((C.proof_parent c)::parents) in
        let trail = C.trail c and penalty = C.penalty c in
        let new_c = C.create ~trail ~penalty lits proof in
        SimplM.return_new new_c
      ) else (SimplM.return_same c)
    ) else (SimplM.return_same c)

  let equatable ~sign ~cl s t  =
    let idx_sc, q_sc = 1, 0 in
    let (<+>) = CCOpt.(<+>) in
    let aux s t =
      UnitIdx.retrieve ~sign (!_idx_simpl, idx_sc) (s, q_sc)
      |> Iter.find_map (fun (_, rhs, (_,_,_,c'), subst) ->
          if C.trail_subsumes c' cl then (
            try
              ignore(Unif.FO.matching ~subst ~pattern:(rhs, idx_sc) (t, q_sc));
              Some c'
            with _ -> None
          ) else None)
    in
    aux s t <+> aux t s

  let positive_simplify_reflect c =
    let driver ~is_simplified c =
      let kept_lits = CCBV.create ~size:(C.length c) true in
      let premises = 
        CCArray.foldi (fun premises i lit   -> 
          let find_simplifying_premise lhs rhs = 
            begin match is_simplified lhs rhs with
            | Some prems -> 
              CCBV.reset kept_lits i;
              C.ClauseSet.union premises prems
            | None -> premises end
          in
          
          match lit with
          | Lit.Equation(lhs, rhs, false) -> find_simplifying_premise lhs rhs
          | Lit.Equation(lhs, rhs, true) when T.equal T.false_ rhs ->
            find_simplifying_premise lhs T.true_
          | _ -> premises
        ) (C.ClauseSet.empty) (C.lits c)
      in
      CCOpt.return_if (not (CCBV.is_empty (CCBV.negate kept_lits)))
        (CCBV.select kept_lits (C.lits c), premises)
    in

    let strong_sr_pair lhs rhs =
      let tasks = Queue.create () in
      let exception CantSimplify in
      try
        Queue.push (lhs, rhs) tasks;
        let premises = ref C.ClauseSet.empty in
        while not (Queue.is_empty tasks) do
          let s,t = Queue.pop tasks in
          if not (T.equal s t) then (
            match equatable ~sign:true ~cl:c s t with
            | Some cl -> premises := C.ClauseSet.add cl !premises
            | None -> 
              begin match T.view s, T.view t with
              | T.App(hd_s, args_s), T.App(hd_t, args_t)
                when T.is_const hd_s && T.equal hd_s hd_t ->
                CCList.iter (fun pair -> Queue.push pair tasks) 
                  (List.combine args_s args_t) 
              | T.AppBuiltin(hd_s, args_s), T.AppBuiltin(hd_t, args_t)
                when Builtin.equal hd_s hd_t && 
                    CCList.length args_s = CCList.length args_t ->
                CCList.iter (fun pair -> Queue.push pair tasks) 
                  (List.combine args_s args_t) 
              | _ -> raise CantSimplify end
          )
        done;
        Some !premises
      with CantSimplify -> None
    in

    let regular_sr_pair lhs rhs =
      if T.equal lhs rhs then Some (C.ClauseSet.empty)
      else match equatable ~sign:true ~cl:c lhs rhs with
           | Some cl -> Some (C.ClauseSet.singleton cl)
           | None -> (T.Seq.common_contexts lhs rhs 
                     |> Iter.find_map (fun (a,b) -> equatable ~sign:true ~cl:c a b)
                     |> CCOpt.map C.ClauseSet.singleton)
    in

    let do_strong_sr = driver ~is_simplified:strong_sr_pair in
    let do_regular_sr = driver ~is_simplified:regular_sr_pair in
    
    let simplifier =
      if Env.flex_get k_strong_sr then do_strong_sr else do_regular_sr
    in
    let _span = ZProf.enter_prof prof_pos_simplify_reflect in
    (* iterate through literals and try to resolve negative ones *)
    match simplifier c with
    | None -> 
      ZProf.exit_prof _span;
      SimplM.return_same c
    | Some (new_lits,premises) ->
      let proof =
        Proof.Step.simp ~rule:(Proof.Rule.mk "simplify_reflect+")
          (List.map C.proof_parent (c::(C.ClauseSet.to_list premises))) in
      let trail = C.trail c and penalty = C.penalty c in
      let new_c = C.create ~trail ~penalty new_lits proof in
      Util.debugf ~section 3 "@[@[%a@]@ pos_simplify_reflect into @[%a@]@]"
        (fun k->k C.pp c C.pp new_c);
      ZProf.exit_prof _span;
      SimplM.return_new new_c

  let negative_simplify_reflect c =
    let _span = ZProf.enter_prof prof_neg_simplify_reflect in
    (* iterate through literals and try to resolve positive ones *)
    let rec iterate_lits acc lits clauses = match lits with
      | [] -> List.rev acc, clauses
      | (Lit.Equation (s, t, true) as lit)::lits' ->
        begin match can_refute s t with
          | None -> (* keep literal *)
            iterate_lits (lit::acc) lits' clauses
          | Some new_clause -> (* drop literal, remember clause *)
            iterate_lits acc lits' (new_clause :: clauses)
        end
      | lit::lits' -> iterate_lits (lit::acc) lits' clauses
    (* try to remove the literal using a negative unit clause *)
    and can_refute s t =
      equatable ~sign:false ~cl:c s t
      |> CCOpt.map C.proof_parent
    in
    (* fold over literals *)
    let lits, premises = iterate_lits [] (C.lits c |> Array.to_list) [] in
    if List.length lits = Array.length (C.lits c)
    then (
      (* no literal removed *)
      ZProf.exit_prof _span;
      Util.debug ~section 3 "neg_reflect did not simplify the clause";
      SimplM.return_same c
    ) else (
      let proof =
        Proof.Step.simp
          ~rule:(Proof.Rule.mk "simplify_reflect-")
          (C.proof_parent c :: premises) in
      let new_c = C.create ~trail:(C.trail c) ~penalty:(C.penalty c) lits proof in
      Util.debugf ~section 3 "@[@[%a@]@ neg_simplify_reflect into @[%a@]@]"
        (fun k->k C.pp c C.pp new_c);
      ZProf.exit_prof _span;
      SimplM.return_new new_c
    )

  type sgn = Lit of bool | Eqn
  let flex_resolve c =
    let exception CantFlexResolve in
    try
      let sgn_map = T.Tbl.create 8 in

      if (C.length c == 0) then raise CantFlexResolve;

      Array.iter (function 
        | Literal.Equation(lhs, rhs, _) as lit ->
          if (Lit.is_predicate_lit lit) then (
            assert(T.is_true_or_false rhs);
            if (T.is_var (T.head_term lhs)) then(
              let hd = T.head_term lhs in
              match T.Tbl.get sgn_map hd with
              | None -> T.Tbl.add sgn_map hd (Lit (Lit.is_positivoid lit))
              | Some (Lit sgn) when sgn == Lit.is_positivoid lit ->  ()
              | _ -> raise CantFlexResolve
            ) else raise CantFlexResolve) 
          else if Lit.is_positivoid lit then (raise CantFlexResolve)
          else (
            let hd_lhs, hd_rhs = CCPair.map_same T.head_term (lhs,rhs) in
            if (T.is_var hd_lhs) && (T.is_var hd_rhs) then (
              match T.Tbl.get sgn_map hd_lhs, T.Tbl.get sgn_map hd_rhs with
              | None, None | Some(Eqn), None
              | None, Some(Eqn) | Some(Eqn), Some(Eqn) ->
                T.Tbl.replace sgn_map hd_lhs Eqn;
                T.Tbl.replace sgn_map hd_rhs Eqn;
              | _ -> raise CantFlexResolve
            ) else raise CantFlexResolve
          )
        | False -> ()
        | _ -> raise CantFlexResolve
      ) (C.lits c);

      let new_cl = 
        C.create ~trail:(C.trail c) [] 
                  (Proof.Step.simp ~rule:(Proof.Rule.mk "flex_resolve") [C.proof_parent c])
                  ~penalty:1 in
      SimplM.return_new new_cl
    with CantFlexResolve -> SimplM.return_same c
  

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
        Util.debugf ~section 5 "(@[<hv>subsumes@ :c1 @[%a@]@ :c2 @[%a@]@ :subst %a%a@]"
          (fun k->k Lits.pp a Lits.pp b Subst.pp subst Proof.pp_tags !tags);
        Some (subst, !tags)
    )

  let subsumes_with a b =
    let _span = ZProf.enter_prof prof_subsumption in
    Util.incr_stat stat_subsumption_call;
    let (c_a, _), (c_b, _) = a,b in
    let w_a = CCArray.fold (fun acc l -> acc + Lit.weight l) 0 c_a in
    let w_b = CCArray.fold (fun acc l -> acc + Lit.weight l) 0 c_b in

    if w_a = w_b && Literals.equal_com c_a c_b then Some (Subst.empty, [])
    else (
      let res = if w_a <= w_b then subsumes_with_ a b else None in
      ZProf.exit_prof _span;
      res
    )

  let subsumes_classic a b = match subsumes_with (a,0) (b,1) with
    | None -> false
    | Some _ -> true

  let subsumes a b = 
    let module SS = SolidSubsumption.Make(struct let st = Env.flex_state () end) in

    if not @@ Env.flex_get k_solid_subsumption 
       || Env.flex_get Combinators.k_enable_combinators
    then subsumes_classic a b 
    else (
      try 
        SS.subsumes a b
      with SolidSubsumption.UnsupportedLiteralKind -> 
        subsumes_classic a b)

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
    let _span = ZProf.enter_prof prof_eq_subsumption in
    Util.incr_stat stat_eq_subsumption_call;
    let res = match a with
      | [|Lit.Equation (s, t, true)|] ->
        let res = CCArray.find_map (equate_lit_with s t) b in
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
    ZProf.exit_prof _span;
    res

  let eq_subsumes a b = CCOpt.is_some (eq_subsumes_with (a,1) (b,0))

  let subsumed_by_active_set c =
    let _span = ZProf.enter_prof prof_subsumption_set in
    Util.incr_stat stat_subsumed_by_active_set_call;
    (* if there is an equation in c, try equality subsumption *)
    let try_eq_subsumption = CCArray.exists Lit.is_eqn (C.lits c) in
    (* use feature vector indexing *)
    let c = if Env.flex_get k_ground_subs_check > 0 then  C.ground_clause c else c in
    let res =
      SubsumIdx.retrieve_subsuming_c !_idx_fv c
      |> Iter.exists
        (fun c' ->
           let res = 
             C.trail_subsumes c' c
             &&
             ( (try_eq_subsumption && eq_subsumes (C.lits c') (C.lits c))
               ||
               subsumes (C.lits c') (C.lits c)
             ) in
           if res  then (
            Util.debugf ~section 2 "@[<2>@[%a@]@ subsumed by @[%a@]@]" (fun k->k C.pp c C.pp c');
            Util.incr_stat stat_clauses_subsumed;
           );
           res)
    in
    ZProf.exit_prof _span;
    res

  let subsumed_in_active_set acc c =
    let _span = ZProf.enter_prof prof_subsumption_in_set in
    Util.incr_stat stat_subsumed_in_active_set_call;
    (* if c is a single unit clause *)
    let try_eq_subsumption =
      C.is_unit_clause c && Lit.is_positivoid (C.lits c).(0)
    in
    (* use feature vector indexing *)
    let res =
      SubsumIdx.retrieve_subsumed_c !_idx_fv c
      |> Iter.fold
        (fun res c' ->
           if C.trail_subsumes c c'
           then
             let c' = if Env.flex_get k_ground_subs_check > 1 then C.ground_clause c' else c' in
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
    ZProf.exit_prof _span;
    res

  (* Number of equational lits. Used as an estimation for the difficulty of the subsumption
     check for this clause. *)
  let num_equational lits =
    Array.fold_left
      (fun acc lit -> 
        acc + (if Lit.is_predicate_lit lit then 0 else 1)
      ) 0 lits

  (* ----------------------------------------------------------------------
   * contextual literal cutting
   * ---------------------------------------------------------------------- *)

  (* Performs successive contextual literal cuttings *)
  let rec contextual_literal_cutting_rec c =
    let open SimplM.Infix in
    if Array.length (C.lits c) <= 1
    || Lits.num_equational (C.lits c) > 3
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
            Proof.Step.simp
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
    let res = ZProf.with_prof prof_clc contextual_literal_cutting_rec c in
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
    || Lits.num_equational (C.lits c) > 3
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
    ZProf.with_prof prof_condensation condensation_rec c
  
  let subsumption_weight c =
    C.Seq.terms c
    |> Iter.fold (fun acc t -> (T.weight ~var:1 ~sym:(fun _ -> 2) t) + acc ) 0

  let immediate_subsume c immediate =
    let subsumes subsumer subsumee =
      (subsumption_weight subsumer <= subsumption_weight subsumee) &&
      C.trail_subsumes subsumer subsumee &&
      ((Array.exists Lit.is_eqn (C.lits subsumee) && 
          eq_subsumes (C.lits subsumer) (C.lits subsumee)) ||
        subsumes (C.lits subsumer) (C.lits subsumee)) in

    let immediate = Iter.filter (fun c' -> not (subsumes c c')) immediate in
    if Iter.exists C.is_empty immediate then None 
    else (
      immediate
      |> Iter.find_map (fun c' -> 
        if (subsumes c' c) then (
          C.mark_redundant c;
          Env.remove_active (Iter.singleton c);
          Env.remove_simpl (Iter.singleton c);
          Util.debugf ~section 2 "immediate subsume @[%a@]@." (fun k -> k C.pp c);
          Some c'
        ) else None))
    |> (function 
        | Some subsumer -> Some (Iter.singleton subsumer)
        | None -> Some immediate)

  let is_orphaned c =
    let res = not (C.is_empty c) && C.is_orphaned c in
    if res then (
      Util.incr_stat stat_orphan_checks
    );
    res

  let recognize_injectivity c =
    let exception Fail in

    (* avoiding cascading if-then-elses *)
    let fail_on condition =
      if condition then raise Fail in

    let find_in_args var args =
      fst @@ CCOpt.get_or ~default:(-1, T.true_)
        (CCList.find_idx (T.equal var) args) in

    try 
      fail_on (C.length c != 2);

      match C.lits c with
      | [|lit1; lit2|] ->
        fail_on (not ((Lit.is_positivoid lit1 || Lit.is_positivoid lit2) &&
                     (Lit.is_negativoid lit1 || Lit.is_negativoid lit2)));

        let pos_lit,neg_lit = 
          if Lit.is_positivoid lit1 then lit1, lit2 else lit2,lit1 in
       
        begin match pos_lit, neg_lit with
        | Equation(x,y,true), Equation(lhs,rhs,sign) ->
          fail_on (not (T.is_var x && T.is_var y));
          fail_on (T.equal x y);

          let (hd_lhs, lhs_args), (hd_rhs, rhs_args) = 
            CCPair.map_same T.as_app_mono (lhs,rhs) in
          
          fail_on (not (T.is_const hd_lhs && T.is_const hd_rhs));
          fail_on (not (T.equal hd_lhs hd_rhs));
          fail_on (not (List.length lhs_args == List.length rhs_args));

          fail_on (not ((find_in_args x lhs_args) != (-1) ||
                        (find_in_args x rhs_args) != (-1)));
          fail_on (not ((find_in_args y lhs_args) != (-1) ||
                        (find_in_args y rhs_args) != (-1)));
          
          (* reorient equations so that x appears in lhs *)
          let lhs,rhs,lhs_args,rhs_args =
            if find_in_args x lhs_args != -1 
            then (lhs, rhs, lhs_args, rhs_args)
            else (rhs, lhs, rhs_args, lhs_args) in

          fail_on (find_in_args x lhs_args != find_in_args y rhs_args);
          
          let same_vars, diff_eqns = List.fold_left (fun (same, diff) (s,t) -> 
            fail_on (not (T.is_var s && T.is_var t));
            if T.equal s t then (s :: same, diff)
            else (same, (s,t)::diff)
          ) ([],[]) (List.combine lhs_args rhs_args) in

          let same_set = T.Set.of_list same_vars in
          let diff_lhs_set, diff_rhs_set = 
            CCPair.map_same T.Set.of_list (CCList.split diff_eqns) in
          
          (* variables in each group are unique *)
          fail_on (List.length same_vars != T.Set.cardinal same_set);
          fail_on (List.length diff_eqns != T.Set.cardinal diff_lhs_set);
          fail_on (List.length diff_eqns != T.Set.cardinal diff_rhs_set);

          (* variable groups do not intersect *)
          fail_on (not (T.Set.is_empty (T.Set.inter diff_lhs_set diff_rhs_set)));
          fail_on (not (T.Set.is_empty (T.Set.inter diff_lhs_set same_set)));
          fail_on (not (T.Set.is_empty (T.Set.inter diff_rhs_set same_set)));

          let (sk_id, sk_ty),inv_sk = 
            Term.mk_fresh_skolem 
              (List.map T.as_var_exn same_vars) 
              (Type.arrow [T.ty lhs] (T.ty x)) in
          let inv_sk = T.app inv_sk [lhs] in
          let inv_lit = [Lit.mk_eq inv_sk x] in

           let proof = Proof.Step.inference ~rule:(Proof.Rule.mk "inj_rec") 
              [C.proof_parent c] in
          Ctx.declare sk_id sk_ty;
          let new_clause = 
            C.create ~trail:(C.trail c) ~penalty:(C.penalty c) inv_lit proof in
          Util.debugf ~section 2 "Injectivity recognized: %a |---| %a" 
            (fun k -> k C.pp c C.pp new_clause);
          [new_clause]
        | _ -> assert false; end
      | _ -> assert false;
    with Fail -> []
  
  let normalize_equalities c =
    let lits = Array.to_list (C.lits c) in
    let normalized = List.map Literal.normalize_eq lits in
    if List.exists CCOpt.is_some normalized then (
      let new_lits = List.mapi (fun i l_opt -> 
          CCOpt.get_or ~default:(Array.get (C.lits c) i) l_opt) normalized in
      let proof = Proof.Step.simp [C.proof_parent c] 
          ~rule:(Proof.Rule.mk "simplify nested equalities")  in
      let new_c = C.create ~trail:(C.trail c) ~penalty:(C.penalty c) new_lits proof in
      SimplM.return_new new_c
    ) 
    else (
      SimplM.return_same c 
    )
  
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
      >>= formula_simplify_reflect
    and active_simplify c =
      condensation c
      >>= contextual_literal_cutting
    and backward_simplify c =
      let set = C.ClauseSet.empty in
      backward_demodulate set c
    and redundant = subsumed_by_active_set
    and backward_redundant = subsumed_in_active_set
    and is_trivial = is_tautology in

    Env.add_basic_simplify normalize_equalities;
    Env.add_basic_simplify flex_resolve;
    if Env.flex_get k_local_rw != `Off then (
      Env.add_basic_simplify local_rewrite
    );

    if Env.flex_get Combinators.k_enable_combinators
       && Env.flex_get k_subvarsup then (
      Env.add_binary_inf "subvarsup" infer_subvarsup_active;
      Env.add_binary_inf "subvarsup" infer_subvarsup_passive;
    );

    if Env.flex_get k_switch_stream_extraction then (
      Env.add_generate ~priority:0 "stream_queue_extraction" extract_from_stream_queue_fix_stm)
    else (
      Env.add_generate ~priority:0 "stream_queue_extraction" extract_from_stream_queue
    );

    if Env.flex_get k_recognize_injectivity then (
      Env.add_unary_inf "recognize injectivity" recognize_injectivity;
    );

    if Env.flex_get k_ho_basic_rules
    then (
      Env.add_binary_inf "superposition_passive" infer_passive_complete_ho;
      Env.add_binary_inf "superposition_active" infer_active_complete_ho;
      Env.add_unary_inf "equality_factoring" infer_equality_factoring_complete_ho;
      Env.add_unary_inf "equality_resolution" infer_equality_resolution_complete_ho;
    
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
    if Env.flex_get k_immediate_simplification then (
      Env.add_immediate_simpl_rule immediate_subsume
    );
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
let _sup_from_var_headed = ref true
let _sup_in_var_args = ref true
let _sup_under_lambdas = ref true
let _lambda_demod = ref false
let _quant_demod = ref false
let _demod_in_var_args = ref true
let _dot_demod_into = ref None
let _ho_basic_rules = ref false
let _switch_stream_extraction = ref false
let _fluidsup_penalty = ref 9
let _dupsup_penalty = ref 2
let _fluidsup = ref true
let _subvarsup = ref true
let _dupsup = ref true
let _recognize_injectivity = ref false
let _restrict_fluidsup = ref false
let _check_sup_at_var_cond = ref true
let _restrict_hidden_sup_at_vars = ref false
let _local_rw = ref `Off
let _destr_eq_res = ref true

let _lambdasup = ref (-1)
let _max_infs = ref (-1)

let _unif_alg = ref `NewJPFull
let _unif_level = ref `Full
let _ground_subs_check = ref 0
let _solid_subsumption = ref false

let _skip_multiplier = ref 4.0
let _imit_first = ref false
let _unif_logop_mode = ref `Pragmatic
let _max_depth = ref 2
let _max_rigid_imitations = ref 2
let _max_app_projections = ref 1
let _max_elims = ref 0
let _max_identifications = ref 0
let _pattern_decider = ref true
let _fixpoint_decider = ref false
let _solid_decider = ref false
let _solidification_limit = ref 3
let _max_unifs_solid_ff = ref 60
let _use_weight_for_solid_subsumption = ref false
let _sort_constraints = ref false
let _bool_demod = ref false
let _immediate_simplification = ref false
let _try_lfho_unif = ref true
let _rw_w_formulas = ref false
let _pred_var_eq_fact = ref false
let _schedule_infs = ref true
let _force_limit = ref 3
let _formula_sr = ref true
let _strong_sr = ref false
let _superposition_with_formulas = ref false


let _guard = ref 30
let _ratio = ref 100
let _clause_num = ref (-1)

let key = Flex_state.create_key ()


let unif_params_to_def () =
  _max_depth := 2;
  _max_app_projections := 1;
  _max_rigid_imitations := 2;
  _max_identifications := 0;
  _max_elims           := 0;
  _max_infs := 5

let register ~sup =
  let module Sup = (val sup : S) in
  let module E = Sup.Env in

  E.update_flex_state (Flex_state.add key sup);
  (* in general all unif algs in Zip are terminating, except for JP *)
  E.flex_add PragUnifParams.k_unif_alg_is_terminating true;
  E.flex_add k_sup_at_vars !_sup_at_vars;
  E.flex_add k_sup_in_var_args !_sup_in_var_args;
  E.flex_add k_sup_under_lambdas !_sup_under_lambdas;
  E.flex_add k_sup_at_var_headed !_sup_at_var_headed;
  E.flex_add k_sup_from_var_headed !_sup_from_var_headed;
  E.flex_add k_fluidsup !_fluidsup;
  E.flex_add k_subvarsup !_subvarsup;
  E.flex_add k_dupsup !_dupsup;
  E.flex_add k_lambdasup !_lambdasup;
  E.flex_add k_quant_demod !_quant_demod;
  E.flex_add k_restrict_fluidsup !_restrict_fluidsup;
  E.flex_add k_check_sup_at_var_cond !_check_sup_at_var_cond;
  E.flex_add k_restrict_hidden_sup_at_vars !_restrict_hidden_sup_at_vars;
  E.flex_add k_demod_in_var_args !_demod_in_var_args;
  E.flex_add k_lambda_demod !_lambda_demod;

  E.flex_add k_use_simultaneous_sup !_use_simultaneous_sup;  
  E.flex_add k_fluidsup_penalty !_fluidsup_penalty;
  E.flex_add k_dupsup_penalty !_dupsup_penalty;
  E.flex_add k_ground_subs_check !_ground_subs_check;
  E.flex_add k_solid_subsumption !_solid_subsumption;
  E.flex_add k_dot_sup_into !_dot_sup_into;
  E.flex_add k_dot_sup_from !_dot_sup_from;
  E.flex_add k_dot_simpl !_dot_simpl;
  E.flex_add k_dot_demod_into !_dot_demod_into;
  E.flex_add k_recognize_injectivity !_recognize_injectivity;
  E.flex_add k_ho_basic_rules !_ho_basic_rules;
  E.flex_add k_max_infs !_max_infs;
  E.flex_add k_switch_stream_extraction !_switch_stream_extraction;
  E.flex_add k_dont_simplify !_dont_simplify;
  E.flex_add k_use_semantic_tauto !_use_semantic_tauto;
  E.flex_add k_bool_demod !_bool_demod;
  E.flex_add k_immediate_simplification !_immediate_simplification;
  E.flex_add k_rw_with_formulas !_rw_w_formulas;


  E.flex_add PragUnifParams.k_max_inferences !_max_infs;
  E.flex_add PragUnifParams.k_skip_multiplier !_skip_multiplier;
  E.flex_add PragUnifParams.k_imit_first !_imit_first;
  E.flex_add PragUnifParams.k_logop_mode !_unif_logop_mode;
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
  E.flex_add PragUnifParams.k_sort_constraints !_sort_constraints;
  E.flex_add PragUnifParams.k_try_lfho !_try_lfho_unif;
  E.flex_add PragUnifParams.k_schedule_inferences !_schedule_infs;
  E.flex_add k_pred_var_eq_fact !_pred_var_eq_fact;
  E.flex_add k_force_limit !_force_limit;
  E.flex_add k_formula_simplify_reflect !_formula_sr;
  E.flex_add k_superpose_w_formulas !_superposition_with_formulas;

  E.flex_add StreamQueue.k_guard !_guard;
  E.flex_add StreamQueue.k_ratio !_ratio;
  E.flex_add StreamQueue.k_clause_num !_clause_num;

  E.flex_add k_local_rw !_local_rw;
  E.flex_add k_destr_eq_res !_destr_eq_res;
  E.flex_add k_strong_sr !_strong_sr;

  let module JPF = JPFull.Make(struct let st = E.flex_state () end) in
  let module JPP = PUnif.Make(struct let st = E.flex_state () end) in
  E.flex_add k_unif_module (module JPF : UnifFramework.US);
  begin match !_unif_alg with 
    | `OldJP -> 
      E.flex_add k_unif_alg JP_unif.unify_scoped;
      E.flex_add PragUnifParams.k_unif_alg_is_terminating false;
    | `NewJPFull -> 
      E.flex_add k_unif_alg JPF.unify_scoped;
      E.flex_add PragUnifParams.k_unif_alg_is_terminating false;
    | `NewJPPragmatic -> 
      E.flex_add k_unif_alg JPP.unify_scoped;
      E.flex_add k_unif_module (module JPP : UnifFramework.US);
       end

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
                         prio=5;
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
      "--sup-from-var-headed", Arg.Bool (fun b -> _sup_from_var_headed := b), " enable/disable superposition from variable headed terms";
      "--sup-in-var-args", Arg.Bool (fun b -> _sup_in_var_args := b), " enable/disable superposition in arguments of applied variables";
      "--sup-under-lambdas", Arg.Bool (fun b -> _sup_under_lambdas := b), " enable/disable superposition in bodies of lambda-expressions";
      "--lambda-demod", Arg.Bool (fun b -> _lambda_demod := b), " enable/disable demodulation in bodies of lambda-expressions";
      "--quant-demod", Arg.Bool (fun b -> _quant_demod := b), " enable/disable demodulation in bodies of quantifiers";
      "--demod-in-var-args", Arg.Bool (fun b -> _demod_in_var_args := b), " enable demodulation in arguments of variables";
      "--ho-basic-rules", Arg.Bool (fun b -> _ho_basic_rules := b), " enable/disable HO version of base superposition calculus rules";
      "--switch-stream-extract", Arg.Bool (fun b -> _switch_stream_extraction := b), " in ho mode, switches heuristic of clause extraction from the stream queue";
      "--fluidsup-penalty", Arg.Int (fun p -> _fluidsup_penalty := p), " penalty for FluidSup inferences";
      "--dupsup-penalty", Arg.Int (fun p -> _dupsup_penalty := p), " penalty for DupSup inferences";
      "--fluidsup", Arg.Bool (fun b -> _fluidsup :=b), " enable/disable FluidSup inferences (only effective when complete higher-order unification is enabled)";
      "--subvarsup", Arg.Bool ((:=) _subvarsup), " enable/disable SubVarSup inferences";
      "--lambdasup", Arg.Int (fun l -> 
          if l < 0 then 
            raise (Util.Error ("argument parsing", 
                               "lambdaSup argument should be non-negative"));
          _lambdasup := l), 
      " enable LambdaSup -- argument is the maximum number of skolems introduced in an inference";
      "--dupsup", Arg.Bool (fun v -> _dupsup := v), " enable/disable DupSup inferences";
      "--rw-with-formulas", Arg.Bool (fun v -> _rw_w_formulas := v), " enable/disable rewriting with formulas";
      "--ground-before-subs", Arg.Set_int _ground_subs_check, " set the level of grounding before substitution. 0 - no grounding. 1 - only active. 2 - both.";
      "--solid-subsumption", Arg.Bool (fun v -> _solid_subsumption := v), " set solid subsumption on or off";
      "--recognize-injectivity", Arg.Bool (fun v -> _recognize_injectivity := v), " recognize injectivity axiom and axiomatize corresponding inverse";
      "--restrict-fluidsup" , Arg.Bool (fun v -> _restrict_fluidsup := v), " enable/disable restriction of fluidSup to up to two literal or inital clauses";
      "--use-weight-for-solid-subsumption", Arg.Bool (fun v -> _use_weight_for_solid_subsumption := v), 
      " enable/disable superposition to and from pure variable equations";
      "--ho-unif-level",
      Arg.Symbol (["full-framework";"full"; "pragmatic-framework";], (fun str ->
          _unif_alg := if (String.equal "full" str) then `OldJP
            else if (String.equal "full-framework" str) then (`NewJPFull)
            else if (String.equal "pragmatic-framework" str) then (
            unif_params_to_def (); 
            `NewJPPragmatic)
            else invalid_arg "unknown argument")), "set the level of HO unification";
      "--ho-imitation-first",Arg.Bool (fun v -> _imit_first:=v), " Use imitation rule before projection rule";
      "--ho-unif-logop-mode",Arg.Symbol (["conservative"; "pragmatic"; "off"], 
        (function | "conservative" -> _unif_logop_mode := `Conservative
                  | "pragmatic" -> _unif_logop_mode := `Pragmatic
                  | _ -> _unif_logop_mode := `Off)), " Choose level of AC reasoning on logical symbols in unification algorithm";
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
      "--max-inferences", Arg.Int (fun p -> _max_infs := p), " set maximal number of inferences";
      "--stream-queue-guard", Arg.Set_int _guard, "set value of guard for streamQueue";
      "--stream-queue-ratio", Arg.Set_int _ratio, "set value of ratio for streamQueue";
      "--bool-demod", Arg.Bool ((:=) _bool_demod), " turn BoolDemod on/off";
      "--schedule-inferences", Arg.Bool ((:=) _schedule_infs), " schedule inferences into streams even when terminating unification is used";
      "--destr-eq-res", Arg.Bool ((:=) _destr_eq_res), " turn destructive equality resolution on/off";
      "--pred-var-eq-fact", Arg.Bool ((:=) _pred_var_eq_fact), " force equality factoring when one side is applied variable";
      "--local-rw", Arg.Symbol (["any-context"; "green-context"; "off"], (fun opt -> 
        match opt with
        | "any-context" -> _local_rw := `AnyContext
        | "green-context" -> _local_rw := `GreenContext
        | "off" -> _local_rw := `Off
        | _ -> invalid_arg "possible arugments are: [any-context; green-context; off]"
      )), " turn local rewriting rule on/off";
      "--immediate-simplification", Arg.Bool ((:=) _immediate_simplification), " turn immediate simplification on/off";
      "--try-lfho-unif", Arg.Bool ((:=) _try_lfho_unif), " if term is of the right shape, try LFHO unification before HO unification";
      "--stream-clause-num", Arg.Set_int _clause_num, "how many clauses to take from streamQueue; by default as many as there are streams";
      "--ho-sort-constraints", Arg.Bool (fun b -> _sort_constraints := b), "sort constraints in unification algorithm by weight";
      "--check-sup-at-var-cond", Arg.Bool (fun b -> _check_sup_at_var_cond := b), " enable/disable superposition at variable monotonicity check";
      "--restrict-hidden-sup-at-vars", Arg.Bool (fun b -> _restrict_hidden_sup_at_vars :=	b), " enable/disable hidden superposition at variables only under certain ordering conditions";
      "--stream-force-limit", Arg.Int((:=) _force_limit), " number of attempts to get a clause when the stream is just created";
      "--formula-simplify-reflect", Arg.Bool((:=) _formula_sr), " apply simplify reflect on the formula level";
      "--superposition-with-formulas", Arg.Bool((:=) _superposition_with_formulas), 
        " enable superposition from (negative) formulas into any subterm";
      "--strong-simplify-reflect", Arg.Bool((:=) _strong_sr), " full effort simplify reflect -- tries to find an equation for each pair of subterms";
    ];

  Params.add_to_mode "ho-complete-basic" (fun () ->
      _use_simultaneous_sup := false;
      _local_rw := `GreenContext;
      _destr_eq_res := false;
      _unif_logop_mode := `Conservative;
      _sup_at_vars := true;
      _sup_in_var_args := false;
      _sup_under_lambdas := false;
      _lambda_demod := false;
      _demod_in_var_args := false;
      _ho_basic_rules := true;
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
      _local_rw := `GreenContext;
      _demod_in_var_args := false;
      _ho_basic_rules := true;
      _unif_alg := `NewJPPragmatic;
      _sup_at_var_headed := true;
      _pred_var_eq_fact := false;
      _lambdasup := -1;
      _dupsup := false;
      _max_infs := 4;
      _max_depth := 2;
      _max_app_projections := 0;
      _max_rigid_imitations := 2;
      _max_identifications := 1;
      _max_elims := 0;
      _fluidsup := false;
    );
  Params.add_to_mode "ho-competitive" (fun () ->
      _use_simultaneous_sup := false;
      _sup_at_vars := true;
      _sup_in_var_args := false;
      _sup_under_lambdas := false;
      _lambda_demod := false;
      _demod_in_var_args := false;
      _ho_basic_rules := true;
      _unif_alg := `NewJPFull;
      _local_rw := `GreenContext;
      _sup_at_var_headed := true;
      _pred_var_eq_fact := true;
      _lambdasup := -1;
      _dupsup := false;
      _fluidsup := false;
    );
  Params.add_to_mode "fo-complete-basic" (fun () ->
      _use_simultaneous_sup := false;
      _local_rw := `Off;
      _destr_eq_res := false;
      _unif_logop_mode := `Conservative;
      _schedule_infs := false;
    );
  Params.add_to_modes 
    [ "lambda-free-intensional"
    ; "lambda-free-extensional"
    ; "ho-comb-complete"
    ; "lambda-free-purify-intensional"
    ; "lambda-free-purify-extensional"] (fun () ->
    _use_simultaneous_sup := false;
    _sup_in_var_args := true;
    _unif_logop_mode := `Conservative;
    _demod_in_var_args := true;
    _local_rw := `GreenContext;
    _dupsup := false;
    _ho_basic_rules := false;
    _destr_eq_res := false;
    _lambdasup := -1;
    _fluidsup := false;
  );
  Params.add_to_modes 
    [ "lambda-free-extensional"
    ; "ho-comb-complete"
    ; "lambda-free-purify-extensional"] (fun () ->
    _restrict_hidden_sup_at_vars := true;
  );
  Params.add_to_modes 
    [ "lambda-free-intensional"
    ; "lambda-free-purify-intensional"] (fun () ->
    _restrict_hidden_sup_at_vars := false;
  );
  Params.add_to_modes
    [ "lambda-free-intensional"
    ; "lambda-free-extensional"
    ; "ho-comb-complete"] (fun () ->
      _sup_at_vars := true;
  );
  Params.add_to_modes
    [ "lambda-free-purify-intensional"
    ; "lambda-free-purify-extensional"] (fun () ->
      _sup_at_vars := false;
      _check_sup_at_var_cond := false;
  );

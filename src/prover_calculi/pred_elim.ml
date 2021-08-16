 
(* This file is free software, part of Zipperposition. See file "license" for more details. *)

(** {1 Blocked Clause Elimination} *)

open Logtk
open Libzipperposition

let k_enabled = Flex_state.create_key ()
let k_check_at = Flex_state.create_key ()
let k_inprocessing = Flex_state.create_key ()
let k_max_resolvents = Flex_state.create_key ()
let k_check_gates = Flex_state.create_key ()
let k_only_original_gates = Flex_state.create_key ()
let k_only_non_conjecture_gates = Flex_state.create_key ()
let k_check_gates_semantically = Flex_state.create_key ()
let k_non_singular_pe = Flex_state.create_key ()
let k_measure_fun = Flex_state.create_key ()
let k_relax_val = Flex_state.create_key ()
let k_prefer_spe = Flex_state.create_key ()
let k_fp_mode = Flex_state.create_key ()

let _enabled = ref false
let _check_gates = ref true
let _inprocessing = ref false
let _check_at = ref 10
let _max_resolvents = ref (-1)
let _non_singular_pe = ref false
let _relax_val = ref 0
let _prefer_spe = ref false
let _measure_name = ref "relaxed"
let _original_gates_only = ref false
let _only_non_conj_gates = ref false
let _check_semantically = ref false


let section = Util.Section.make ~parent:Const.section "pred-elim"

module A = Libzipperposition_avatar
module OrigEnv = Env

module type S = sig
  module Env : Env.S

  (** {6 Registration} *)
  val setup : ?in_fp_mode:bool -> unit -> unit
  val begin_fixpoint : unit -> unit
  val fixpoint_step : unit -> bool
  val end_fixpoint : unit -> unit
end

let register_parameters env =
  let module E = (val env : Env.S) in
  E.flex_add k_enabled !_enabled;
  E.flex_add k_check_at !_check_at;
  E.flex_add k_inprocessing !_inprocessing;
  E.flex_add k_max_resolvents !_max_resolvents;
  E.flex_add k_check_gates !_check_gates;
  E.flex_add k_only_original_gates !_original_gates_only;
  E.flex_add k_only_non_conjecture_gates !_only_non_conj_gates;
  E.flex_add k_check_gates_semantically !_check_semantically;
  E.flex_add k_non_singular_pe !_non_singular_pe;
  E.flex_add k_relax_val !_relax_val;
  E.flex_add k_prefer_spe !_prefer_spe


module Make(E : Env.S) : S with module Env = E = struct
  module Env = E
  module C = Env.C
  module CS = C.ClauseSet
  module L = Literal
  module T = Term
  module SAT = Sat_solver.Make ()

  type logic = 
    | NEqFO  (* nonequational FO *)
    | EqFO (* equational FO *)
    | NonAppVarHo (* higher-order logic, but at the top level
                     each literal has only (fully applied)
                     function symbols *)
    | Unsupported   (* HO or FO with theories *)
  
  let log_to_int = 
    [(NEqFO, 0); (EqFO, 1); (NonAppVarHo, 2); (Unsupported, 3)]
  let log_compare (l1:logic) (l2:logic) =
    compare (List.assoc l1 log_to_int) (List.assoc l2 log_to_int)

  exception UnsupportedLogic
  
  type pred_elim_info =
  {
    sym : ID.t;
    (* clauses with single pos/neg occurrence of a symbol *)
    mutable pos_cls : CS.t;
    mutable neg_cls : CS.t;
    (* clauses with multiple occurrences of a symbol *)
    mutable offending_cls : CS.t;
    (* clauses that have the gate shape (occurs in pos/neg cls)  *)
    mutable possible_gates : CS.t;
    (* do the clauses in the possible_gates form a gate, and if so which one? *)
    mutable is_gate : (C.t list * C.t list) option;
    (* square of number of different variables stored in this clause set *)
    mutable sq_var_weight : float;
    (* number of literals stored in this clause set *)
    mutable num_lits : int;
    (* what was this number during the last check *)
    mutable last_check : (float * int) option;
    mutable heap_idx : int;
    mutable deleted  : bool;
  }

  let card t =
    CS.cardinal t.pos_cls + CS.cardinal t.neg_cls +
    (match t.is_gate with
    | None -> 0
    | Some (p, n) -> List.length p + List.length n)
    + CS.cardinal t.offending_cls

  let pp_task out task =
    let original = ID.payload_pred 
        ~f:(function  ID.Attr_cnf_def -> true 
                      | _ -> false) task.sym in
    CCFormat.fprintf out 
      "%a(%b) {@. +: @[%a@];@. -:@[%a@];@. ?:@[%a@]@. g:@[%a@]@. v^2:@[%g@]; |l|:@[%d@]; |%a|:@[%d@]; h_idx: @[%d@] @.}@."
      ID.pp task.sym original (CS.pp C.pp) task.pos_cls (CS.pp C.pp) task.neg_cls
      (CS.pp C.pp) task.offending_cls (CCOpt.pp (CCPair.pp (CCList.pp C.pp) (CCList.pp C.pp))) task.is_gate task.sq_var_weight
      task.num_lits ID.pp task.sym (card task) task.heap_idx

  let copy_task t = 
    let c = {
      sym = t.sym; pos_cls = CS.empty; neg_cls = CS.empty; offending_cls = CS.empty;
      possible_gates = CS.empty; is_gate = None; sq_var_weight = 0.0;
      num_lits = 0; last_check = None; heap_idx = -1; deleted = false;
    } in
    c.pos_cls <- t.pos_cls; c.neg_cls <- t.neg_cls; 
    c.offending_cls <- t.offending_cls; c.possible_gates <- t.possible_gates;
    c.sq_var_weight <- t.sq_var_weight; c.num_lits <- t.num_lits;
    c.last_check <- t.last_check; c.heap_idx <- t.heap_idx;
    c


  module TaskWrapper = struct
    type t = pred_elim_info
    let idx task = task.heap_idx
    let set_idx task idx = 
      task.heap_idx <- idx
    let lt a b =
      assert((not a.deleted) || (not b.deleted));
      if not a.deleted && not b.deleted then (
          let open CCOrd in
          (compare (card a) (card b)
          <?> (compare, (not (CCOpt.is_some a.is_gate)), (not (CCOpt.is_some b.is_gate)))
          <?> (compare, a.num_lits, b.num_lits)
          <?> (compare, a.sq_var_weight, b.sq_var_weight)
          <?> (ID.compare, a.sym, b.sym)) < 0
       )
      else (a.deleted)
  end

  module TaskSet = struct  
    include MyHeap.Make(TaskWrapper) 

    (* a trick to remove an element in O(log (n)):
       make it the smallest, remove the smallest -- ugly hack*)
    let remove_el h el =
      assert (in_heap el);
      el.deleted <- true; (* implicitly makes it the smallest *)
      decrease h el;
      let min_el = remove_min h in
      assert(ID.equal min_el.sym el.sym);
      el.deleted <- false (* clear it for the next insertion *)

      

    let update h ~old ~new_ =
      assert(ID.equal old.sym new_.sym);
      assert(old.heap_idx == new_.heap_idx);
      assert(in_heap new_ && in_heap old);

      if TaskWrapper.lt new_ old then (decrease h new_) 
      else if TaskWrapper.lt old new_ then  (increase h new_)
      else ()
  end

  let k_measure_fun = Flex_state.create_key ()

  let _task_queue = TaskSet.create ()
  (* an optimization -- profiler showed that much time is spent in
     doing evaluation queue functions for added clauses -- instead
     we temporarily store the clauses in this set and then
     when we are sure that the clause is retained we 
     add it to the passive set *)
  let _newly_added = ref CS.empty
  (* clause set containing all the registered clauses. Makes sure that
     no clause is tracked or deleted multiple times from the system  *)
  let _tracked = ref CS.empty

  let _logic = ref NEqFO
  let refine_logic new_val =
    if !_logic != Unsupported then (
      _logic := new_val
    )
  
  let logic_to_str = function
    | EqFO -> "eq" | NEqFO -> "neq" | NonAppVarHo -> "non_appvar"
    | Unsupported -> "unsupported"
  
  let _ignored_symbols = ref ID.Set.empty

  let mk_pred_elim_info sym =
    {
      sym; pos_cls = CS.empty; neg_cls = CS.empty; 
      offending_cls=CS.empty; possible_gates = CS.empty;
      is_gate=None; sq_var_weight=0.0; last_check=None; heap_idx=(-1); num_lits=0; deleted=false;
    }
  
  let _pred_sym_idx = ref ID.Map.empty

  let get_sym_sign lit =
    match lit with
    | L.Equation(lhs,rhs,_) ->
      let sign = L.is_positivoid lit in
      let is_poly = 
        not (Type.VarSet.is_empty (T.ty_vars lhs))
        || not (Type.VarSet.is_empty (T.ty_vars rhs))
      in
      if not is_poly && not (Type.is_fun (T.ty lhs)) then (
        if Type.is_prop (T.ty lhs) then (
           if not (CCOpt.is_some (T.head lhs)) then (
             raise UnsupportedLogic;
           );

          if not (Term.is_fo_term lhs) then (
            refine_logic NonAppVarHo;
          );
          if L.is_predicate_lit lit then (
            let hd_sym = T.head_exn lhs in
            Some (hd_sym, sign)
          ) else (
            (* reasoning with formulas is currently unsupported *)
            Util.debugf ~section 1 "unsupported because of @[%a@]@." (fun k -> k L.pp lit);
            _logic := Unsupported;
            raise UnsupportedLogic;
        )) else (if T.is_fo_term lhs && T.is_fo_term rhs then refine_logic EqFO
                 else refine_logic NonAppVarHo; None)
      ) else (
        _logic := Unsupported; 
        Util.debugf ~section 1 "unsupported because of @[%a@]@." (fun k -> k L.pp lit);
        raise UnsupportedLogic)
    | _ -> None

  let remove_symbol entry =
    _pred_sym_idx := ID.Map.remove entry.sym !_pred_sym_idx;
    _ignored_symbols := ID.Set.add entry.sym !_ignored_symbols;
    if TaskSet.in_heap entry then (
      TaskSet.remove_el _task_queue entry 
    )

  let calc_sq_var cl =
    let n = List.length (Literals.vars (C.lits cl)) in
    float_of_int (n * n)

  let calc_new_stats resolvents = 
    List.fold_left (fun (acc_sq, acc_lit_num) cl -> 
      acc_sq +. calc_sq_var cl, acc_lit_num + C.length cl) (0.0, 0) resolvents
  
  let calc_num_cls task = 
    CS.cardinal task.pos_cls + CS.cardinal task.neg_cls +
    (match task.is_gate with
    | None -> 0
    | Some(ps, ns) -> List.length ps + List.length ns) +
    CS.cardinal task.offending_cls
  
  let kk_measure relax task resolvents =
    let new_mu, new_lit_num = calc_new_stats resolvents in
    task.num_lits >= new_lit_num && task.sq_var_weight >= new_mu

  let relaxed_measure relax task resolvents =
    let (new_mu, new_lit_num), new_cl_num =
      calc_new_stats resolvents, List.length resolvents in
    task.num_lits > new_lit_num-relax ||
    calc_num_cls task > new_cl_num-relax ||    
    task.sq_var_weight > new_mu

  let conservative_measure relax task resolvents =
   let (_, new_lit_num) = calc_new_stats resolvents in
   CS.cardinal (CS.filter C.is_unit_clause task.pos_cls) >= 2 ||
   CS.cardinal (CS.filter C.is_unit_clause task.neg_cls) >= 2 ||
   task.num_lits > new_lit_num - relax

  let _measure = ref relaxed_measure

  let should_schedule task =
    let eligible_for_non_singular_pe task =
      Env.flex_get k_non_singular_pe &&
      (match task.is_gate with
       | Some (pos,neg) ->
         let limit = 
          if Env.flex_get k_max_resolvents < 0 
          then max_int
          else Env.flex_get k_max_resolvents in
         let pos_num, neg_num = List.length pos, List.length neg in
         let max_resolvents =
          (CS.fold (fun cl num_res -> 
              if num_res == limit
              then limit (*shortcut *) 
              else (
                let new_res = CCArray.fold (fun acc lit -> 
                  match get_sym_sign lit with
                  | Some (sym,sign) when ID.equal task.sym sym ->
                    acc * (if sign then neg_num else pos_num)
                  | _ -> acc
                ) 1 (C.lits cl) in
                if new_res + num_res < limit then new_res + num_res else limit 
          )) task.offending_cls 0)
        in
        if max_resolvents < limit then true
        else (remove_symbol task; false)
       | None -> false)
    in
    
    CS.is_empty task.offending_cls ||
    eligible_for_non_singular_pe task

  let possibly_ignore_sym entry =
    let card s = CS.cardinal s in
    let possible_resolvents = 
      match entry.is_gate with
      | Some(pos_gates,neg_gates) ->
        List.length pos_gates * card entry.neg_cls +
        List.length neg_gates * card entry.pos_cls
      | None -> 
        card entry.neg_cls * card entry.pos_cls
    in
    if Env.flex_get k_max_resolvents < 0 then ()
    else (
      if possible_resolvents > Env.flex_get k_max_resolvents 
      then remove_symbol entry
    )
  
  let scan_cl_lits ?(handle_gates=true) cl =
    if !_logic == Unsupported then raise UnsupportedLogic;

    let num_vars = List.length @@ Literals.vars (C.lits cl) in
    let is_flat = function
    | L.Equation(lhs,_,_) as lit ->
      assert (L.is_predicate_lit lit);
      let args = T.args lhs in
      List.for_all T.is_var args &&
      T.Set.cardinal (T.Set.of_list args) == List.length args
    | _ -> assert false
    in

    let update_idx pos neg offending gates num_vars cl =
      let update ~action sym =
        _pred_sym_idx := ID.Map.update sym (fun old ->
          let entry = CCOpt.get_or ~default:(mk_pred_elim_info sym) old in
          let old_ = copy_task entry in
          begin match action with
          | `Pos ->
            entry.pos_cls <- CS.add cl entry.pos_cls;
          | `Neg ->
            entry.neg_cls <- CS.add cl entry.neg_cls;
            possibly_ignore_sym entry;
          | `Offending ->
            entry.offending_cls <- CS.add cl entry.offending_cls;
            if TaskSet.in_heap entry && (not (should_schedule entry)) then (
               TaskSet.remove_el _task_queue entry;
            )
          | `Gates ->
            if handle_gates then(
              entry.possible_gates <- CS.add cl entry.possible_gates
            )
          end;
          if action != `Gates then (
            entry.sq_var_weight <- entry.sq_var_weight +. calc_sq_var cl;
            entry.num_lits <- entry.num_lits + C.length cl
          );
          possibly_ignore_sym entry;
          if TaskSet.in_heap old_ && TaskSet.in_heap entry then (
            TaskSet.update _task_queue ~old:old_ ~new_:entry
          );
          if ID.Set.mem entry.sym !_ignored_symbols 
          then None 
          else Some entry
        ) !_pred_sym_idx
      in

      ID.Set.iter (update ~action:`Pos) pos;
      ID.Set.iter (update ~action:`Neg) neg;
      ID.Set.iter (update ~action:`Offending) offending;
      ID.Set.iter (update ~action:`Gates) gates;
    in

    try   
      let pos,neg,offending,gates = CCArray.foldi (fun ((pos,neg,offending,gates) as acc) idx lit ->
        let symbol_is_fresh sym =
          not (ID.Set.mem sym pos) && not (ID.Set.mem sym neg) &&
          not (ID.Set.mem sym offending) && not (ID.Set.mem sym !_ignored_symbols)
        in
        (match get_sym_sign lit with
        | Some (sym, sign) when symbol_is_fresh sym ->
          let is_offending = ref false in
          for i = idx+1 to (C.length cl)-1 do
            (match get_sym_sign (C.lits cl).(i) with
            | Some(sym', _) ->
              is_offending := !is_offending || ID.equal sym sym'
            | None -> () )
          done;
          if !is_offending then (
            pos,neg,ID.Set.add sym offending,gates
          ) else (
            let gates = if is_flat lit then ID.Set.add sym gates else gates in
            if sign then (ID.Set.add sym pos,neg,offending,gates)
            else (pos, ID.Set.add sym neg,offending,gates))
        | _ -> acc)
      ) (ID.Set.empty, ID.Set.empty, ID.Set.empty, ID.Set.empty) (C.lits cl) in

      update_idx pos neg offending gates num_vars cl
    with UnsupportedLogic ->
      refine_logic Unsupported;
      TaskSet.clear _task_queue

  let react_clause_addded cl =
    if !_logic != Unsupported then(
      if not (CS.mem cl !_tracked) then (
        Util.debugf ~section 5 "added:@[%a@]" (fun k -> k C.pp cl);
        _tracked := CS.add cl !_tracked;
        scan_cl_lits ~handle_gates:false cl);
      Signal.ContinueListening
    ) else Signal.StopListening
  
  let react_clause_removed cl =
    if !_logic != Unsupported then (
      let should_retry task =
        should_schedule task &&
        not (ID.Set.mem task.sym !_ignored_symbols) &&
        (match task.last_check with
        | Some(old_sq_var_sum, old_num_lit) -> 
          task.sq_var_weight < old_sq_var_sum || task.num_lits < old_num_lit
        | None -> true)
      in

      let handle_gate sign task cl =
        match task.is_gate with
        | Some(pos_cls, neg_cls) ->
          if sign && CCList.mem ~eq:C.equal cl pos_cls
             || (not sign) && CCList.mem ~eq:C.equal cl neg_cls then (
            (* reintroduce gate clauses *)
            task.pos_cls <- CS.add_list task.pos_cls pos_cls;
            task.neg_cls <- CS.add_list task.neg_cls neg_cls;
            if Env.flex_get k_non_singular_pe &&
               TaskSet.in_heap task &&
               not (CS.is_empty task.offending_cls) then (
              TaskSet.remove_el _task_queue task 
            );
            task.is_gate <- None
          )
        | None -> ()
      in

      let handled = ref ID.Set.empty in
      if not (CS.mem cl !_newly_added) &&
         CS.mem cl !_tracked then (
        _tracked := CS.remove cl !_tracked;
        Array.iteri (fun idx lit -> 
          match get_sym_sign lit with
          | Some(sym, sign) when not (ID.Set.mem sym !handled) ->
            let is_offending = ref false in
            for i = idx+1 to (C.length cl) -1 do
              match get_sym_sign (C.lits cl).(i) with
              | Some (sym', sign) ->
                is_offending := !is_offending || ID.equal sym sym'
              | None -> ()
            done;
            _pred_sym_idx := ID.Map.update sym (function
              | Some task ->
                let old = copy_task task in
                task.sq_var_weight <- task.sq_var_weight -. calc_sq_var cl;
                task.num_lits <- task.num_lits - C.length cl;
                if !is_offending then (
                  task.offending_cls <- CS.remove cl task.offending_cls
                ) else if sign then (
                  handle_gate sign task cl;
                  task.pos_cls <- CS.remove cl task.pos_cls;
                ) else (
                  handle_gate sign task cl;
                  task.neg_cls <- CS.remove cl task.neg_cls;
                );
                if not (TaskSet.in_heap task) && should_retry task then (
                  Util.debugf ~section 10 "retrying @[%a@]@." (fun k -> k pp_task task);
                  TaskSet.insert _task_queue task;
                );

                if TaskSet.in_heap old && TaskSet.in_heap task then (
                  TaskSet.update _task_queue ~new_:task ~old;
                );

                CCOpt.return_if (not (ID.Set.mem task.sym !_ignored_symbols)) task
              | None -> None (*probably the symbol became ignored*) ) !_pred_sym_idx;
          | _ -> ()
        ) (C.lits cl));
      Signal.ContinueListening)
    else Signal.StopListening

  let replace_clauses task clauses =
    Util.debugf ~section 2 "replaced clauses(%a):@. regular:@[%a@]@. gates:@[%a@]@." 
      (fun k -> k ID.pp task.sym (CS.pp C.pp) (CS.union task.pos_cls task.neg_cls) 
                  (CCOpt.pp (CCPair.pp (CCList.pp C.pp) (CCList.pp C.pp))) task.is_gate);
    Util.debugf ~section 2 "resolvents: @[%a@]@." (fun k -> k (CCList.pp C.pp) clauses);
    _ignored_symbols := ID.Set.add task.sym !_ignored_symbols;
    let remove iter =
      Env.remove_active iter;
      Env.remove_passive iter;
      Env.remove_simpl iter;
      Iter.iter (fun c -> (C.mark_redundant c);
        if CS.mem c !_newly_added then (
          _newly_added := CS.remove c !_newly_added;
          ignore (react_clause_removed c);)) 
      iter;
    in
    assert(CS.is_empty task.offending_cls || Env.flex_get k_non_singular_pe);
    remove (CS.to_iter task.offending_cls);
    remove (CS.to_iter task.pos_cls);
    remove (CS.to_iter task.neg_cls);
    (match task.is_gate with
    | Some(pos_cls, neg_cls) ->
      remove (CCList.to_iter pos_cls);
      remove (CCList.to_iter neg_cls)
    | None -> ());
    _newly_added := CS.add_list !_newly_added clauses;
    List.iter (fun cl -> ignore (react_clause_addded cl)) clauses;

    _pred_sym_idx := ID.Map.remove task.sym !_pred_sym_idx


  let is_tauto c =
    Literals.is_trivial (C.lits c) || Trail.is_trivial (C.trail c)

  let find_lit_by_sym sym sign cl =
    CCOpt.get_exn (CCArray.find_map_i (fun idx lit -> 
      match get_sym_sign lit with
      | Some (sym', sign') when ID.equal sym sym' && sign = sign' -> 
        Some (idx, CCOpt.get_exn (L.View.get_lhs lit))
      | _ -> None
    ) (C.lits cl))

  let neq_resolver ~sym ~pos_cl ~neg_cl =
    let pos_sc, neg_sc = 0, 1 in
    let pos_idx, pos_term = find_lit_by_sym sym true pos_cl in  
    let neg_idx, neg_term = find_lit_by_sym sym false neg_cl in
    try
      let subst = Unif.FO.unify_syn (pos_term, pos_sc) (neg_term, neg_sc) in
      let renaming = Subst.Renaming.create () in
      let lits = 
        (List.map (fun lit -> 
          L.apply_subst renaming subst (lit, pos_sc)) 
        (CCArray.except_idx (C.lits pos_cl) pos_idx)) @
        (List.map (fun lit -> 
          L.apply_subst renaming subst (lit, neg_sc)) 
        (CCArray.except_idx (C.lits neg_cl) neg_idx))
      in
      let proof =
        Proof.Step.simp 
          ~tags:[Proof.Tag.T_cannot_orphan]
          ~rule:(Proof.Rule.mk "dp-resolution") 
          [C.proof_parent_subst renaming (pos_cl,pos_sc) subst;
           C.proof_parent_subst renaming (neg_cl,neg_sc) subst]
      in
      let c =
        C.create ~penalty:(max (C.penalty pos_cl) (C.penalty neg_cl))
                 ~trail:(C.trail_l [pos_cl; neg_cl]) lits proof
      in
      CCOpt.return_if (not (is_tauto c)) c
    with Unif.Fail -> None
  
  let eq_resolver ~sym ~pos_cl ~neg_cl =
    let handle_distinct_vars xs sc_x ys sc_y =
      let is_unique xs = 
        List.for_all (Term.is_var) xs &&
        CCList.length (CCList.sort_uniq ~cmp:T.compare xs)
          == CCList.length xs
      in
      let mk_subst vars sc_vars terms sc_terms =
        List.fold_left (fun subst (v,t) -> 
          Subst.FO.bind' subst (T.as_var_exn v, sc_vars) (t, sc_terms)
        ) Subst.empty (CCList.combine vars terms)
      in

      if is_unique xs then Some (mk_subst xs sc_x ys sc_y) 
      else if is_unique ys then Some (mk_subst ys sc_y xs sc_x) 
      else None
    in

    let pos_sc, neg_sc = 0, 1 in
    let renaming = Subst.Renaming.create () in
    let pos_idx, pos_term = find_lit_by_sym sym true pos_cl in  
    let neg_idx, neg_term = find_lit_by_sym sym false neg_cl in
    let pos_args, neg_args = CCPair.map_same T.args (pos_term, neg_term) in
    let proof subst renaming =
      Proof.Step.simp 
        ~tags:[Proof.Tag.T_cannot_orphan] 
        ~rule:(Proof.Rule.mk "dp-resolution") 
        [C.proof_parent_subst renaming (pos_cl,pos_sc) subst;
         C.proof_parent_subst renaming (neg_cl,neg_sc) subst]
    in
    let c =
      match handle_distinct_vars pos_args pos_sc neg_args neg_sc with
      | Some subst ->
        let pos_lits = Literals.apply_subst renaming subst (C.lits pos_cl, pos_sc) in
        let neg_lits = Literals.apply_subst renaming subst (C.lits neg_cl, neg_sc) in
        let lits = (CCArray.except_idx pos_lits pos_idx) @ (CCArray.except_idx neg_lits  neg_idx) in
        C.create ~penalty:(max (C.penalty pos_cl) (C.penalty neg_cl))
                  ~trail:(C.trail_l [pos_cl; neg_cl]) lits (proof subst renaming) 
      | None ->
        let subst = Subst.empty in
        let pos_cl' = C.apply_subst ~renaming (pos_cl, pos_sc) subst in
        let neg_cl' = C.apply_subst ~renaming (neg_cl, neg_sc) subst in    
        let apply t = Subst.FO.apply renaming subst t in  
        let lits =
          (List.map (fun (p,n) -> L.mk_neq (apply (p, pos_sc)) (apply (n, neg_sc))) 
          (List.combine pos_args neg_args)) @ 
          (CCArray.except_idx (C.lits pos_cl') pos_idx) @
          (CCArray.except_idx (C.lits neg_cl') neg_idx)
        in
        C.create ~penalty:(max (C.penalty pos_cl') (C.penalty neg_cl'))
                  ~trail:(C.trail_l [pos_cl'; neg_cl']) lits (proof subst renaming)
    in
    CCOpt.return_if (not (is_tauto c)) c

  let check_if_gate task =
    let sym = task.sym in
    let gates_l = CS.to_list task.possible_gates in
    let filter_gates ?(sign=None) ~lit_num_filter gates_l =
      List.filter (fun cl ->
        not (is_tauto cl) &&
        lit_num_filter (Array.length (C.lits cl)) &&
        (match (CCArray.find_idx (fun lit -> 
          match get_sym_sign lit with 
          | Some(sym', sign') -> ID.equal sym sym' && 
                                 ((CCOpt.is_none sign) || CCOpt.get_exn sign == sign')
          | None -> false) (C.lits cl)) with
        | Some (i, lit) ->
          let free_vars = T.VarSet.of_list (L.vars lit) in
          CCOpt.is_none (CCArray.find_map_i (fun j lit'  ->
            if (i=j || T.VarSet.subset (T.VarSet.of_list (L.vars lit')) free_vars) then None
            else Some j
          ) (C.lits cl))
        | _ -> false))
      (CCList.fast_sort (fun cl cl' -> compare (C.length cl) (C.length cl')) gates_l)
    in

    let find_and_or bin_clauses long_clauses =
      (* both AND and OR definitions are of the form (~)name \/ literals
         and a bunch of binary clauses (~)name \/ lit   *)
      CCList.find_map (fun long_cl ->
        let sym_lits, other_lits = List.partition (fun lit ->
          match get_sym_sign lit with
          | Some(sym',_) -> ID.equal sym sym'
          | _ -> false
        ) (CCArray.to_list @@ C.lits long_cl) in
        assert (List.length sym_lits = 1);
        let sym_name_lit = List.hd sym_lits in
        let bin_gates = CCArray.of_list bin_clauses in
        if List.length other_lits > CCArray.length bin_gates then None
        else (
          (* bit vector remembering which binary clauses we have already used *)
          let matched = CCBV.create ~size:(CCArray.length bin_gates) false in
          let is_gate = List.for_all (fun lit ->
            let found = ref false in
            let i = ref 0 in
            while not !found && !i < CCArray.length bin_gates do
              let cl = bin_gates.(!i) in
              if not (CCBV.get matched !i) then (
                let idx_name_opt = CCArray.find_idx (fun lit ->
                  match get_sym_sign lit with
                  | Some(sym',_) -> ID.equal sym sym'
                  | None -> false
                ) (C.lits cl) in
                let idx_name, _ = CCOpt.get_exn idx_name_opt in
                let name_lit = L.negate (C.lits cl).(idx_name) in
                let other_lit = L.negate (C.lits cl).(1 - idx_name) in
                let is_matched = 
                  not @@ Iter.is_empty
                    (L.variant (lit,0) (other_lit,1)
                    |> Iter.filter (fun (subst,_) -> 
                        not @@ Iter.is_empty @@ 
                          L.variant ~subst (sym_name_lit, 0) (name_lit,1)))
                in
                if is_matched then (
                  CCBV.set matched !i;
                  found := true
                ));
                incr i;
            done;
            !found
          ) other_lits in
          let bin_cls = CCBV.select matched bin_gates in
          if is_gate then Some(long_cl, bin_cls)
          else None))
      long_clauses
    in

    let diff_vars_cnt cl =
      List.length @@ Literals.vars (C.lits cl)
    in

    let check_and () =
      let pos_gates = filter_gates ~sign:(Some true) ~lit_num_filter:(fun n -> n > 2) gates_l in
      let neg_gates = filter_gates ~sign:(Some false) ~lit_num_filter:((=) 2) gates_l in
      match find_and_or neg_gates pos_gates with
      | Some (pos_cl, neg_cls) when (diff_vars_cnt pos_cl <= 3) ->
        let to_remove = CS.of_list (pos_cl :: neg_cls) in
        task.neg_cls <- CS.diff task.neg_cls to_remove;
        task.pos_cls <- CS.diff task.pos_cls to_remove;
        task.is_gate <- Some([pos_cl], neg_cls);
        true
      | _ -> false
    in
    let check_or () =
      let pos_gates = filter_gates ~sign:(Some true) ~lit_num_filter:((=) 2) gates_l in
      let neg_gates = filter_gates ~sign:(Some false) ~lit_num_filter:(fun n -> n > 2) gates_l in
      (* checking for or will also check for equivalences p(x) <-> q(x) *)
      match find_and_or pos_gates neg_gates with
      | Some(neg_cl, pos_cls) when (diff_vars_cnt neg_cl <= 3) ->
        let to_remove = CS.of_list (neg_cl :: pos_cls) in
        task.neg_cls <- CS.diff task.neg_cls to_remove;
        task.pos_cls <- CS.diff task.pos_cls to_remove;
        task.is_gate <- Some(pos_cls, [neg_cl]);
        true
      | _ -> false
    in

    (* not yet implemented *)
    let check_ite () = false in
    let check_sat () =
      SAT.clear ();
      let orig_sc,new_sc = 0,1 in
      let rename_clause ~name_lit c =
        let lits = C.lits c in
        CCArray.find_map_i (fun i lit ->
          match lit with 
          | L.Equation(lhs,_,_) when L.is_predicate_lit lit ->
            begin try
              let subst = Unif.FO.variant (lhs, new_sc) (name_lit, orig_sc) in
              let lits = Literals.apply_subst Subst.Renaming.none subst 
                ((CCArray.of_list (CCArray.except_idx lits i)), new_sc) in
              Some (i,lits,c)
            with Unif.Fail -> None end
          | _ -> None) lits
      in

      let split_clauses used_cls =
        let pos_cls, neg_cls = CCList.partition (fun cl ->
          CCArray.exists (fun lit -> 
            match get_sym_sign lit with
            | Some(id,sign) -> ID.equal id task.sym && sign
            | _ -> false) (C.lits cl)
        ) used_cls
        in
        if List.exists (fun pos_cl -> 
          List.exists (fun neg_cl -> 
            CCOpt.is_some @@ 
              neq_resolver ~sym:task.sym ~pos_cl ~neg_cl) neg_cls
        ) pos_cls then None
        else Some (pos_cls, neg_cls)
      in


      let find_definition_set cls =
        List.iter (fun (i,lits,c) ->
          if not (is_tauto c) then (
            CCList.filter_map BBox.inject_lit (CCArray.to_list lits)
            |> SAT.add_clause ~proof:(C.proof_step c))
        ) cls;
        (match SAT.check ~full:true () with
        | Sat_solver.Unsat _ -> 
          let proof = Proof.S.step (SAT.get_proof ()) in
          let parents = List.map (fun p -> Proof.S.step @@ Proof.Parent.proof p) (Proof.Step.parents proof) in
          Util.debugf ~section 5 "SAT prover found unsat set: %d@." (fun k -> k  (CCList.length parents));
          let used_cls = CCList.filter_map (fun (_,_,cl) -> 
            CCOpt.return_if (CCList.mem ~eq:Proof.Step.equal (C.proof_step cl) parents) cl) cls  in
          Util.debugf ~section 5 "used clauses: @[%a@]@." (fun k -> k (CCList.pp C.pp) used_cls);
          split_clauses used_cls
        | _ -> None)
        
      in
      let is_def = function
        | (Literal.Equation(lhs,_,_) as lit) when Literal.is_predicate_lit lit ->
          CCOpt.is_some (Term.head lhs) && List.for_all T.is_var (T.args lhs)
        | _ -> false
      in
      match gates_l with 
      | x :: xs ->
        (* we will standardize all clauses by the first name literal in x *)
        let i,name_lit = CCOpt.get_exn (CCArray.find_map_i (fun i lit -> 
            match lit with 
            | L.Equation(lhs,rhs,_) when L.is_predicate_lit lit -> 
              if ID.equal task.sym (T.head_exn lhs) then Some(i,lhs) else None
            | _ -> None) (C.lits x)) 
        in
        let cls = 
          (i,CCArray.of_list @@ CCArray.except_idx (C.lits x) i,x)
            :: (CCList.filter_map (rename_clause ~name_lit) xs) in
        (match find_definition_set cls with
        | Some (core_pos,core_neg) ->
          let to_remove = CS.of_list (core_pos @ core_neg) in
          if CS.cardinal to_remove != 2 || 
             (* if there are two clauses then they must be of the form p(X,Y) <-> q(X,Y) *)
             CS.for_all (fun c -> C.length c == 2 &&
                                  CCArray.for_all is_def (C.lits c)) 
             to_remove then (
            Util.debugf ~section 3 "semantic pos def: @[%a@]@."
            (fun k -> k (CCList.pp C.pp) core_pos);
            Util.debugf ~section 3 "semantic neg def: @[%a@]@."
              (fun k -> k (CCList.pp C.pp) core_neg);
            task.neg_cls <- CS.diff task.neg_cls to_remove;
            task.pos_cls <- CS.diff task.pos_cls to_remove;
            task.is_gate <- Some(core_pos, core_neg);
            true
          ) else false
        | _ -> false)
      | _ -> false
    in
    if Env.flex_get k_check_gates &&
       ((not (Env.flex_get k_only_original_gates)) 
          || not @@ ID.payload_pred 
              ~f:(function 
                    ID.Attr_cnf_def -> true 
                    | _ -> false)
              task.sym) &&
        ((not (Env.flex_get k_only_non_conjecture_gates)) 
          || not @@ Signature.sym_in_conj task.sym (Env.signature ())) then (
      if Env.flex_get k_check_gates_semantically then ignore (check_sat ())
      else ignore (check_and () || check_or () || check_ite ()))

  let schedule_tasks () =
    ID.Map.iter (fun _ task -> 
      check_if_gate task;

      if should_schedule task then (
        Util.debugf ~section 5 "inserting: @[%a@]" (fun k -> k pp_task task);
        TaskSet.insert _task_queue task  
    )) !_pred_sym_idx

  let get_resolver () =
    if !_logic == NEqFO then neq_resolver else eq_resolver

  let calc_resolvents ~sym ~pos ~neg =
    CCList.flat_map (fun pos_cl ->
      CCList.filter_map (fun neg_cl ->
        get_resolver () ~sym ~pos_cl ~neg_cl
      ) neg
    ) pos

  let calc_non_singular_resolvents ~sym ~pos ~neg ~offending =
    let find_lit_by_sym_opt sym sign cl =
      try
        CCOpt.return (find_lit_by_sym sym sign cl)
      with _ -> None
    in

    let rec aux has_pred no_pred =
      (match has_pred with
      | [] -> no_pred
      | cl :: cls ->
        (* for the first occurrence of the symbol p in cl,
           we compute all possible replacements w.r.t. gate clauses.
           If there are more symbols left, we continue with all the replacements  *)
        let new_cls = 
          (match find_lit_by_sym_opt sym true cl with
          | Some _ ->
            CCList.filter_map (fun neg_cl -> 
              get_resolver () ~sym ~pos_cl:cl ~neg_cl) neg
          | None ->
            (match find_lit_by_sym_opt sym false cl with
            | Some _ ->
              CCList.filter_map (fun pos_cl -> 
                get_resolver () ~sym ~pos_cl ~neg_cl:cl) pos
            | None -> invalid_arg ""))
        in
        let has_lit cl =
          let (<+>) = CCOpt.(<+>) in
          CCOpt.is_some (find_lit_by_sym_opt sym true cl <+> 
                         find_lit_by_sym_opt sym false cl)
        in
        let has_pred', no_pred' = List.partition has_lit new_cls in
        aux (has_pred' @ cls) (no_pred' @ no_pred))
    in

    aux (CS.to_list offending) []

  let measure_decreases () =
    Env.flex_get k_measure_fun

  let do_pred_elim () =
    let removed_cls = ref None in
    let updated_removed inc =
      match !removed_cls with
      | None -> removed_cls := Some inc
      | Some inc' -> removed_cls := Some (inc' + inc)
    in

    let process_task task =
      assert(CS.is_empty task.offending_cls || Env.flex_get k_non_singular_pe);
      let pos_cls, neg_cls = 
        CCPair.map_same CS.to_list (task.pos_cls, task.neg_cls)
      in
      let sym = task.sym in
      let resolvents =
        match task.is_gate with
        | Some (pos_gates, neg_gates) ->
          if Env.flex_get k_prefer_spe && CS.is_empty task.offending_cls then (
            let results = 
              (calc_resolvents ~sym ~pos:pos_gates ~neg:neg_cls)
              @ (calc_resolvents ~sym ~pos:pos_cls ~neg:neg_gates)
              @ (calc_resolvents ~sym ~pos:pos_cls ~neg:neg_cls)
            in
            
            if measure_decreases () (Env.flex_get k_relax_val) task results
            then (results)
            else (
              (calc_resolvents ~sym ~pos:pos_gates ~neg:neg_cls)
                @ (calc_resolvents ~sym ~pos:pos_cls ~neg:neg_gates)
          )) 
          else 
            ((calc_non_singular_resolvents ~sym ~pos:pos_gates ~neg:neg_gates 
                                          ~offending:task.offending_cls)
            @ (calc_resolvents ~sym ~pos:pos_gates ~neg:neg_cls)
            @ (calc_resolvents ~sym ~pos:pos_cls ~neg:neg_gates))
        | None -> 
          assert(CS.is_empty task.offending_cls);
          calc_resolvents ~sym ~pos:pos_cls ~neg:neg_cls
      in
      if measure_decreases () (Env.flex_get k_relax_val) task resolvents then (
        Util.debugf ~section 1 "task info: @[%a@]" (fun k -> k pp_task task);
        updated_removed (calc_num_cls task - List.length resolvents);
        replace_clauses task resolvents;
      ) else (
        task.last_check <- Some (task.sq_var_weight, task.num_lits)
      )
    in

    let module S = TaskSet in
    while not (S.is_empty _task_queue) do
      let task = S.remove_min _task_queue in
      
      if not (ID.Set.mem task.sym !_ignored_symbols) then(
        Util.debugf ~section 5 "checking: @[%a@]" (fun k -> k pp_task task);
        process_task (task)
      );
    done;
    
    (* storing all newly computed clauses *)
    Env.add_passive (CS.to_iter !_newly_added);
    _newly_added := CS.empty;
    !removed_cls

  let steps = ref 0
  (* driver that does that every k-th step of given-clause loop *)
  let do_predicate_elimination () =
    steps := (!steps + 1) mod (Env.flex_get k_check_at);

    if !steps = 0 then (
     ignore( do_pred_elim ());
    )

  let initialize () =
    let init_clauses =
      CS.to_list (Env.ProofState.ActiveSet.clauses ())
      @ CS.to_list (Env.ProofState.PassiveSet.clauses ())
    in
    begin try
      Util.debugf ~section 5 "init_cl: @[%a@]@."
        (fun k -> k (CCList.pp C.pp) init_clauses);

      let init_clause_num = List.length init_clauses in

      CCFormat.printf "%% PE start: %d@." init_clause_num;
      
      List.iter (fun cl -> 
        scan_cl_lits cl;
        _tracked := CS.add cl !_tracked;
      ) init_clauses;

      CCFormat.printf "logic: %s@." (logic_to_str !_logic);

      if !_logic == Unsupported then (
        raise UnsupportedLogic
      );

      schedule_tasks ();

      Util.debugf ~section 5 "state:@[%a@]@."
        (fun k -> k (Iter.pp_seq pp_task) (ID.Map.values !_pred_sym_idx));

      Util.debugf ~section 1 "logic has%sequalities"
        (fun k -> k (if !_logic == EqFO then " " else " no "));

      Signal.on Env.ProofState.PassiveSet.on_add_clause react_clause_addded;
      Signal.on Env.ProofState.PassiveSet.on_remove_clause react_clause_removed;
      Signal.on Env.ProofState.ActiveSet.on_add_clause react_clause_addded;
      Signal.on Env.ProofState.ActiveSet.on_remove_clause react_clause_removed;
      Signal.on_every Env.on_forward_simplified (fun (c, new_state) ->
        if !_logic != Unsupported then (
          match new_state with
          | Some c' ->
            ignore(react_clause_removed c);
            ignore(react_clause_addded c')
          | _ -> ignore(react_clause_removed c) (* c is redundant *)
      ));


      ignore(do_pred_elim ());

      Util.debugf ~section 5 "after elim: @[%a@]@."
        (fun k -> k (CS.pp C.pp) (Env.ProofState.PassiveSet.clauses ()));
      Util.debugf ~section 5 "state:@[%a@]@."
        (fun k -> k (Iter.pp_seq pp_task) (ID.Map.values !_pred_sym_idx));


      let clause_diff =
        init_clause_num -
        (Iter.length (Env.get_active ()) + Iter.length (Env.get_passive ())) in
      CCFormat.printf "%% PE eliminated: %d@." clause_diff;

      if Env.flex_get k_inprocessing then (
        (* Env.Ctx.lost_completeness (); *)
        Env.add_clause_elimination_rule ~priority:2 "pred_elim" 
          do_predicate_elimination
      ) else if not @@ Env.flex_get k_fp_mode then raise UnsupportedLogic
    with UnsupportedLogic ->
      Util.debugf ~section 1 "logic is unsupported" CCFun.id;
      (* releasing possibly used memory *)
      _logic := Unsupported;
      _pred_sym_idx := ID.Map.empty
    end;
    Signal.StopListening

  let register () =
    Signal.on Env.on_start initialize

  let fixpoint_active = ref false
  let begin_fixpoint () =
    fixpoint_active := true;
    let env = (module E : OrigEnv.S) in
    register_parameters env;
    (*  has to be called after register parameters as 
        measure functions are not visible outside the module *)
    Env.flex_add k_measure_fun (match !_measure_name with 
      | "kk" -> kk_measure
      | "relaxed" -> relaxed_measure
      | "conservative" -> conservative_measure
      | _ -> invalid_arg "measure function not found");

    let init_clauses =
      CS.to_list (Env.ProofState.ActiveSet.clauses ())
      @ CS.to_list (Env.ProofState.PassiveSet.clauses ())
    in
    begin try
     
      List.iter (fun cl -> 
        scan_cl_lits cl;
        _tracked := CS.add cl !_tracked;
      ) init_clauses;

      schedule_tasks ();

      Signal.on Env.ProofState.PassiveSet.on_add_clause (fun c -> 
        if !fixpoint_active then react_clause_addded c
        else Signal.StopListening
      );
      Signal.on Env.ProofState.PassiveSet.on_remove_clause (fun c ->
        if !fixpoint_active then react_clause_removed c
        else Signal.StopListening
      );

      let ans = (do_pred_elim ()) in
      CCFormat.printf "%% PE start fixpoint: @[%a@]@." (CCOpt.pp CCInt.pp) ans;
      Util.debugf ~section 2 "Clause number changed for %a" (fun k -> k (CCOpt.pp CCInt.pp) ans)
      
    with UnsupportedLogic ->
      Util.debugf ~section 1 "logic is unsupported" CCFun.id;
      (* releasing possibly used memory *)
      _logic := Unsupported;
      _pred_sym_idx := ID.Map.empty
    end

  let fixpoint_step () =
    CCFormat.printf "relax val: %d@." (Env.flex_get k_relax_val);
    let ans = do_pred_elim () in
    Util.debugf ~section 1 "Clause number changed for %a" (fun k -> k (CCOpt.pp CCInt.pp) ans);
    if CCOpt.is_some ans then (
      CCFormat.printf "%% PE fixpoint: %d@." (CCOpt.get_exn ans)
    );
    CCOpt.is_some ans
  
  let end_fixpoint () =
    _logic := Unsupported;
    _pred_sym_idx := ID.Map.empty;
    fixpoint_active := false

  
  let setup ?(in_fp_mode=false) () =
    Env.flex_add k_fp_mode in_fp_mode;
    Env.flex_add k_measure_fun (match !_measure_name with 
      | "kk" -> kk_measure
      | "relaxed" -> relaxed_measure
      | "conservative" -> conservative_measure
      | _ -> invalid_arg "measure function not found");

    if Env.flex_get k_enabled then (
      if not (Env.flex_get A.k_avatar_enabled) then (register ())
      else (
        CCFormat.printf "AVATAR is not yet compatible with PredElim@."
      )
    )
end

let extension =
  let action env =
    let module E = (val env : Env.S) in
    register_parameters env;
    let module PredElim = Make(E) in

    E.flex_add k_enabled !_enabled;
    E.flex_add k_check_at !_check_at;
    E.flex_add k_inprocessing !_inprocessing;
    E.flex_add k_max_resolvents !_max_resolvents;
    E.flex_add k_check_gates !_check_gates;
    E.flex_add k_non_singular_pe !_non_singular_pe;
    E.flex_add k_relax_val !_relax_val;
    
    PredElim.setup ()
  in
  { Extensions.default with Extensions.
                         name="pred_elim";
                         prio = 50;
                         env_actions=[action];
  }

let () =
  Options.add_opts [
    "--pred-elim", Arg.Bool ((:=) _enabled), " enable predicate elimination";
    "--pred-elim-relax-value", Arg.Int ((:=) _relax_val), " value of relax constant for our new measure";
    "--pred-elim-measure-fun", Arg.Symbol (["kk"; "relaxed"; "conservative"], ((:=) _measure_name)), 
      " use either standard Korovin-Khasidashvili measure or our relaxed measure for measuring the proof state size";
    "--pred-elim-check-gates", Arg.Bool ((:=) _check_gates), " enable recognition of gate clauses";
    "--pred-elim-only-original-gates", Arg.Bool ((:=) _original_gates_only), " recognize only gates that are not introduced by Zipperposition";
    "--pred-elim-check-gates-semantically", Arg.Bool ((:=) _check_semantically), " recognize gates semantically, as described in our SAT techniques paper";
    "--pred-elin-only-non-conjecture-gates", Arg.Bool ((:=) _only_non_conj_gates), " recognize only non-conjecture symbols as possible gates";
    "--pred-elim-prefer-spe", Arg.Bool ((:=) _prefer_spe), " try DPE only when SPE fails";
    "--pred-elim-relax-value", Arg.Int ((:=) _relax_val), " value of relax constant for our new measure";
    "--pred-elim-measure-fun", Arg.Symbol (["kk"; "relaxed"; "conservative"], ((:=) _measure_name)), " use either standard Korovin-Khasidashvili measure or our relaxed measure for measuring the proof state size";
    "--pred-elim-non-singular", Arg.Bool ((:=) _non_singular_pe), " enable PE when gate is recognized and there are multiple occurrences of a symbol";
    "--pred-elim-inprocessing", Arg.Bool ((:=) _inprocessing), " predicate elimination as inprocessing rule";
    "--pred-elim-check-at", Arg.Int ((:=) _check_at), " when to perform predicate elimination inprocessing";
    "--pred-elim-max-resolvents", Arg.Int ((:=) _max_resolvents), " after how many resolvents to stop tracking a symbol";
  ]

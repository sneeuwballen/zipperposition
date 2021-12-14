
(* This file is free software, part of Zipperposition. See file "license" for more details. *)

(** {1 Blocked Clause Elimination} *)

open Logtk
open Libzipperposition



let k_enabled = Flex_state.create_key ()
let k_check_at = Flex_state.create_key ()
let k_max_symbol_occ = Flex_state.create_key ()
let k_processing_kind = Flex_state.create_key ()
let k_fp_mode = Flex_state.create_key ()

let section = Util.Section.make ~parent:Const.section "bce"

let _enabled = ref false
let _processing_kind = ref `PreprocessingOnly
let _check_at = ref 10
let _max_symbol_occ = ref (-1) (* -1 stands for infinity *)

module Avatar = Libzipperposition_avatar

module type S = sig
  module Env : Env.S

  (** {6 Registration} *)
  val setup : ?in_fp_mode:bool -> unit -> unit
  val begin_fixpoint : unit -> unit
  val fixpoint_step : unit -> bool
  val end_fixpoint : unit -> unit
end

module Make(E : Env.S) : S with module Env = E = struct
  module Env = E
  module C = Env.C
  module L = Literal
  module T = Term
  module CC = Congruence.FO
  module DEQ = CCDeque
  module SymSignIdx = Map.Make (struct 
      type t = (ID.t * bool) 
      let compare = CCPair.compare ID.compare CCBool.compare
  end)


  let k_removed_active = Flex_state.create_key ()
  let k_removed_passive = Flex_state.create_key ()
  let k_bce_sat_tracked = Flex_state.create_key ()
  
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
  
  (* an object representing the information necessary for
     performing a task of checking whether a clause is blocked on
     a given literals *)
  type bce_check_task =
  {
    (* clause and index for which we are checking blockedness *)
    lit_idx : int;  
    clause  : C.t; 
    (* list of candidates to check *)
    cands   : C.t CCDeque.t;
    (* is the list actively stored in the heap of tasks, or in the waiting
       state, because check against some candidate failed *)
    mutable heap_idx  : int;
  }

  module TaskStore = Map.Make (struct 
    type t = int * C.t
    let compare (idx_a, cl_a) (idx_b, cl_b) = 
      CCOrd.(<?>) (CCInt.compare (C.id cl_a) (C.id cl_b)) (compare, idx_a, idx_b)
  end)

  module TaskWrapper = struct
    type t = bce_check_task
    let idx task = task.heap_idx
    let set_idx task idx =
      task.heap_idx <- idx
    let lt a b =
      (DEQ.length a.cands < DEQ.length b.cands)
      || (DEQ.length a.cands = DEQ.length b.cands
            && CCInt.compare (C.id a.clause) (C.id b.clause) < 0)
      || (DEQ.length a.cands = DEQ.length b.cands
            && CCInt.compare (C.id a.clause) (C.id b.clause) = 0
            && a.lit_idx < b.lit_idx)
  end

  module TaskPriorityQueue = CCMutHeap.Make(TaskWrapper)
  let init_heap_idx = -1

  (* (symbol, sign) -> clauses with the corresponding occurence *)
  let ss_idx = ref SymSignIdx.empty
  (* clause (or its id) -> all clauses that it locks *)
  let clause_lock = ref Util.Int_map.empty
  (* a store that implements perfect sharing of tasks  *)
  let task_store = ref TaskStore.empty
  (* priority queue of the tasks to be performed *)
  let task_queue = TaskPriorityQueue.create ()
  (* a set containing symbols for which BCE will not be tried *)
  let ignored_symbols = ref ID.Set.empty
  
  (* assuming the weakest logic *)
  let logic = ref NEqFO

  let refine_logic new_val =
    if log_compare new_val !logic > 0 then (
      logic := new_val;
      if (new_val == NonAppVarHo) then (
        Env.Ctx.lost_completeness ()
      );
    )

  let lit_to_term sign =
    if sign then CCFun.id else T.Form.not_

  (* ignoring other fields of tasks *)
  let task_eq a b = a.lit_idx = b.lit_idx && C.equal a.clause b.clause

  let symbol_occurrs_too_often sym_count =
    Env.flex_get k_max_symbol_occ > 0 &&
    (sym_count > Env.flex_get k_max_symbol_occ)

  let add_lit_to_idx lit_lhs sign cl =
    let sym = T.head_exn lit_lhs in

    let sym_occs sym sign =
      CCOpt.map_or ~default:0 C.ClauseSet.cardinal
        (SymSignIdx.find_opt (sym,sign) !ss_idx)
    in

    let total_sym_occs = sym_occs sym true + sym_occs sym false + 1 in

    if symbol_occurrs_too_often total_sym_occs then (
      ss_idx := SymSignIdx.remove (sym, false) (SymSignIdx.remove (sym, true) !ss_idx);
      ignored_symbols := ID.Set.add sym !ignored_symbols;
      Util.debugf ~section 5 "ignoring symbol @[%a@]@." (fun k -> k ID.pp sym);
    ) else (
      ss_idx := SymSignIdx.update (sym, sign) (fun old ->
        Some (C.ClauseSet.add cl (CCOpt.get_or ~default:C.ClauseSet.empty old))
      ) !ss_idx;
    )

  (* find all clauses for which L-resolution should be tried against literal
     with given lhs and sign  *)
  let find_candindates lhs sign = 
    let hd = T.head_exn lhs in
      C.ClauseSet.to_list
        (CCOpt.get_or
            ~default:C.ClauseSet.empty
          (SymSignIdx.find_opt (hd, not sign) !ss_idx))
  
  (* Scan the clause and if it is in supported logic fragment,
     store its literals in the symbol index *)
  let scan_cl_lits cl =
    CCArray.iter (function 
      | L.Equation(lhs,rhs,_) as lit ->
        let sign = L.is_positivoid lit in
        let is_poly = 
          not (Type.VarSet.is_empty (T.ty_vars lhs))
          || not (Type.VarSet.is_empty (T.ty_vars rhs))
        in
        if not is_poly && not (Type.is_fun (T.ty lhs)) then (
          if Type.is_prop (T.ty lhs) then (
            if L.is_predicate_lit lit && CCOpt.is_some (T.head lhs) then (
              if not (T.is_fo_term lhs) then (
                refine_logic NonAppVarHo
              );
              let hd_sym = T.head_exn lhs in
              if not (ID.Set.mem hd_sym !ignored_symbols) 
              then add_lit_to_idx lhs sign cl
            ) else (
              (* reasoning with formulas is currently unsupported *)
              Util.debugf ~section 1 "unsupported because of formula @[%a@]@." (fun k -> k L.pp lit);
              logic := Unsupported;
              raise UnsupportedLogic;
            )
          ) else (
            if T.is_fo_term lhs && T.is_fo_term rhs then refine_logic EqFO
            else refine_logic NonAppVarHo)
        ) else (
            logic := Unsupported; 
            Util.debugf ~section 1 "unsupported because of functional literal @[%a@]@." (fun k -> k L.pp lit);
            raise UnsupportedLogic)
      | _ -> ()
    ) (C.lits cl)

  (* Add candidates to already registered task *)
  let add_candidates lit_idx cl cand_cls =
    let t = TaskStore.find (lit_idx, cl) !task_store in
    DEQ.add_iter_back t.cands (CCList.to_iter cand_cls);
    if TaskPriorityQueue.in_heap t then (
      TaskPriorityQueue.increase task_queue t;
    )

  (* Register a new task, calculate its candidates and make it active. Only
     clauses within the supported logic fragment can be registered.

     If update_others is false, index state of other clauses will not be
     updated. We want to turn this option to false in the initialization phase,
     as index state will be update when the time for registering new clause
     comes and updating of states is an expensive operation since it traverses
     the heap in O(n) *)
  let register_task ?(update_others=true) lit_idx clause =
    (* insert new clause into the candidate list of previously inserted clauses *)
    let update_cand_lists hd sign clause cands =
      List.iter (fun cand ->
        if not (C.equal cand clause) then (
          CCArray.iteri (fun lit_idx lit ->
            match lit with
            | L.Equation(lhs,_,_)
              when L.is_predicate_lit lit  &&
                  ID.equal (T.head_exn lhs) hd &&
                  sign != L.is_positivoid lit ->
              add_candidates lit_idx cand [clause]
            | _ -> ()
          ) (C.lits cand))
      ) cands;
    in

    match (C.lits clause).(lit_idx) with
    | L.Equation (lhs, rhs, _) as lit 
      when L.is_predicate_lit lit
            && not (ID.Set.mem (T.head_exn lhs) !ignored_symbols) ->
      (* assert (T.is_fo_term lhs); *)
      let hd = T.head_exn lhs in
      let sign = L.is_positivoid lit in
      let cands = find_candindates lhs sign in
      if update_others then (
        update_cand_lists hd sign clause cands
      );
      let task = {lit_idx; clause; cands=DEQ.of_list cands;
                  heap_idx = init_heap_idx} in
      task_store := TaskStore.add (lit_idx, clause) task !task_store;
      TaskPriorityQueue.insert task_queue task
    | _ -> ( (* equation literals do not represent tasks *) )

  (* Update all the bookeeping information when a new clause is introduced *)
  let add_clause cl =
    try
      if !logic == Unsupported then raise UnsupportedLogic;

      scan_cl_lits cl;
      CCArray.iteri (fun lit_idx _ -> register_task lit_idx cl) (C.lits cl)
    with UnsupportedLogic ->
      refine_logic Unsupported;
      TaskPriorityQueue.clear task_queue

  (* remove the clause from symbol index *)
  let deregister_symbols cl =
    CCArray.iteri (fun _ lit -> 
      match lit with 
      | L.Equation(lhs,_,_) 
        when L.is_predicate_lit lit  ->
        ss_idx :=
          SymSignIdx.update (T.head_exn lhs, L.is_positivoid lit) (function
            | Some old ->
              let new_ = C.ClauseSet.remove cl old in
              CCOpt.return_if (not (C.ClauseSet.is_empty new_)) new_
            | None -> None (*already removed*)) !ss_idx;
      | _ -> ()
    ) (C.lits cl)

  let lock_clause locker locked_task =
    assert(C.id locker == C.id (DEQ.peek_front locked_task.cands));
    clause_lock := Util.Int_map.update (C.id locker) (fun old_val -> 
      let locked_tasks = CCOpt.get_or ~default:[] old_val in
      Some (locked_task :: locked_tasks)
    ) !clause_lock

  
  (* If clause is removed from the active/passive set, then release
     the locks that it holds, and make all the locked clauses active *)
  let release_locks clause =
    Util.debugf ~section 3 "clearing locks: @[%a@]@." (fun k -> k C.pp clause);
    try 
      List.iter (fun task ->
        assert (not (TaskPriorityQueue.in_heap task));
        assert (not (DEQ.is_empty task.cands));
        let locking_cl = DEQ.take_front task.cands in
        assert (C.id locking_cl = C.id clause);
        Util.debugf ~section 3 " |@[%a@]|%d@." (fun k -> k C.pp task.clause task.lit_idx);
        TaskPriorityQueue.insert task_queue task
      ) (Util.Int_map.find (C.id clause) !clause_lock);
      clause_lock := Util.Int_map.remove (C.id clause) !clause_lock
    with Not_found ->
      (* clause was already removed *)
      ()


  (* remove the clause from the whole BCE tracking system *)
  let deregister_clause clause =
    deregister_symbols clause;
    release_locks clause

  let remove_from_proof_state clause =
    begin
      try
        if Env.is_active clause then (
          C.Tbl.add (Env.flex_get k_removed_active) clause ();
        ) else if Env.is_passive clause then (
          C.Tbl.add (Env.flex_get k_removed_passive) clause ();
        )
    with _ ->
      (* we are in the preprocessing phase, so we can mark the clause *) 
      C.mark_redundant clause
      end;
    if Env.flex_get k_processing_kind != `InprocessingSat then ( 
      C.mark_redundant clause
      (* if we are doing the inprocessing in SAT mode, we cannot
         mark the clauses as redundant, since they might have to be returned
         to the proof state. *)
    );
    Env.remove_active (Iter.singleton clause);
    Env.remove_passive (Iter.singleton clause);
    Env.remove_simpl (Iter.singleton clause)
  
  
  (* checks whether all L-resolvents between orig_cl on literal with index
     lit_idx and partner are valid   *)
  let resolvent_is_valid_neq lit_idx orig_cl partner =
    assert (lit_idx < C.length orig_cl);
    let sc_orig, sc_partner = 0, 1 in

    (* splits the partner clause into unifiable and nonunifiable literals
       with respect to the literal of the original clause chosen for checking *)
    let split_partner lhs sign partner = 
      CCArray.foldi (fun (unifiable, others) idx lit ->
        match lit with
        | L.Equation(lhs', _, _) 
          when L.is_predicate_lit lit 
               && L.is_positivoid lit != sign ->
          begin try
            let subst = Unif.FO.unify_syn (lhs, sc_orig) (lhs', sc_partner) in
            (lhs', subst) :: unifiable, others
          with Unif.Fail -> unifiable, lit :: others end
        | _ -> unifiable, lit :: others
      ) ([], []) (C.lits partner)
    in

    let check_resolvents l_idx orig_cl (unifiable, nonunifiable) =
      let orig_sign = L.is_positivoid ((C.lits orig_cl).(l_idx)) in
      (* literals against which unifiable part of the clause needs to be checked *)
      let for_tautology_checking =
        List.filter (fun (lit, _) -> 
          L.is_positivoid lit = orig_sign 
          && L.is_predicate_lit lit) 
        ( (List.map (fun x -> x,sc_orig) (CCArray.except_idx (C.lits orig_cl) l_idx)) @  
           List.map (fun x -> x, sc_partner) nonunifiable ) 
      in
      if CCList.is_empty unifiable then true
      else (
        (* lhs is the lhs of the literal we are currently checking
           subst is substitution built so far
           rest are other literals that should be checked *)
        let rec check_lit lhs subst rest =
          (* if clause is valid because there are opposite literals in nonunifiable
             part -- we have won as those literals will not be removed with L-resolution *)
          let is_valid =
            List.exists (fun lit' ->
              CCOpt.is_some (CCArray.find_map_i (fun idx lit ->
                if idx != l_idx && 
                  L.are_opposite_subst ~subst (lit, sc_orig) (lit', sc_partner) then(
                  Some lit)
                else None) 
              (C.lits orig_cl))
             ) nonunifiable in

          Util.debugf ~section 30 
            "check: @. lit: @[%a@]@. unif:@[%a@]@. non_unif @[%a@]@. partner_cl: @[%a@]@."
            (fun k -> k L.pp ((C.lits orig_cl).(l_idx)) (CCList.pp T.pp) 
                             (List.map fst unifiable) 
                             (CCList.pp L.pp) nonunifiable C.pp partner  );
          
          is_valid ||
          (not (CCList.is_empty rest) && (
            (* else, we do L-resolution with the unifiable part extended *)
            let contrasting, rest' =
              CCList.partition (fun (lhs,_) ->
                CCOpt.is_some (
                  List.find_opt (fun (lit, sc) ->
                    let lhs'  = CCOpt.get_exn (L.View.get_lhs lit) in
                    Unif.FO.equal ~subst (lhs',sc) (lhs, sc_partner)
                  )
                  (for_tautology_checking)))  
              rest 
            in

            (* clause is not valid *)
            if CCList.is_empty contrasting then false
            else ( 
              try
                let subst = 
                  List.fold_left (fun subst (lhs',_) -> 
                    (* extending the substitution *)
                    Unif.FO.unify_syn ~subst (lhs, sc_partner) (lhs', sc_partner)              
                  ) (subst) contrasting 
                in
                (* clause is valid, but using only literals in the unifiable part,
                   try extended L-resolution *)
                check_lit lhs subst rest'
              with Unif.Fail -> 
                (* substitution cannot be extended, on the ground level L-resolution
                   is not possible, thus clause is valid *)
                true)
          )) 
        in
        (* check if all L-resolvents are valid in polynomial time *)
        let rec check_l_resolvents rest = function
          | (lhs, subst) as x :: xs ->
            check_lit lhs subst (rest @ xs) &&
            check_l_resolvents (x :: rest) xs
          | [] -> true
        in
        check_l_resolvents [] unifiable
      )
    in

    let lit = (C.lits orig_cl).(lit_idx) in 
    let lhs, sign = 
      match lit with
      | L.Equation(lhs, _, _) when L.is_predicate_lit lit ->
        (lhs, L.is_positivoid lit)
      | _ -> assert false (* literal must be eligible for BCE *)
    in
    check_resolvents lit_idx orig_cl (split_partner lhs sign partner)

  (* checks whether all *flat* L-resolvents between orig_cl on literal with
     index lit_idx and partner are valid *using congruence closure algorithm*  *)
  let resolvent_is_valid_eq lit_idx orig_cl partner =
    assert (lit_idx < C.length orig_cl);
    let sc_orig, sc_partner = 0, 1 in
    let renaming = Subst.Renaming.create () in
    (* renaming clauses apart -- cannot be done automatically since no unifiers
       are being computed and the API will not take care of that for us *)
    let orig_cl = C.apply_subst ~renaming (orig_cl, sc_orig) Subst.empty in
    let partner = C.apply_subst ~renaming (partner, sc_partner) Subst.empty in
    
    (* splitting into parts that have the same head and the sign as the literal
       in the original clause and the other ones  *)
    let split_partner lhs sign partner =
      CCArray.foldi (fun (same_hds, others) idx lit ->
        match lit with
        | L.Equation(lhs', _, _) 
          when L.is_predicate_lit lit 
               && L.is_positivoid lit != sign 
               && ID.equal (T.head_exn lhs) (T.head_exn lhs') ->      
          lhs' :: same_hds, others
        | _ -> same_hds, lit :: others
      ) ([], []) (C.lits partner)
    in
    
    let lit = (C.lits orig_cl).(lit_idx) in 
    let orig_lhs, orig_sign = 
      match lit with
      | L.Equation(lhs, _, _) when L.is_predicate_lit lit ->
        (lhs, L.is_positivoid lit)
      | _ -> assert false (* literal must be eligible for BCE *)
    in
    
    let check_resolvents l_idx orig_cl (same_hd_lits, diff_hd_lits) =
      let orig_args = T.args @@ CCOpt.get_exn @@ (L.View.get_lhs (C.lits orig_cl).(l_idx)) in

      (* add equations that correspond to computing a flat resolvent between
         original lit and the one that has new_args arguments to the head *)
      let add_flat_resolvent ~cc new_args =
        Util.debugf ~section 10  " adding resolvent <%a>, <%a>@." 
          (fun k -> k (CCList.pp T.pp) orig_args (CCList.pp T.pp) new_args);

        List.fold_left (fun acc (lhs,rhs) -> CC.mk_eq acc lhs rhs) 
          cc (List.combine orig_args new_args)
      in

      (* calculating positive, that is negative literals in both clauses *)
      let orig_pos, orig_neg = 
        CCList.partition L.is_positivoid (CCArray.except_idx (C.lits orig_cl) l_idx) in
      let partner_pos, partner_neg = 
        CCList.partition L.is_positivoid (diff_hd_lits)
      in
      let all_pos = orig_pos @ partner_pos in
      let all_neg = orig_neg @ partner_neg in

      (* Literals from same_hd_lits part might need to be tested for congruence
         with literals of the same head, but opposite side from either clause *)
      let for_congruence_testing =
        CCList.filter_map (fun lit -> 
          if L.is_predicate_lit lit && L.is_positivoid lit = orig_sign then (
            CCOpt.flat_map (fun t -> 
              CCOpt.return_if 
                (ID.equal (T.head_exn t) (T.head_exn orig_lhs)) 
                t) 
            (L.View.get_lhs lit)
          ) else None) (if orig_sign then all_pos else all_neg)
      in
      (* equational theory induced by all the negative literals *)
      let orig_cc =
        List.fold_left (fun acc lit -> 
          assert (L.is_negativoid lit);
          let lhs,rhs,_ = CCOpt.get_exn @@ L.View.as_eqn lit in
          CC.mk_eq acc lhs rhs
        ) (CC.create ~size:16 ()) all_neg
      in

      if CCList.is_empty same_hd_lits then true (* no L-resolvent possible *)
      else (
        let rec check_lit ~cc rest =
          (* validity is achieved without using same_hd_lits literals *)
          let is_valid = List.exists (fun lit -> 
            assert(L.is_positivoid lit);
            
            match lit with
            | L.Equation (lhs, rhs, _) -> CC.is_eq cc lhs rhs
            | L.True -> true
            | _ -> false
          ) all_pos in
          
          is_valid ||
          (
            not (CCList.is_empty rest) && (
              let congruent, rest = List.partition (fun lhs -> 
                  List.exists (fun lhs' -> CC.is_eq cc lhs lhs') for_congruence_testing
                ) rest 
              in
              (* clause is not valid *)
              if CCList.is_empty congruent then false
              else (
                (* validity is achieved using literals from same_hd_lits, let's
                   see what happens when they are removed as part of flat
                   L-resolvent computation*)
                let cc = 
                  List.fold_left (fun acc lhs -> add_flat_resolvent ~cc (T.args lhs)) 
                    cc congruent
                in
                check_lit ~cc rest
          )))
        in
        (* check if all l-resolvents are valid in polynomial time *)
        let rec check_l_resolvents others = function
          | x :: xs ->
            let cc_with_lhs = add_flat_resolvent ~cc:orig_cc (T.args x) in
            check_lit ~cc:cc_with_lhs (others @ xs) &&
            check_l_resolvents (x :: others) xs
          | [] -> true 
        in
        check_l_resolvents [] same_hd_lits
      )
    in
    check_resolvents lit_idx orig_cl (split_partner orig_lhs orig_sign partner)

  let get_validity_checker () =
    assert (!logic != Unsupported);
    if !logic != NEqFO then resolvent_is_valid_eq
    else resolvent_is_valid_neq

  let is_blocked cl =
    let validity_checker = get_validity_checker () in
    let blocked_lit_idx =
      CCArray.find_map_i (fun idx lit -> 
        match lit with
        | L.Equation(lhs,_,_) when L.is_predicate_lit lit ->
          let sym = T.head_exn lhs in
          (match SymSignIdx.find_opt (sym, not (L.is_positivoid lit)) !ss_idx with 
          | Some partners ->
            if (C.ClauseSet.for_all (fun partner -> 
              C.equal cl partner || validity_checker idx cl partner
            ) partners) 
            then (Some idx)
            else None
          | None -> 
            if not (ID.Set.mem sym !ignored_symbols) 
            then Some idx 
            else None)
        | _ -> None
      ) (C.lits cl)
    in
    let ans = CCOpt.is_some blocked_lit_idx in
    if ans then (
      Util.debugf ~section 3 "@[%a@] is blocked on @[%a@] @." 
        (fun k -> k C.pp cl L.pp (C.lits cl).(CCOpt.get_exn blocked_lit_idx));
      Util.debugf ~section 3 "proof:@[%a@]@." (fun k -> k Proof.S.pp_tstp (C.proof cl));
    );
    ans

  (* function that actually performs the blocked clause elimination *)
  let do_eliminate_blocked_clauses () =
    let removed_cnt = ref 0 in

    (* checks if each candidate stored in the task has valid L-resolvent.
       if that is not the case, it returns the rest of candidates to be checked. *)
    let process_task task =
      let cl = task.clause in
      let lit_idx = task.lit_idx in
      let hd_sym = 
        T.head_exn @@ CCOpt.get_exn @@ L.View.get_lhs (C.lits task.clause).(lit_idx)
      in
      let validity_checker = get_validity_checker () in
      
      let rec task_is_blocked deq =
        DEQ.is_empty deq || (
          let partner = DEQ.take_front deq in
          if C.equal cl partner || C.is_redundant partner || validity_checker lit_idx cl partner
          then (
            Util.debugf ~section 5 "valid-res(@[%a@], @[%a@](%b))@."
              (fun k -> k C.pp cl C.pp partner (C.is_redundant partner));
            task_is_blocked deq
          ) else (
            Util.debugf ~section 5 "blocks(@[%a@], @[%a@])@."
              (fun k -> k C.pp partner C.pp cl);
            DEQ.push_front deq partner;
            lock_clause partner task;
            false))
      in
      
      if not (C.is_empty task.clause || 
              C.is_redundant task.clause ||
              ID.Set.mem hd_sym !ignored_symbols) then (
        Util.debugf ~section 3 "checking blockedness" CCFun.id;
        (* let original_partners = CCDeque.to_list task.cands in *)
        match task_is_blocked task.cands with
        | true ->
          Util.debugf ~section 2 "removed(%d): @[%a@]" (fun k -> k task.lit_idx C.pp cl);
          deregister_clause cl;
          remove_from_proof_state cl;
          incr removed_cnt;
        | false -> 
          assert (not (TaskPriorityQueue.in_heap task))
      ) else (
        Util.debugf ~section 3 "ignoring %b %b %b" 
          (fun k -> k (C.is_empty task.clause) 
                      (C.is_redundant task.clause) 
                      (ID.Set.mem hd_sym !ignored_symbols) ););
    in


    let module Q = TaskPriorityQueue in
    while not (Q.is_empty task_queue) do
      process_task (Q.remove_min task_queue)
    done;

    Util.debugf ~section 2 "bce removed %d clauses@." (fun k -> k !removed_cnt);
    !removed_cnt

  let steps = ref 0
  (* driver that does that every k-th step of given-clause loop *)
  let eliminate_blocked_clauses () =
    steps := (!steps + 1) mod (Env.flex_get k_check_at);

    if !steps = 0 then (
      let original_cls = 
        Iter.to_list (Iter.append (Env.get_active ()) (Env.get_passive ())) 
      in
      let eliminated = do_eliminate_blocked_clauses () in
      if eliminated != 0 then (
        Util.debugf ~section 2  "original clause set:@.@[%a@]" 
          (fun k -> k (CCList.pp C.pp) original_cls);
      );
    )

  let react_clause_addded cl =
    add_clause cl

  let react_clause_removed cl =
    deregister_clause cl

  let do_bce_sat () =
    C.Tbl.clear (Env.flex_get k_removed_active);
    C.Tbl.clear (Env.flex_get k_removed_passive);
    
    Util.debugf ~section 1 "new BCE-SAT attempt" CCFun.id;

    ignore @@ do_eliminate_blocked_clauses ();


    if C.Tbl.length (Env.flex_get k_bce_sat_tracked) == 0 then (
      CCFormat.printf "%% BCE inprocessing removed all clauses"
    ) else (
      (* reinserting removed clauses *)
      let removed_actives = C.Tbl.keys (Env.flex_get k_removed_active) in
      let removed_passives = C.Tbl.keys (Env.flex_get k_removed_passive) in
      Util.debugf ~section 1 "reinserting %d/%d clauses" (fun k -> 
        k (Iter.length removed_actives + Iter.length removed_passives)
          (C.Tbl.length (Env.flex_get k_bce_sat_tracked)));
      Env.add_active removed_actives;
      Env.add_simpl removed_actives;
      Env.add_passive removed_passives;
    )

  let eliminate_bce_sat () =
    steps := (!steps + 1) mod (Env.flex_get k_check_at);

    if !steps = 0 then do_bce_sat ()


  let initialize_regular () =
    let init_clauses =
      C.ClauseSet.to_list (Env.ProofState.ActiveSet.clauses ())
      @ C.ClauseSet.to_list (Env.ProofState.PassiveSet.clauses ())
    in
    begin try
      Util.debugf ~section 3 "init_cl: @[%a@]@."
        (fun k -> k (CCList.pp C.pp) init_clauses);

      let init_clause_num = List.length init_clauses in

      CCFormat.printf "%% BCE start: %d@." init_clause_num;
      
      (* build the symbol index *)
      List.iter scan_cl_lits init_clauses;

      Util.debugf ~section 1 "logic has%sequalities"
        (fun k -> k (if !logic == EqFO then " " else " no "));

      (* create tasks for each clause *)
      List.iter 
        (fun cl ->
          CCArray.iteri (fun lit_idx _ -> 
            register_task ~update_others:false lit_idx cl) 
          (C.lits cl))
      init_clauses;
      (* eliminate clauses *)
      ignore (do_eliminate_blocked_clauses ());

      let clause_diff =
        init_clause_num -
        (Iter.length (Env.get_active ()) + Iter.length (Env.get_passive ())) in
      CCFormat.printf "%% BCE eliminated: %d@." clause_diff;

      if Env.flex_get k_processing_kind != `PreprocessingOnly || Env.flex_get k_fp_mode then (
        if Env.flex_get k_processing_kind == `InprocessingFull then (
          Env.Ctx.lost_completeness ()
        );

        if Env.flex_get k_processing_kind == `InprocessingSat then (
          Env.flex_add k_removed_active (C.Tbl.create 256);
          Env.flex_add k_removed_passive (C.Tbl.create 256);
          Env.flex_add k_bce_sat_tracked (C.Tbl.create 256);
          
          let add_cl_sat cl =
            C.Tbl.add (Env.flex_get k_bce_sat_tracked) cl ();
            react_clause_addded cl
          in
          let remove_cl_sat cl =
            C.Tbl.remove (Env.flex_get k_bce_sat_tracked) cl;
            react_clause_removed cl
          in

          Env.ProofState.PassiveSet.clauses ()
          |> C.ClauseSet.to_iter
          |> Iter.iter (fun cl -> C.Tbl.add (Env.flex_get k_bce_sat_tracked) cl ());


          Signal.on_every Env.ProofState.PassiveSet.on_add_clause (fun cl -> 
            if C.proof_depth cl = 0 then add_cl_sat cl
          );
          Signal.on_every Env.ProofState.PassiveSet.on_remove_clause remove_cl_sat;
          Signal.on_every Env.on_forward_simplified (fun (_,state) ->
            CCOpt.iter (add_cl_sat) state);
          Signal.on_every Env.ProofState.ActiveSet.on_remove_clause remove_cl_sat;
        ) else (
          (* clauses begin their life when they are added to the passive set *)
          Signal.on_every Env.ProofState.PassiveSet.on_add_clause react_clause_addded;
          (* clauses can be calculus-removed from the active set only in DISCOUNT loop *)
          Signal.on_every Env.ProofState.ActiveSet.on_remove_clause react_clause_removed;
          (* Clauses are removed from the passive set when they are moved to active.
            In this case clause can me modified or deemed redundant by forward
            modification procedures. we react accordingly.*)
          Signal.on_every Env.on_forward_simplified (fun (c, new_state) -> 
            match new_state with
            | Some c' ->
              if not (C.equal c c') then (
                react_clause_removed c; 
                react_clause_addded c'
              )
            | _ -> react_clause_removed c; (* c is redundant *));
        );
        if not @@ Env.flex_get k_fp_mode then (
          if Env.flex_get k_processing_kind = `InprocessingFull then( 
            Env.add_clause_elimination_rule ~priority:1 "BCE" eliminate_blocked_clauses
          )else (Env.add_clause_elimination_rule ~priority:1 "BCE_SAT" eliminate_bce_sat)
        )
      ) else ( raise UnsupportedLogic ) (* clear all data structures *)
    with UnsupportedLogic ->
      Util.debugf ~section 1 "logic is unsupported" CCFun.id;
      (* releasing possibly used memory *)
      ss_idx := SymSignIdx.empty;
      clause_lock := Util.Int_map.empty;
      task_store := TaskStore.empty;
      TaskPriorityQueue.clear task_queue;
    end;
    Signal.StopListening

  let fixpoint_active = ref false
  
  let begin_fixpoint () =
    E.flex_add k_max_symbol_occ !_max_symbol_occ;

    let init_clauses =
      C.ClauseSet.to_list (Env.ProofState.ActiveSet.clauses ())
      @ C.ClauseSet.to_list (Env.ProofState.PassiveSet.clauses ())
    in
    begin try
      fixpoint_active := true;

      List.iter scan_cl_lits init_clauses;
      List.iter 
        (fun cl ->
          CCArray.iteri (fun lit_idx _ -> 
            register_task ~update_others:false lit_idx cl) 
          (C.lits cl))
      init_clauses;
      (* eliminate clauses *)
      let num_eliminated =  do_eliminate_blocked_clauses () in
      Util.debugf ~section 2"Step eliminates %d clauses" (fun k -> k num_eliminated);

      CCFormat.printf "%% BCE start fixpoint: @[%d@]@." num_eliminated;

      Signal.on Env.ProofState.PassiveSet.on_add_clause (fun c ->
        if !fixpoint_active then (react_clause_addded c; Signal.ContinueListening)
        else Signal.StopListening
      );
      Signal.on Env.ProofState.PassiveSet.on_remove_clause (fun c ->
        if !fixpoint_active then (react_clause_removed c; Signal.ContinueListening)
        else Signal.StopListening
      );

    with UnsupportedLogic ->
      Util.debugf ~section 2 "logic is unsupported" CCFun.id;
      (* releasing possibly used memory *)
      ss_idx := SymSignIdx.empty;
      clause_lock := Util.Int_map.empty;
      task_store := TaskStore.empty;
      TaskPriorityQueue.clear task_queue;
      fixpoint_active := false
    end

  let fixpoint_step () =
    let num_eliminated = do_eliminate_blocked_clauses () in
    Util.debugf ~section 1 "Step eliminates %d clauses" (fun k -> k num_eliminated);
    if num_eliminated != 0 then (
      CCFormat.printf "%% BCE fixpoint: %d@." num_eliminated
    );
    num_eliminated != 0

  let end_fixpoint () =
    ss_idx := SymSignIdx.empty;
    clause_lock := Util.Int_map.empty;
    task_store := TaskStore.empty;
    TaskPriorityQueue.clear task_queue;
    fixpoint_active := false

  let register () =
    Signal.on Env.on_start initialize_regular

  let setup ?(in_fp_mode=false) () =
    if Env.flex_get k_enabled then (
      Env.flex_add k_fp_mode in_fp_mode;
      if not (Env.flex_get Avatar.k_avatar_enabled) then (register ())
      else (
        CCFormat.printf "AVATAR is not yet compatible with BCE@."
      )
    )
end

let extension =
  let action env =
    let module E = (val env : Env.S) in
    let module BCE = Make(E) in
    E.flex_add k_enabled !_enabled;
    E.flex_add k_max_symbol_occ !_max_symbol_occ;
    E.flex_add k_check_at !_check_at;
    E.flex_add k_processing_kind !_processing_kind;    
    BCE.setup ()
  in
  { Extensions.default with Extensions.
                         name="bce";
                         prio = 40;
                         env_actions=[action];
  }

let () =
  Options.add_opts [
    "--bce", Arg.Bool ((:=) _enabled), " scan clauses for AC definitions";
    "--bce-processing-kind", Arg.Symbol (["preprocessing";"inprocessing-full";"inprocessing-sat"], (function 
      | "preprocessing" -> _processing_kind := `PreprocessingOnly
      | "inprocessing-full" -> _processing_kind := `InprocessingFull
      | "inprocessing-sat" -> _processing_kind := `InprocessingSat
      | _ -> assert false)), 
    " scan clauses for AC definitions";
    "--bce-check-every", Arg.Int ((:=) _check_at), " check BCE every n steps of saturation algorithm";
    "--bce-max-symbol-occurences", Arg.Int ((:=) _max_symbol_occ), " limit a given symbol to n occurences only";
  ]

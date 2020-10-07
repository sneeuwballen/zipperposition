
(* This file is free software, part of Zipperposition. See file "license" for more details. *)

(** {1 Blocked Clause Elimination} *)

open Logtk
open Libzipperposition

let k_enabled = Flex_state.create_key ()
let k_check_at = Flex_state.create_key ()

module type S = sig
  module Env : Env.S

  (** {6 Registration} *)
  val setup : unit -> unit
end

module Make(E : Env.S) : S with module Env = E = struct
  module Env = E
  module C = Env.C
  module L = Literal
  module T = Term
  module CC = Congruence.FO
  module SymSignIdx = Map.Make (struct 
      type t = (ID.t * bool) 
      let compare = CCPair.compare ID.compare Bool.compare
  end)
  (* module MH = CCMutHeap.Make() *)
  
  type logic = 
    | NEqFO  (* nonequational FO *)
    | EqFO (* equational FO *)
    | Unsupported   (* HO or FO with theories *)

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
    mutable cands   : C.t list;
    (* is the list actively stored in the heap of tasks, or in the waiting
       state, because check against some candidate failed *)
    mutable heap_idx  : int;
  }

  module TaskStore = Map.Make (struct 
    type t = int * C.t
    let compare (idx_a, cl_a) (idx_b, cl_b) = 
      CCOrd.(<?>) (C.compare cl_a cl_b) (compare, idx_a, idx_b)
  end)

  module TaskWrapper = struct
    type t = bce_check_task
    let idx task = task.heap_idx
    let set_idx task idx = 
      task.heap_idx <- idx
    let lt a b =
      (List.length a.cands < List.length b.cands)
      || (List.length a.cands = List.length b.cands 
            && C.compare a.clause b.clause < 0)
      || (List.length a.cands = List.length b.cands 
            && C.compare a.clause b.clause = 0 
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
  
  (* assuming the weakest logic *)
  let logic = ref NEqFO

  let refine_logic new_val =
    if !logic != Unsupported then (
      logic := new_val
    )

  (* ignoring other fields of tasks *)
  let task_eq a b = a.lit_idx = b.lit_idx && C.equal a.clause b.clause

  let register_ss_idx sym sign cl =
    ss_idx := SymSignIdx.update (sym, sign) (fun old ->
      Some (C.ClauseSet.add cl (CCOpt.get_or ~default:C.ClauseSet.empty old))
    ) !ss_idx
  
  
  (* Scan the clause and if it is in supported logic fragment,
     store its literals in the symbol index *)
  let scan_cl_lits cl =
    CCArray.iter (function 
      | L.Equation(lhs,rhs,sign) as lit ->
        let sign = L.is_pos lit in
        if T.is_fo_term lhs && T.is_fo_term rhs then (
          if Type.is_prop (T.ty lhs) then (
            if L.is_predicate_lit lit then (
              register_ss_idx (T.head_exn lhs) sign cl
            ) else (
              (* reasoning with formulas is currently unsupported *)
              logic := Unsupported;
              raise UnsupportedLogic;
            )
          ) else refine_logic EqFO
        ) else (
            logic := Unsupported; 
            raise UnsupportedLogic)
      | L.Int _ | L.Rat _ -> 
        logic := Unsupported;
        raise UnsupportedLogic
      | _ -> ()
    ) (C.lits cl)

  (* Add candidates to already registered task *)
  let add_candidates lit_idx cl cand_cls =
    let t = TaskStore.find (lit_idx, cl) !task_store in
    let new_cand_list = 
      match t.cands with 
      | [] -> cand_cls
      | x :: xs -> x :: (cand_cls @ xs)
    in
    t.cands <- new_cand_list; (* making sure that we do not delete the first element *)
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
                  sign != L.is_pos lit ->
              add_candidates lit_idx cand [clause]
            | _ -> ()
          ) (C.lits cand))
      ) cands;
    in

    match (C.lits clause).(lit_idx) with
    | L.Equation (lhs, rhs, sign) as lit when L.is_predicate_lit lit ->
      assert (T.is_fo_term lhs);
      let hd = T.head_exn lhs in
      let sign = L.is_pos lit in
      let cands =
        C.ClauseSet.to_list
        (CCOpt.get_or
            ~default:C.ClauseSet.empty
            (SymSignIdx.find_opt (hd, not sign) !ss_idx)) in
      if update_others then (
        update_cand_lists hd sign clause cands
      );
      let task = {lit_idx; clause; cands; heap_idx = init_heap_idx} in
      task_store := TaskStore.add (lit_idx, clause) task !task_store;
      (* task_queue := TaskPriorityQueue.add !task_queue task; *)
      TaskPriorityQueue.insert task_queue task
    | _ -> ( (* equation literals do not represent tasks *) )

  (* Update all the bookeeping information when a new clause is introduced *)
  let add_clause cl =
    try 
      scan_cl_lits cl;
      CCArray.iteri (fun lit_idx _ -> register_task lit_idx cl) (C.lits cl)
    with UnsupportedLogic ->
      (* if the initial problem had only supported features (FO logic),
         it cannot jump out of the fragment *)
      invalid_arg "jumped out of the supported logic fragment"

  (* remove the clause from symbol index *)
  let deregister_symbols cl =
    CCArray.iteri (fun lit_idx lit -> 
      match lit with 
      | L.Equation(lhs,_,_) 
        when L.is_predicate_lit lit  ->
        ss_idx :=
          SymSignIdx.update (T.head_exn lhs, L.is_pos lit) (function
            | Some old ->
              let new_ = C.ClauseSet.remove cl old in
              CCOpt.return_if (not (C.ClauseSet.is_empty new_)) new_
            | None -> None (*already removed*)) !ss_idx
      | _ -> ()
    ) (C.lits cl)

  let lock_clause locker locked_task =
    assert(C.id locker == C.id (List.hd locked_task.cands));
    clause_lock := Util.Int_map.update (C.id locker) (fun old_val -> 
      let locked_tasks = CCOpt.get_or ~default:[] old_val in
      Some (locked_task :: locked_tasks)
    ) !clause_lock

  
  (* If clause is removed from the active/passive set, then release
     the locks that it holds, and make all the locked clauses active *)
  let release_locks clause =
    try 
      List.iter (fun task -> 
        match task.cands with
        | x :: xs ->
          assert (C.id x = C.id clause);
          task.cands <- xs;
          assert (not (TaskPriorityQueue.in_heap task));
          TaskPriorityQueue.insert task_queue task
        | [] -> assert false
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
    C.mark_redundant clause;
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
               && L.is_pos lit != sign ->
          begin try
            let subst = Unif.FO.unify_syn (lhs, sc_orig) (lhs', sc_partner) in
            (lhs', subst) :: unifiable, others
          with Unif.Fail -> unifiable, lit :: others end
        | _ -> unifiable, lit :: others
      ) ([], []) (C.lits partner)
    in

    let check_resolvents l_idx orig_cl (unifiable, nonunifiable) =
      let orig_sign = L.is_pos ((C.lits orig_cl).(l_idx)) in
      (* literals against which unifiable part of the clause needs to be checked *)
      let for_tautology_checking =
        List.filter (fun (lit, _) -> 
          L.is_pos lit = orig_sign 
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
          is_valid ||
          (not (CCList.is_empty rest) && (
            (* else, we do L-resolution with the unifiable part extended *)
            let unifiable, rest' =
              CCList.partition (fun (lhs,_) ->
                CCOpt.is_some (
                  List.find_opt (fun (lit, sc) ->
                    let lhs'  = CCOpt.get_exn (L.View.get_lhs lit) in
                    try
                      ignore @@ Unif.FO.unify_syn ~subst (lhs', sc) (lhs, sc_partner);
                      true
                    with Unif.Fail -> false) 
                  (for_tautology_checking)))  
              rest 
            in
            (* clause is not valid *)
            if CCList.is_empty unifiable then false
            else ( 
              try
                let subst = 
                  List.fold_left (fun subst (lhs',_) -> 
                    (* extending the substitution *)
                    Unif.FO.unify_syn ~subst (lhs, sc_partner) (lhs', sc_partner)              
                  ) (subst) unifiable 
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
        (lhs, L.is_pos lit)
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
               && L.is_pos lit != sign 
               && ID.equal (T.head_exn lhs) (T.head_exn lhs') ->      
          lhs' :: same_hds, others
        | _ -> same_hds, lit :: others
      ) ([], []) (C.lits partner)
    in
    
    let lit = (C.lits orig_cl).(lit_idx) in 
    let orig_lhs, orig_sign = 
      match lit with
      | L.Equation(lhs, _, _) when L.is_predicate_lit lit ->
        (lhs, L.is_pos lit)
      | _ -> assert false (* literal must be eligible for BCE *)
    in
    
    let check_resolvents l_idx orig_cl (same_hd_lits, diff_hd_lits) =
      let orig_args = T.args @@ CCOpt.get_exn @@ (L.View.get_lhs (C.lits orig_cl).(l_idx)) in

      (* add equations that correspond to computing a flat resolvent between
         original lit and the one that has new_args arguments to the head *)
      let add_flat_resolvent ~cc new_args =
        List.fold_left (fun acc (lhs,rhs) -> CC.mk_eq acc lhs rhs) 
          cc (List.combine orig_args new_args)
      in

      (* calculating positive, that is negative literals in both clauses *)
      let orig_pos, orig_neg = 
        CCList.partition L.is_pos (CCArray.except_idx (C.lits orig_cl) l_idx) in
      let partner_pos, partner_neg = 
        CCList.partition L.is_pos (diff_hd_lits)
      in
      let all_pos = orig_pos @ partner_pos in
      let all_neg = orig_neg @ partner_neg in

      (* Literals from same_hd_lits part might need to be tested for congruence
         with literals of the same head, but opposite side from either clause *)
      let for_congruence_testing =
        CCList.filter_map (fun lit -> 
          if L.is_predicate_lit lit && L.is_pos lit = orig_sign then (
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
          assert (L.is_neg lit);
          let lhs,rhs,_ = CCOpt.get_exn @@ L.View.as_eqn lit in
          CC.mk_eq acc lhs rhs
        ) (CC.create ~size:16 ()) all_neg
      in

      if CCList.is_empty same_hd_lits then true (* no L-resolvent possible *)
      else (
        let rec check_lit ~cc rest =
          (* validity is achieved without using same_hd_lits literals *)
          let is_valid = List.exists (fun lit -> 
            assert(L.is_pos lit);
            match L.View.as_eqn lit with
            | Some (lhs, rhs, _) -> CC.is_eq cc lhs rhs
            | None -> assert false
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
            let cc_with_lhs = add_flat_resolvent ~cc:orig_cc (T.args orig_lhs) in
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
    if !logic == EqFO then resolvent_is_valid_eq
    else resolvent_is_valid_neq

  (* function that actually performs the blocked clause elimination *)
  let do_eliminate_blocked_clauses () =
    (* checks if each candidate stored in the task has valid L-resolvent.
       if that is not the case, it returns the rest of candidates to be checked. *)
    let process_task task =
      let cl = task.clause in
      let lit_idx = task.lit_idx in
      let validity_checker = get_validity_checker () in
      
      let rec process_candidates = function
        | [] -> []
        | (partner :: rest) as cands ->
          if C.equal cl partner || C.is_redundant partner || validity_checker lit_idx cl partner 
          then process_candidates rest
          else (
            task.cands <- cands;
            lock_clause partner task;
            cands
          )
      in
      
      if not @@ C.is_redundant task.clause then (
        match process_candidates task.cands with
        | [] ->
          deregister_clause cl;
          remove_from_proof_state cl
        | rest -> 
          assert (not (TaskPriorityQueue.in_heap task))
      );

    in


    let module Q = TaskPriorityQueue in
    while not (Q.is_empty task_queue) do
      process_task (Q.remove_min task_queue)
    done

  let steps = ref 0
  (* driver that does that every k-th step of given-clause loop *)
  let eliminate_blocked_clauses () =
    steps := (!steps + 1) mod (Env.flex_get k_check_at);

    if !steps = 0 then do_eliminate_blocked_clauses ()

  let react_clause_addded cl =
    add_clause cl

  let react_clause_removed cl =
    deregister_clause cl


  let initialize () =
    let init_clauses = 
      C.ClauseSet.to_list @@ C.ClauseSet.union
        (Env.ProofState.PassiveSet.clauses ())
        (Env.ProofState.ActiveSet.clauses ())
    in
    begin try
      (* build the symbol index *)
      List.iter scan_cl_lits init_clauses;
      (* create tasks for each clause *)
      List.iter 
        (fun cl ->
          CCArray.iteri (fun lit_idx _ -> 
            register_task ~update_others:false lit_idx cl) 
          (C.lits cl))
      init_clauses;
      (* eliminate clauses *)
      do_eliminate_blocked_clauses ();

      (* clauses begin their life when they are added to the passive set *)
      Signal.on_every Env.ProofState.PassiveSet.on_add_clause react_clause_addded;
      (* clauses can be calculus-removed from the active set only in DISCOUNT loop *)
      Signal.on_every Env.ProofState.ActiveSet.on_remove_clause react_clause_removed;
      (* Clauses are removed from the passive set when they are moved to active.
         In this case clause can me modified or deemed redundant by forward
         modification procedures. we react accordingly.*)
      Signal.on_every Env.on_forward_simplified (fun (c, new_state) -> 
        react_clause_removed c;
        match new_state with
        | Some c' -> react_clause_addded c'
        | _ -> () (* c is redundant *)
      );
      Env.add_clause_elimination_rule ~priority:1 "BCE" eliminate_blocked_clauses
    with UnsupportedLogic ->
      (* releasing possibly used memory *)
      ss_idx := SymSignIdx.empty;
      clause_lock := Util.Int_map.empty;
      task_store := TaskStore.empty;
      TaskPriorityQueue.clear task_queue;
    end;
    Signal.StopListening


  let register () =
    Signal.on Env.on_start initialize

  let setup () =
    if Env.flex_get k_enabled then register ()
end

let _enabled = ref false
let _check_at = ref 10

let extension =
  let action env =
    let module E = (val env : Env.S) in
    let module BCE = Make(E) in

    E.flex_add k_check_at !_check_at;
    E.flex_add k_enabled !_enabled;
    
    BCE.setup ()
  in
  { Extensions.default with Extensions.
                         name="bce";
                         env_actions=[action];
  }

let () =
  Options.add_opts [
    "--bce", Arg.Bool ((:=) _enabled), " scan clauses for AC definitions";
    "--bce-check-every", Arg.Int ((:=) _check_at), " check BCE every n steps of saturation algorithm";
  ]
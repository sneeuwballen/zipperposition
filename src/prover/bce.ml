
(* This file is free software, part of Zipperposition. See file "license" for more details. *)

(** {1 Blocked Clause Elimination} *)

open Logtk

module type S = sig
  module Env : Env.S

  (** {6 Registration} *)

end

module Make(E : Env.S) : S with module Env = E = struct
  module Env = E
  module C = Env.C
  module L = Literal
  module T = Term
  module SymSignIdx = Map.Make (struct 
      type t = (ID.t * bool) 
      let compare = CCPair.compare ID.compare Bool.compare
  end)
  
  type logic = 
    | NEqFO  (* nonequational FO *)
    | EqFO (* equational FO *)
    | Unsupported   (* HO or FO with theories *)
  
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
    mutable active  : bool;
  }

  module TaskStore = Map.Make (struct 
    type t = int * C.t
    let compare (idx_a, cl_a) (idx_b, cl_b) = 
      CCOrd.(<?>) (C.compare cl_a cl_b) (compare, idx_a, idx_b)
  end)

  module TaskPriorityQueue = CCHeap.Make (struct
    type t = bce_check_task
    let leq a b =
      (List.length a.cands < List.length b.cands)
      || (List.length a.cands = List.length b.cands 
            && C.compare a.clause b.clause < 0)
      || (List.length a.cands = List.length b.cands 
            && C.compare a.clause b.clause = 0 
            && a.lit_idx < b.lit_idx)
    end)

  (* (symbol, sign) -> clauses with the corresponding occurence *)
  let ss_idx = ref SymSignIdx.empty
  (* clause (or its id) -> all clauses that it locks *)
  let clause_lock = ref Util.Int_map.empty
  (* a store that implements perfect sharing of tasks  *)
  let task_store = ref TaskStore.empty
  (* priority queue of the tasks to be performed *)
  let task_queue = ref TaskPriorityQueue.empty
  
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
  
  
  (* Register a clause for tracking with BCE system *)
  let register_cl cl =
    CCArray.iter (function 
      | L.Equation(lhs,rhs,sign) as lit ->
        let sign = L.is_pos lit in
        if T.is_fo_term lhs && T.is_fo_term rhs then (
          if Type.is_prop (T.ty lhs) then (
            if L.is_predicate_lit lit then (
              register_ss_idx (T.head_exn lhs) sign cl
            ) else (
              (* reasoning with formulas is currently unsupported *)
              logic := Unsupported
            )
          ) else refine_logic EqFO
        ) else logic := Unsupported
      | L.Int _ | L.Rat _ -> logic := Unsupported
      | _ -> ()
    ) (C.lits cl)

  (* Add candidates to already registered task *)
  let add_candidates lit_idx cl cand_cls =
    let t = TaskStore.find (lit_idx, cl) !task_store in
    t.cands <- cand_cls @ t.cands;
    if t.active then (
      (* unfortunately, CCHeap does not support key increase / decrease operations *)
      task_queue :=
        TaskPriorityQueue.add 
          (TaskPriorityQueue.delete_one task_eq t !task_queue) 
          t
    )

  (* Register a new task, calculate its candidates and make it active.
     Only clauses within the supported logic fragment can be registered. *)
  let register_task lit_idx clause =
    (* insert new clause into the candidate list of previously inserted clauses *)
    let update_cand_lists hd sign clause cands =
      List.iter (fun cand ->
        CCArray.iteri (fun lit_idx lit ->
          match lit with
          | L.Equation(lhs,_,_)
            when L.is_predicate_lit lit  &&
                 ID.equal (T.head_exn lhs) hd &&
                 sign != L.is_pos lit ->
            add_candidates lit_idx cand [clause]
          | _ -> ()
        ) (C.lits cand)
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
      update_cand_lists hd sign clause cands;
      let task = {lit_idx; clause; cands; active=true} in
      task_store := TaskStore.add (lit_idx, clause) task !task_store;
      task_queue := TaskPriorityQueue.add !task_queue task;
      ()
    | _ -> ( (* equation literals do not represent tasks *) )

  (* Update all the bookeeping information when a new clause is introduced *)
  let add_clause cl =
    register_cl cl;
    CCArray.iteri (fun lit_idx _ -> register_task lit_idx cl) (C.lits cl)

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
            | None -> assert false) !ss_idx
      | _ -> ()
    ) (C.lits cl)
  
  (* If clause is removed from the active/passive set, then release
     the locks that it holds, and make all the locked clauses active *)
  let release_locks clause =
    List.iter (fun task -> 
      match task.cands with
      | x :: xs ->
        assert (C.id x = C.id clause);
        task.cands <- xs;
        task.active <- true; 
        task_queue := TaskPriorityQueue.add !task_queue task;
      | [] -> assert false
    ) (Util.Int_map.find (C.id clause) !clause_lock)

  (* remove the clause from the whole BCE tracking system *)
  let remove_clause clause =
    deregister_symbols clause;
    release_locks clause
  
  let resolvent_is_valid_neq lit_idx orig_cl partner =
    assert (lit_idx < C.length orig_cl);
    let sc_orig, sc_partner = 0, 1 in

    let split_partner lhs sign partner = 
      CCArray.foldi (fun (unifiable, others) idx lit ->
        match lit with
        | L.Equation(lhs', _, _) 
          when L.is_predicate_lit lit && L.is_pos lit != sign ->
          begin try
            let subst = Unif.FO.unify_syn (lhs, sc_orig) (lhs', sc_partner) in
            (lhs, subst) :: unifiable, others
          with Unif.Fail -> unifiable, lit :: others end
        | _ -> unifiable, lit :: others
      ) ([], []) (C.lits partner)
    in

    let check_resolvents l_idx orig_cl (unifiable, other_lits) =
      let orig_sign = L.is_pos ((C.lits orig_cl).(l_idx)) in
      if CCList.is_empty unifiable then true
      else (
        let rec check_lit lhs subst rest =
          let is_valid =
            List.exists (fun lit' ->
              CCOpt.is_some (CCArray.findi (fun idx lit ->
                if idx != l_idx && 
                  L.are_opposite_subst ~subst (lit, sc_orig) (lit', sc_partner) then(
                  Some lit)
                else None) 
              (C.lits orig_cl))
             ) other_lits in
          is_valid ||
          (
            not (CCList.is_empty rest) &&
            let unifiable, rest' = CCList.partition (fun (lhs,_) ->
              CCOpt.is_some (CCArray.findi (fun idx lit ->
                let lhs'  = CCOpt.get_exn (L.View.get_lhs lit) in
                if idx != l_idx && 
                  L.is_pos lit = orig_sign then (
                  try
                    ignore @@ Unif.FO.unify_syn ~subst (lhs', sc_orig) (lhs, sc_partner);
                    Some lit
                  with Unif.Fail -> None
                ) else None) 
              (C.lits orig_cl)))  
            rest in

            try
              let subst = 
                List.fold_left (fun (subst) (lhs',_) -> 
                  Unif.FO.unify_syn ~subst (lhs, sc_partner) (lhs', sc_partner)              
                ) (subst) unifiable 
              in
              check_lit lhs subst rest'
            with Unif.Fail -> true
          ) 
        in
        let rec check_l_resolvents others = function
          | (lhs, subst) as x :: xs ->
            check_lit lhs subst (others @ xs) &&
            check_l_resolvents (x :: others) xs
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
end
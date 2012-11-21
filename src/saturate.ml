(*
Zipperposition: a functional superposition prover for prototyping
Copyright (C) 2012 Simon Cruanes

This is free software; you can redistribute it and/or
modify it under the terms of the GNU General Public License
as published by the Free Software Foundation; either version 2
of the License, or (at your option) any later version.

This is distributed in the hope that it will be useful,
but WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
GNU General Public License for more details.

You should have received a copy of the GNU General Public License
along with this program; if not, write to the Free Software
Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA
02110-1301 USA.
*)

(* main saturation algorithm *)

open Types

module C = Clauses
module O = Orderings
module PS = ProofState
module CD = ClauseDag
module Sup = Superposition
module Sel = Selection
module Utils = FoUtils
module Delayed = Delayed

let enable = true
let prof_generate = HExtlib.profile ~enable "generate"
let prof_generate_unary = HExtlib.profile ~enable "generate_unary"
let prof_generate_binary = HExtlib.profile ~enable "generate_binary"
let prof_simplify = HExtlib.profile ~enable "simplify"
let prof_all_simplify = HExtlib.profile ~enable "all_simplify"
let prof_is_redundant = HExtlib.profile ~enable "is_redundant"

(** the status of a state *)
type szs_status = 
  | Unsat of hclause
  | Sat
  | Unknown
  | Error of string 
  | Timeout

let check_timeout = function
  | None -> false
  | Some timeout -> Unix.gettimeofday () > timeout

let set_of_support ~calculus state axioms =
  (* reordonate causes using the ordering of the state *)
  let cs = state.PS.cs in
  let axioms = calculus#preprocess ~cs axioms in
  (* add the axioms to the active set *)
  let axioms_set = PS.add_actives_vec state.PS.axioms_set axioms in
  Utils.debug 1 (lazy (Utils.sprintf "%% added %d clauses to set-of-support" (Vector.size axioms)));
  {state with PS.axioms_set = axioms_set}

(** simplify the clause using the active_set. Returns
    the (renamed) clause and the simplified clause. *)
let simplify ~calculus active_set clause =
  let do_it clause =
    let cs = active_set.PS.a_cs in
    let old_c = PS.relocate_active active_set clause in
    let c = calculus#simplify active_set old_c in
    let c = calculus#basic_simplify ~cs c in
    (if not (C.eq_clause c old_c)
      then Utils.debug 2 (lazy (Utils.sprintf "@[<hov 4>clause @[<h>%a@]@ simplified into @[<h>%a@]@]"
                          !C.pp_clause#pp old_c !C.pp_clause#pp c)));
    old_c, c
  in 
  prof_simplify.HExtlib.profile do_it clause

(** generate all clauses from binary inferences *)
let generate_binary ~calculus active_set clause =
  prof_generate_binary.HExtlib.profile
    (Calculus.do_binary_inferences active_set calculus#binary_rules) clause

(** generate all clauses from unary inferences *)
let generate_unary ~calculus ~cs clause =
  prof_generate_unary.HExtlib.profile
    (Calculus.do_unary_inferences ~cs calculus#unary_rules) clause

(** depth at which unary inferences are performed (max number
    of times inferences are applied recursively to a clause) *)
let unary_max_depth = ref 2

(** generate all clauses from inferences, updating the state (for the
    parent/descendant relation) *)
let generate ~calculus state c =
  let do_it c =
    let acc = Vector.create 30 in
    let cs = state.PS.cs in
    (* an active set containing only the given clause *)
    let given_active_set = PS.singleton_active_set ~cs (C.normalize_clause ~cs c) in
    (* binary clauses *)
    Vector.append acc (generate_binary ~calculus state.PS.axioms_set c);
    Vector.append acc (generate_binary ~calculus state.PS.active_set c);
    Vector.append acc (generate_binary ~calculus given_active_set c);
    (* unary inferences *)
    let unary_queue = Queue.create () in
    Queue.push (c, 0) unary_queue;
    while not (Queue.is_empty unary_queue) do
      let c, depth = Queue.pop unary_queue in
      let c = calculus#basic_simplify ~cs c in   (* simplify a bit the clause *)
      if not (Sup.is_tautology c) then begin
        Vector.push acc c;  (* add the clause to set of inferred clauses *)
        if depth < !unary_max_depth
          then begin
            (* infer clauses from c, add them to the queue *)
            let new_clauses = generate_unary ~calculus ~cs c in
            Vector.iter new_clauses (fun c' -> Queue.push (c', depth+1) unary_queue)
          end
      end
    done;
    acc
  in
  prof_generate.HExtlib.profile do_it c

(** remove direct descendants of the clauses from the passive set *)
let remove_orphans state removed_clauses =
  let passive = state.PS.passive_set
  and cs = state.PS.cs in
  (* remove descendants of removed_clause from the passive set *)
  let passive = Vector.fold removed_clauses passive
    (fun passive c ->
      let descendants = CD.descendants ~cs state.PS.dag c in
      List.iter (fun c -> Utils.debug 3 (lazy (Utils.sprintf "  @[<h>remove orphan clause %a@]"
                !C.pp_clause#pp c))) descendants;
      PS.remove_passives passive descendants)
  in
  {state with PS.passive_set = passive}

(** check whether the clause is redundant w.r.t the active_set *)
let is_redundant ~calculus active_set clause =
  (* forward subsumption check *)
  let c = PS.relocate_active active_set clause in
  prof_is_redundant.HExtlib.profile (calculus#redundant active_set) c

(** find redundant clauses in active_set, w.r.t clause c *)
let subsumed_by ~calculus active_set clause =
  let c = PS.relocate_active active_set clause in
  calculus#redundant_set active_set c

(** Use all simplification rules to convert a clause into a list of maximally
    simplified clauses (possibly empty, if redundant or trivial).
    This is used on generated clauses, and on the given clause. *)
let all_simplify ~cs ~calculus active_set clause =
  let do_it clause =
    let clauses = Vector.create 10
    and queue = Queue.create () in
    Queue.push clause queue;
    while not (Queue.is_empty queue) do
      let c = Queue.pop queue in
      (* usual simplifications *)
      let _, c = simplify ~calculus active_set c in
      if Sup.is_tautology c then () else
      (* list simplification *)
      match calculus#list_simplify ~cs c with
      | None -> Vector.push clauses c (* totally simplified clause *)
      | Some clauses ->
        Vector.iter clauses (fun c' -> Queue.push c' queue) (* process new clauses *)
    done;
    clauses
  in
  prof_all_simplify.HExtlib.profile do_it clause

(** Simplifications to perform on initial clauses *)
let initial_simplifications ~cs ~calculus clauses =
  let v' = Vector.create (Vector.size clauses) in
  Vector.iter clauses
    (fun c ->
      (* apply list simplifications in a flatMap step *)
      match calculus#list_simplify ~cs c with
      | None -> Vector.push v' c
      | Some clauses -> Vector.append v' clauses);
  v'

let given_clause_step ~calculus state =
  let cs = state.PS.cs in
  (* select next given clause *)
  match PS.next_passive_clause state.PS.passive_set with
  | passive_set, None -> state, Sat (* passive set is empty *)
  | passive_set, Some c ->
    let state = { state with PS.passive_set=passive_set } in
    (* simplify given clause w.r.t. active set and SOS, then remove redundant clauses *)
    let _, c = simplify ~calculus state.PS.axioms_set c in
    let given = all_simplify ~cs ~calculus state.PS.active_set c in
    let given = Vector.filter given
      (fun c' -> not (is_redundant ~calculus state.PS.active_set c'))
    in
    if Vector.is_empty given then state, Unknown (* given clause is redundant, after simplification *)
    else
    let c = Vector.pop given in (* choose one clause as the given one *)
    let new_clauses = given in
    (* empty clause found *)
    if c.clits = [||] then state, Unsat (C.hashcons_clause c)
    else begin
      assert (not (is_redundant ~calculus state.PS.active_set c));
      Sel.check_selected c;
      Utils.debug 1 (lazy (Utils.sprintf
                    "============ step with given clause @[<h>%a@] =========="
                    !C.pp_clause#pp c));
      (* an active set containing only the given clause *)
      let given_active_set = PS.singleton_active_set ~cs (C.normalize_clause ~cs c) in
      (* find clauses that are subsumed by c in active_set *)
      let subsumed_active = subsumed_by ~calculus state.PS.active_set c in
      let active_set = PS.remove_actives_vec state.PS.active_set subsumed_active in
      let state = { state with PS.active_set = active_set } in
      let state = remove_orphans state subsumed_active in   (* orphan criterion *)
      (* simplify active set using c TODO write a function for this; TODO use indexing *)
      let simplified_actives = Vector.create 10 in  (* simplified active clauses *)
      let bag_remain, bag_simplified = C.partition_bag
        state.PS.active_set.PS.active_clauses
        (fun hc ->
          (* try to simplify hc using the given clause *)
          let original, simplified = simplify ~calculus given_active_set hc in
          if not (C.eq_clause original simplified)
            then begin
              (* remove the original clause form active_set, save the simplified clause *)
              Vector.push simplified_actives simplified;
              Utils.debug 2 (lazy (Utils.sprintf
                           "@[<hov 4>active clause @[<h>%a@]@ simplified into @[<h>%a@]@]"
                           !C.pp_clause#pp original !C.pp_clause#pp simplified));
              Utils.debug 3 (lazy (Utils.sprintf
                            "@[<hov 4>simplified clause @[<h>%a@]@ has descendants @[<h>%a@]@]"
                            !C.pp_clause#pp original (Utils.pp_list !C.pp_clause#pp_h)
                            (CD.descendants ~cs state.PS.dag original)));
              false
            end else true (* no change *)
        )
      in
      (* the simplified active clauses are removed from active set and
         added to the set of new clauses. *)
      let active_set = PS.remove_active_bag state.PS.active_set bag_simplified in
      let state = { state with PS.active_set = active_set } in
      (* orphan criterion: remove descendants of simplified active clauses *)
      let state = remove_orphans state simplified_actives in
      Vector.append new_clauses simplified_actives;
      (* do inferences w.r.t to the active set, SOS, and c itself *)
      let inferred_clauses = generate ~calculus state c in
      Vector.append new_clauses inferred_clauses;
      (* add given clause to active set *)
      let active_set, _ = PS.add_active state.PS.active_set (C.normalize_clause ~cs c) in
      let state = { state with PS.active_set=active_set } in
      (* simplification of new clauses w.r.t active set; only the non-trivial ones
         are kept (by list-simplify) *)
      let new_clauses =
        let v = Vector.create (Vector.size new_clauses) in
        Vector.iter new_clauses (fun c ->
          let clauses = all_simplify ~cs ~calculus state.PS.active_set c in
          let clauses = Vector.filter clauses (fun c' -> not (is_redundant ~calculus state.PS.active_set c')) in
          let clauses = Vector.map clauses (C.normalize_clause ~cs) in
          Vector.append v clauses);
        v
      in
      Vector.iter new_clauses
        (fun new_c -> Utils.debug 1 (lazy (Utils.sprintf
                                    "    inferred new clause @[<hov 3>%a@]"
                                    !C.pp_clause#pp new_c)));
      (* add new clauses (including simplified active clauses) to passive set *)
      let passive_set = PS.add_passives_vec state.PS.passive_set new_clauses in
      (* update the dag *)
      let dag = CD.updates_vec ~cs state.PS.dag new_clauses in
      let state = {state with PS.passive_set = passive_set; PS.dag = dag} in
      (* test whether the empty clause has been found *)
      try
        let empty_clause = Vector.find new_clauses (fun c -> c.clits = [||]) in
        state, Unsat (C.hashcons_clause empty_clause)
      with Not_found ->
      (* empty clause not found, return unknown *)
      state, Unknown
    end

(** print progress *)
let print_progress steps state =
  let stats = PS.stats state in
  Format.printf "\r%d steps; %d active; %d passive" steps stats.PS.stats_active_clauses
    stats.PS.stats_passive_clauses;
  Format.print_flush ()

let given_clause ?steps ?timeout ?(progress=false) ~calculus state =
  let rec do_step state num =
    if check_timeout timeout then state, Timeout, num else
    begin
    Utils.debug 1 (lazy (Format.sprintf "# iteration %d" num));
    match steps with
    | Some i when num >= i -> state, Unknown, num
    | _ ->
      begin
        if progress && (num mod 10) = 0 then print_progress num state else ();
        (* do one step *)
        let new_state, status = given_clause_step ~calculus state in
        match status with
        | Sat | Unsat _ | Error _ -> state, status, num (* finished *)
        | Timeout -> assert false
        | Unknown ->
          do_step new_state (num+1)  (* do one more step *)
      end
    end
  in
  do_step state 0

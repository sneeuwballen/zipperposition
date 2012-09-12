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
  let ord = state.PS.ord in
  let axioms = calculus#preprocess ~ord axioms in
  let axioms = List.filter (fun c -> not (calculus#trivial c)) axioms in
  (* add the axioms to the active set *)
  let axioms_set = PS.add_actives state.PS.axioms_set axioms in
  Utils.debug 1 (lazy (Utils.sprintf "%% added %d clauses to set-of-support"
                  (List.length axioms)));
  {state with PS.axioms_set = axioms_set}

(** simplify the clause using the active_set. Returns
    the (renamed) clause and the simplified clause. *)
let simplify ~calculus active_set clause =
  let ord = active_set.PS.a_ord in
  let old_c = PS.relocate_active active_set clause in
  let c = calculus#simplify active_set old_c in
  let c = calculus#basic_simplify ~ord c in
  (if not (C.eq_clause c old_c)
    then Utils.debug 2 (lazy (Utils.sprintf "clause @[<h>%a@] simplified into @[<h>%a@]"
                        !C.pp_clause#pp old_c !C.pp_clause#pp c)));
  old_c, c

(** generate all clauses from binary inferences *)
let generate_binary ~calculus active_set clause =
  Calculus.do_binary_inferences active_set calculus#binary_rules clause

(** generate all clauses from unary inferences *)
let generate_unary ~calculus ~ord clause =
  Calculus.do_unary_inferences ~ord calculus#unary_rules clause

(** generate all clauses from inferences, updating the state (for the
    parent/descendant relation) *)
let generate ~calculus state c =
  let ord = state.PS.ord in
  (* an active set containing only the given clause *)
  let given_active_set = PS.singleton_active_set ~ord (C.normalize_clause ~ord c) in
  (* binary clauses *)
  let binary_clauses = [] in
  let binary_clauses = List.rev_append
    (generate_binary ~calculus state.PS.axioms_set c) binary_clauses in
  let binary_clauses = List.rev_append
    (generate_binary ~calculus state.PS.active_set c) binary_clauses in
  let binary_clauses = List.rev_append
    (generate_binary ~calculus given_active_set c) binary_clauses in
  (* unary inferences *)
  let unary_clauses = generate_unary ~calculus ~ord c in
  let new_clauses =  List.rev_append unary_clauses binary_clauses in
  new_clauses

(** remove direct descendants of the clauses from the passive set *)
let remove_orphans state removed_clauses =
  let passive = state.PS.passive_set
  and ord = state.PS.ord in
  let passive =
    List.fold_left
      (fun passive removed_clause ->
        (* remove descendnts of removed_clause from the passive set *)
        let descendants = CD.descendants ~ord state.PS.dag removed_clause in
        List.iter (fun c ->
          Utils.debug 3 (lazy (Utils.sprintf "  @[<h>remove orphan clause %a@]"
                         !C.pp_clause#pp c))) descendants;
        PS.remove_passives passive descendants)
      passive removed_clauses
  in
  {state with PS.passive_set = passive}

(** check whether the clause is redundant w.r.t the active_set *)
let is_redundant ~calculus active_set clause =
  (* forward subsumption check *)
  let c = PS.relocate_active active_set clause in
  calculus#redundant active_set c

(** find redundant clauses in active_set, w.r.t clause c *)
let subsumed_by ~calculus active_set clause =
  let c = PS.relocate_active active_set clause in
  calculus#redundant_set active_set c

let given_clause_step ~calculus state =
  let ord = state.PS.ord in
  (* select next given clause *)
  match PS.next_passive_clause state.PS.passive_set with
  | passive_set, None -> state, Sat (* passive set is empty *)
  | passive_set, Some c ->
    let state = { state with PS.passive_set=passive_set } in
    (* simplify given clause w.r.t. active set and SOS *)
    let _, c = simplify ~calculus state.PS.active_set c in
    let _, c = simplify ~calculus state.PS.axioms_set c in
    (* empty clause found *)
    if c.clits = [] then state, Unsat (C.hashcons_clause c)
    (* tautology or subsumed, useless *)
    else if calculus#trivial c ||
            is_redundant ~calculus state.PS.active_set c
    then state, Unknown
    else begin
      (* select literals *)
      let c = C.select_clause state.PS.state_select c in
      Sel.check_selected c;
      Utils.debug 1 (lazy (Utils.sprintf
                    "============ step with given clause @[<h>%a@] =========="
                    !C.pp_clause#pp c));
      (* an active set containing only the given clause *)
      let given_active_set = PS.singleton_active_set ~ord (C.normalize_clause ~ord c) in
      (* find clauses that are subsumed by c in active_set *)
      let subsumed_active = subsumed_by ~calculus state.PS.active_set c in
      let active_set = PS.remove_actives state.PS.active_set subsumed_active in
      let state = { state with PS.active_set = active_set } in
      let state = remove_orphans state subsumed_active in   (* orphan criterion *)
      (* simplify active set using c TODO write a function for this; TODO use indexing *)
      let simplified_actives = ref [] in  (* simplified active clauses *)
      let bag_remain, bag_simplified = C.partition_bag
        state.PS.active_set.PS.active_clauses
        (fun hc ->
          (* try to simplify hc using the given clause *)
          let original, simplified = simplify ~calculus given_active_set hc in
          if not (C.eq_clause original simplified)
            then begin
              (* remove the original clause form active_set, save the simplified clause *)
              simplified_actives := simplified :: !simplified_actives;
              Utils.debug 2 (lazy (Utils.sprintf
                           "active clause @[<h>%a@] simplified into @[<h>%a@]"
                           !C.pp_clause#pp original !C.pp_clause#pp simplified));
              Utils.debug 3 (lazy (Utils.sprintf
                            "simplified clause %a has descendants %a"
                            !C.pp_clause#pp original (Utils.pp_list !C.pp_clause#pp_h)
                            (CD.descendants ~ord state.PS.dag original)));
              false
            end else true (* no change *)
        )
      in
      (* the simplified active clauses are removed from active set and
         added to the set of new clauses. *)
      let active_set = PS.remove_active_bag state.PS.active_set bag_simplified in
      let state = { state with PS.active_set = active_set } in
      let state = remove_orphans state !simplified_actives in  (* orphan criterion *)
      let new_clauses = List.map (C.normalize_clause ~ord) !simplified_actives in
      (* do inferences w.r.t to the active set, SOS, and c itself *)
      let inferred_clauses = generate ~calculus state c in
      let new_clauses = List.rev_append inferred_clauses new_clauses in
      (* add given clause to active set *)
      let active_set, _ = PS.add_active state.PS.active_set (C.normalize_clause ~ord c) in
      let state = { state with PS.active_set=active_set } in
      (* simplification of new clauses w.r.t active set; only the non-trivial ones
         are kept *)
      let new_clauses = List.fold_left
        (fun new_clauses c ->
          let c, simplified_c = simplify ~calculus state.PS.active_set c in
          (* do not keep trivial  clauses *)
          if calculus#trivial simplified_c
            then new_clauses
            else simplified_c::new_clauses)
        [] new_clauses
      in
      List.iter
        (fun new_c -> Utils.debug 1 (lazy (Utils.sprintf
                                    "    inferred new clause @[<hov 3>%a@]"
                                    !C.pp_clause#pp new_c))) new_clauses;
      (* add new clauses (including simplified active clauses) to passive set *)
      let passive_set = PS.add_passives state.PS.passive_set new_clauses in
      (* update the dag *)
      let dag = CD.updates ~ord state.PS.dag new_clauses in
      let state = {state with PS.passive_set = passive_set; PS.dag = dag} in
      (* test whether the empty clause has been found *)
      try
        let empty_clause = List.find (fun c -> c.clits = []) new_clauses in
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
        if progress then print_progress num state else ();
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

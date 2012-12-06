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
let prof_subsumed_by = HExtlib.profile ~enable "subsumed_by"

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

(** simplify the hclause using the active_set. Returns both the
    hclause and the simplified hclause. *)
let simplify_ ~calculus active_set idx old_hc =
  let ord = active_set.PS.a_ord in
  (* rename clause *)
  let hc = calculus#simplify active_set idx old_hc in
  let hc = calculus#basic_simplify ~ord hc in
  (if not (C.eq_hclause hc old_hc)
    then Utils.debug 2 (lazy (Utils.sprintf "@[<hov 4>clause @[<h>%a@]@ simplified into @[<h>%a@]@]"
                        !C.pp_clause#pp_h old_hc !C.pp_clause#pp_h hc)));
  old_hc, hc

let simplify ~calculus active_set idx hc =
  prof_simplify.HExtlib.profile (simplify_ ~calculus active_set idx) hc

(** Simplify the active set using the given clause. TODO use indexing *)
let backward_simplify ~calculus state active_set given_active_set given =
  let ord = state.PS.ord in
  let simplified_actives = ref [] in
  (* unit index just for the clause *)
  let idx = Dtree.unit_index#add_clause given in
  simplified_actives,
  C.CSet.partition active_set.PS.active_clauses
    (fun hc ->
      (* try to simplify hc using the given clause *)
      let original, simplified = simplify ~calculus given_active_set idx hc in
      if not (C.eq_hclause original simplified)
        then begin
          (* remove the original clause form active_set, save the simplified clause *)
          simplified_actives := simplified :: !simplified_actives;
          Utils.debug 2 (lazy (Utils.sprintf
                       "@[<hov 4>active clause @[<h>%a@]@ simplified into @[<h>%a@]@]"
                       !C.pp_clause#pp_h original !C.pp_clause#pp_h simplified));
          Utils.debug 3 (lazy (Utils.sprintf
                        "@[<hov 4>simplified clause @[<h>%a@]@ has descendants @[<h>%a@]@]"
                        !C.pp_clause#pp_h original (Utils.pp_list !C.pp_clause#pp_h)
                        (CD.descendants ~ord state.PS.dag original)));
          false
        end else true) (* no change *)

(** generate all clauses from binary inferences *)
let generate_binary ~calculus active_set clause =
  prof_generate_binary.HExtlib.profile
    (Calculus.do_binary_inferences active_set calculus#binary_rules) clause

(** generate all clauses from unary inferences *)
let generate_unary ~calculus ~ord clause =
  prof_generate_unary.HExtlib.profile
    (Calculus.do_unary_inferences ~ord calculus#unary_rules) clause

(** depth at which unary inferences are performed (max number
    of times inferences are applied recursively to a clause) *)
let unary_max_depth = ref 2

(** generate all clauses from inferences, updating the state (for the
    parent/descendant relation) *)
let generate_ ~calculus state given =
  let ord = state.PS.ord in
  (* binary clauses *)
  let binary_clauses = generate_binary ~calculus state.PS.active_set given in
  (* unary inferences *)
  let unary_clauses = ref []
  and unary_queue = Queue.create () in
  Queue.push (given, 0) unary_queue;
  while not (Queue.is_empty unary_queue) do
    let hc, depth = Queue.pop unary_queue in
    let hc = calculus#basic_simplify ~ord hc in (* simplify a bit the clause *)
    if not (Sup.is_tautology hc) then begin
      unary_clauses := hc :: !unary_clauses;    (* add the clause to set of inferred clauses *)
      if depth < !unary_max_depth
        then begin
          (* infer clauses from c, add them to the queue *)
          let new_clauses = generate_unary ~calculus ~ord hc in
          List.iter (fun hc' -> Queue.push (hc', depth+1) unary_queue) new_clauses
        end
    end
  done;
  let new_clauses =  List.rev_append !unary_clauses binary_clauses in
  new_clauses

let generate ~calculus state hc =
  prof_generate.HExtlib.profile (generate_ ~calculus state) hc

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
                         !C.pp_clause#pp_h c))) descendants;
        PS.remove_passives passive descendants)
      passive removed_clauses
  in
  {state with PS.passive_set = passive}

(** check whether the clause is redundant w.r.t the active_set *)
let is_redundant ~calculus active_set hc =
  prof_is_redundant.HExtlib.profile (calculus#redundant active_set) hc

(** find redundant clauses in active_set, w.r.t clause c *)
let subsumed_by ~calculus active_set hc =
  prof_subsumed_by.HExtlib.profile (calculus#redundant_set active_set) hc

(** Use all simplification rules to convert a clause into a list of maximally
    simplified clauses (possibly empty, if redundant or trivial).
    This is used on generated clauses, and on the given clause. *)
let all_simplify_ ~ord ~calculus ~select active_set idx clause =
  let clauses = ref []
  and queue = Queue.create () in
  Queue.push clause queue;
  while not (Queue.is_empty queue) do
    let c = Queue.pop queue in
    if Sup.is_tautology c then () else
    (* usual simplifications *)
    let _, c = simplify ~calculus active_set idx c in
    let c = C.select_clause ~select c in
    (* list simplification *)
    match calculus#list_simplify ~ord ~select c with
    | None -> clauses := c :: !clauses (* totally simplified clause *)
    | Some clauses ->
      List.iter (fun c' -> Queue.push c' queue) clauses (* process new clauses *)
  done;
  !clauses

let all_simplify ~ord ~calculus ~select active_set clause =
  prof_all_simplify.HExtlib.profile (all_simplify_ ~ord ~calculus ~select active_set) clause

(** Simplifications to perform on initial clauses *)
let initial_simplifications ~ord ~select ~calculus clauses =
  List.fold_left
    (fun acc c ->
      (* apply list simplifications in a flatMap step *)
      match calculus#list_simplify ~ord ~select c with
      | None -> c::acc
      | Some clauses -> List.rev_append clauses acc)
    [] clauses

let given_clause_step ~calculus state =
  let ord = state.PS.ord
  and select = state.PS.state_select in
  (* select next given clause *)
  match PS.next_passive_clause state.PS.passive_set with
  | passive_set, None -> state, Sat (* passive set is empty *)
  | passive_set, Some hc ->
    let state = { state with PS.passive_set=passive_set } in
    (* simplify given clause w.r.t. active set and SOS, then remove redundant clauses *)
    let c_list = all_simplify ~ord ~calculus ~select state.PS.active_set state.PS.state_index hc in
    let c_list = List.filter
      (fun hc' -> not (is_redundant ~calculus state.PS.active_set hc'))
      c_list in
    match c_list with
    | [] -> state, Unknown  (* all simplifications are redundant *)
    | hc::new_clauses ->     (* select first clause, the other ones are passive *) 
    (* empty clause found *)
    if hc.hclits = [||]
    then state, Unsat hc
    else begin
      assert (not (is_redundant ~calculus state.PS.active_set hc));
      (* select literals *)
      let hc = C.select_clause select hc in
      Sel.check_selected hc;
      Utils.debug 1 (lazy (Utils.sprintf
                    "============ step with given clause @[<h>%a@] =========="
                    !C.pp_clause#pp_h hc));
      (* an active set containing only the given clause *)
      let given_active_set = PS.singleton_active_set ~ord hc in
      (* find clauses that are subsumed by c in active_set *)
      let subsumed_active = subsumed_by ~calculus state.PS.active_set hc in
      let active_set = PS.remove_actives state.PS.active_set subsumed_active in
      let state = { state with PS.active_set = active_set } in
      let state = remove_orphans state subsumed_active in   (* orphan criterion *)
      (* simplify active set using c *)
      let simplified_actives, (set_remain, set_simplified) =
        backward_simplify ~calculus state state.PS.active_set given_active_set hc in
      (* the simplified active clauses are removed from active set and
         added to the set of new clauses. *)
      let active_set = PS.remove_active_set state.PS.active_set set_simplified in
      let state = { state with PS.active_set = active_set } in
      let state = remove_orphans state !simplified_actives in  (* orphan criterion *)
      let new_clauses = List.rev_append !simplified_actives new_clauses in
      let new_clauses = List.filter
        (fun hc -> not (is_redundant ~calculus given_active_set hc)) new_clauses in
      (* add given clause to active set *)
      let active_set = PS.add_active state.PS.active_set hc in
      let state = { state with PS.active_set=active_set } in
      (* do inferences between c and the active set (including c) *)
      let inferred_clauses = generate ~calculus state hc in
      let new_clauses = List.rev_append inferred_clauses new_clauses in
      (* simplification of new clauses w.r.t active set; only the non-trivial ones
         are kept (by list-simplify) *)
      let new_clauses = List.fold_left
        (fun new_clauses hc ->
          let cs = all_simplify ~ord ~select ~calculus state.PS.active_set state.PS.state_index hc in
          List.rev_append cs new_clauses)
        [] new_clauses
      in
      List.iter
        (fun new_c -> Utils.debug 1 (lazy (Utils.sprintf
                                    "    inferred new clause @[<hov 3>%a@]"
                                    !C.pp_clause#pp_h new_c))) new_clauses;
      (* add new clauses (including simplified active clauses) to passive set *)
      let passive_set = PS.add_passives state.PS.passive_set new_clauses in
      (* update the dag *)
      let dag = CD.updates ~ord state.PS.dag new_clauses in
      let state = {state with PS.passive_set = passive_set; PS.dag = dag;} in
      (* add new clauses to the simplification index *)
      let state = PS.add_rules state new_clauses in
      (* test whether the empty clause has been found *)
      try
        let empty_clause = List.find (fun hc -> hc.hclits = [||]) new_clauses in
        state, Unsat empty_clause
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
        (* print progress *)
        if progress && (num mod 10) = 0 then print_progress num state else ();
        (* some cleanup from time to time *)
        let state = if (num mod 100 = 0)
          then (Utils.debug 1 (lazy "%% perform cleanup of passive set");
               {state with PS.passive_set= PS.clean_passive state.PS.passive_set})
          else state in
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

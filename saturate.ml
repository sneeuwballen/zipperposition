(* main saturation algorithm *)

open Types
open Hashcons

module C = Clauses
module PS = ProofState
module Sup = Superposition
module Utils = FoUtils

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

(** the list of inference rules *)
let inference_rules =
  [("infer_active", Sup.infer_active);
   ("infer_passive", Sup.infer_passive);
   ("equality_resolution", Sup.infer_equality_resolution);
   ("equality_factoring", Sup.infer_equality_factoring);
   ]

(** do inferences that involve the given clause *)
let do_inferences state c = 
  let active_set = state.PS.active_set in
  HExtlib.flatten_map
    (fun (name, rule) ->
      Utils.debug 3 (lazy ("#  apply rule " ^ name));
      rule active_set c)
    inference_rules

let given_clause_step state =
  let ord = state.PS.ord in
  (* select next given clause *)
  match PS.next_passive_clause state.PS.passive_set with
  | passive_set, None -> state, Sat (* passive set is empty *)
  | passive_set, Some c ->
    let state = { state with PS.passive_set=passive_set } in
    (* rename variables in c to avoid collisions *)
    let c = PS.relocate_active state.PS.active_set c.node in
    let c = Sup.basic_simplify ~ord c in
    (* empty clause found *)
    if c.clits = [] then state, Unsat (C.hashcons_clause c)
    (* tautology, useless *)
    else if Sup.is_tautology c then state, Unknown
    else begin
      Utils.debug 1 (lazy (Format.sprintf "# *** step with given clause %s"
                    (Utils.on_buffer C.pp_clause c)));
      (* do inferences (TODO simplify before) *)
      let new_clauses = do_inferences state c in
      let new_clauses = List.map (Sup.basic_simplify ~ord) new_clauses in
      let new_clauses = List.filter (fun c -> not (Sup.is_tautology c)) new_clauses in
      (* only keep clauses that are not already in active_set *)
      let new_clauses = List.filter (fun c ->
        let hc = C.hashcons_clause c in
        not (C.is_in_bag state.PS.active_set.PS.active_clauses hc.tag))
        new_clauses in
      List.iter
        (fun new_c -> Utils.debug 1 (lazy
          (Format.sprintf "#    infered new clause %s"
          (Utils.on_buffer C.pp_clause new_c))))
        new_clauses;
      (* add new clauses to passive set, and given clause to active set *)
      let passive_set = PS.add_passives state.PS.passive_set new_clauses
      and active_set, _ = PS.add_active state.PS.active_set (C.normalize_clause ~ord c) in
      let state = { state with PS.passive_set=passive_set; PS.active_set=active_set} in
      (* test whether the empty clause has been found *)
      try
        let empty_clause = List.find (fun c -> c.clits = []) new_clauses in
        state, Unsat (C.hashcons_clause empty_clause)
      with Not_found ->
      (* empty clause not found, return unknown *)
      state, Unknown
    end

let given_clause ?steps ?timeout state =
  let rec do_step state num =
    if check_timeout timeout then state, Timeout, num else
    begin
    Utils.debug 1 (lazy (Format.sprintf "# iteration %d" num));
    match steps with
    | Some i when num >= i -> state, Unknown, num
    | _ ->
      begin
        (* do one step *)
        let new_state, status = given_clause_step state in
        match status with
        | Sat | Unsat _ | Error _ -> state, status, num (* finished *)
        | Timeout -> assert false
        | Unknown ->
          do_step new_state (num+1)  (* do one more step *)
      end
    end
  in
  do_step state 0


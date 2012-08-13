(* main saturation algorithm *)

open Types
open Hashcons

module C = Clauses
module PS = ProofState
module Sup = Superposition
module Utils = FoUtils

let debug_enabled = ref false
let set_debug b = debug_enabled := b
let debug s = if !debug_enabled then print_endline (Lazy.force s) else ()

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
      debug (lazy ("#  apply rule " ^ name));
      rule active_set c)
    inference_rules

let given_clause_step state =
  (* select next given clause *)
  match PS.next_passive_clause state.PS.passive_set with
  | passive_set, None -> state, Sat (* passive set is empty *)
  | passive_set, Some c ->
    let state = { state with PS.passive_set=passive_set } in
    (* rename variables in c to avoid collisions *)
    let c = PS.relocate_active state.PS.active_set c.node in
    debug (lazy (Format.sprintf "# *** step with given clause %s"
                 (Utils.on_buffer Pp.pp_clause c)));
    (* do inferences (TODO simplify before) *)
    let new_clauses = do_inferences state c in
    List.iter
      (fun new_c -> debug (lazy
        (Format.sprintf "  infered new clause %s"
          (Utils.on_buffer Pp.pp_clause new_c))))
      new_clauses;
    (* add new clauses to passive set *)
    let passive_set = PS.add_passives state.PS.passive_set new_clauses in
    let state = { state with PS.passive_set=passive_set} in
    (* test whether the empty clause has been found *)
    try
      let empty_clause = List.find (fun c -> c.clits = []) new_clauses in
      state, Unsat (C.hashcons_clause empty_clause)
    with Not_found ->
    (* empty clause not found, return unknown *)
    state, Unknown

let given_clause ?max_steps ?timeout state =
  let rec do_step state num =
    if check_timeout timeout then state, Unknown else
    match max_steps with
    | Some i when i >= num -> state, Unknown
    | _ ->
    (* do one step *)
    let new_state, status = given_clause_step state in
    match status with
    | Sat | Unsat _ | Error _ -> state, status (* finished *)
    | Timeout -> assert false
    | Unknown ->
      do_step new_state (num+1)  (* do one more step *)
  in do_step state 0


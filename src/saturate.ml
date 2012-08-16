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

let given_clause_step state =
  let ord = state.PS.ord in
  (* select next given clause *)
  match PS.next_passive_clause state.PS.passive_set with
  | passive_set, None -> state, Sat (* passive set is empty *)
  | passive_set, Some c ->
    let state = { state with PS.passive_set=passive_set } in
    (* rename variables in c to avoid collisions *)
    let c = PS.relocate_active state.PS.active_set c.node in
    (* simplify given clause w.r.t. active set *)
    let old_c = c
    and c = Sup.demodulate state.PS.active_set [] c in
    let c = Sup.basic_simplify ~ord c in
    (if not (C.eq_clause c old_c)
      then Utils.debug 2 (lazy (Utils.sprintf "clause %a simplified into %a"
                        (C.pp_clause ~sort:false) old_c (C.pp_clause ~sort:false) c)));
    (* empty clause found *)
    if c.clits = [] then state, Unsat (C.hashcons_clause c)
    (* tautology, useless *)
    else if Sup.is_tautology c then state, Unknown
    else begin
      Utils.debug 1 (lazy (Format.sprintf "# *** step with given clause %s"
                    (Utils.on_buffer C.pp_clause c)));
      (* an active set containing only the given clause *)
      let given_active_set = PS.singleton_active_set ~ord (C.normalize_clause ~ord c) in
      (* then rename c *)
      let c, _ =
        let maxvar = max (state.PS.active_set.PS.active_clauses.C.bag_maxvar)
                         (given_active_set.PS.active_clauses.C.bag_maxvar) in
        C.fresh_clause ~ord maxvar c
      in
      (* do inferences w.r.t to the active set, and c itself *)
      let new_clauses = 
        List.rev_append (Sup.do_inferences state.PS.active_set inference_rules c)
                        (Sup.do_inferences given_active_set inference_rules c)
      in
      let new_clauses = List.map
        (fun c -> Sup.basic_simplify ~ord (C.normalize_clause ~ord c))
        new_clauses in
      let new_clauses = List.filter (fun c -> not (Sup.is_tautology c)) new_clauses in
      (* add given clause to active set and demodulate active set w.r.t itself *)
      let active_set, _ = PS.add_active state.PS.active_set (C.normalize_clause ~ord c) in
      let simplified_actives = ref [] in  (* simplified active clauses *)
      let bag_remain, bag_simplified = C.partition_bag
        active_set.PS.active_clauses
        (fun hc ->
          (* try to simplify hc using the given clause *)
          let renamed = PS.relocate_active given_active_set hc.node in
          let simplified = Sup.demodulate given_active_set [hc.tag] renamed in
          if C.eq_clause simplified renamed
            then true
            else begin
              let simplified = Sup.basic_simplify ~ord simplified in
              simplified_actives := simplified :: !simplified_actives;
              Utils.debug 2 (lazy (Utils.sprintf "active clause %a simplified into %a"
                           (C.pp_clause ~sort:false) renamed
                           (C.pp_clause ~sort:false) simplified));
              false
            end
        )
      in
      (* the simplified active clauses are removed from active set and
         added to the set of new clauses *)
      let active_set = {active_set with PS.active_clauses = bag_remain } in
      let new_clauses = List.rev_append !simplified_actives new_clauses in
      (* only keep clauses that are not already in active_set *)
      let new_clauses =
        List.filter
          (fun c ->
            let hc = C.hashcons_clause c in
            not (C.is_in_bag state.PS.active_set.PS.active_clauses hc.tag))
          new_clauses
      in
      List.iter
        (fun new_c -> Utils.debug 1 (lazy (Utils.sprintf "#    infered new clause %a"
                                           (C.pp_clause ~sort:false) new_c)))
        new_clauses;
      (* add new clauses (including simplified active clauses) to passive set
         TODO remove orphans of simplified active clauses *)
      let passive_set = PS.add_passives state.PS.passive_set new_clauses in
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
    (if (num mod 50) = 0 then state.PS.ord#clear_cache ());
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


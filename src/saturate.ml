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

open Basic

module C = Clauses
module O = Orderings
module PS = ProofState
module Sup = Superposition
module Sel = Selection
module Utils = FoUtils
module Delayed = Delayed

let stat_redundant_given = mk_stat "redundant given clauses"
let stat_processed_given = mk_stat "processed given clauses"

(** the status of a state *)
type 'a szs_status = 
  | Unsat of 'a
  | Sat
  | Unknown
  | Error of string 
  | Timeout

let check_timeout = function
  | None -> false
  | Some timeout -> Unix.gettimeofday () > timeout

(** One iteration of the main loop ("given clause loop") *)
let given_clause_step ?(generating=true) ~env num =
  let ctx = env.Env.ctx in
  let ord = ctx.ctx_ord in
  let experts = Env.get_experts env in
  Utils.debug 3 "@[<hov2>env for next given loop:@; %a@]@." Env.pp env;
  (* select next given clause *)
  match Env.next_passive ~env with
  | None -> Sat (* passive set is empty *)
  | Some hc ->
    (* simplify given clause w.r.t. active set, then remove redundant clauses *)
    let c_list = Env.all_simplify ~env hc in
    let c_list = List.filter
      (fun hc' -> not (Env.is_redundant ~env hc'))
      c_list in
    match c_list with
    | [] -> 
      Utils.debug 2 "%% given clause %a is redundant" !C.pp_clause#pp_h hc;
      incr_stat stat_redundant_given;
      Unknown  (* all simplifications are redundant *)
    | hc::_ when C.is_empty hc ->
      Unsat hc  (* empty clause found *)
    | hc::new_clauses when Experts.Set.is_redundant experts hc ->
      Utils.debug 2 "%% given clause %a is redundant" !C.pp_clause#pp_h hc;
      Env.add_simpl ~env (Sequence.singleton hc);
      Env.add_passive ~env (Sequence.of_list new_clauses);
      Unknown  (* redundant given clause *)
    | hc::new_clauses ->
      let new_clauses = Vector.from_list new_clauses in
      (* select first clause, the other ones are passive *) 
      assert (not (Env.is_redundant ~env hc));
      (* process the given clause! *)
      incr_stat stat_processed_given;
      C.check_ord_hclause ~ord hc;
      Utils.debug 2 "%% ============ step %5d  ============" num;
      Utils.debug 1 "%% @[<h>%a@]" !C.pp_clause#pp_h hc;
      (* yield control to meta-prover *)
      Vector.append_seq new_clauses (Env.meta_step ~env hc);
      (* find clauses that are subsumed by given in active_set *)
      let subsumed_active = Sequence.of_list (Env.subsumed_by ~env hc) in
      Env.remove_active ~env subsumed_active;
      Env.remove_simpl ~env subsumed_active;
      Env.remove_orphans ~env subsumed_active; (* orphan criterion *)
      (* add given clause to simpl_set *)
      Env.add_simpl ~env (Sequence.singleton hc);
      (* simplify active set using c *)
      let simplified_actives, newly_simplified = Env.backward_simplify ~env hc in
      let simplified_actives = C.CSet.to_seq simplified_actives in
      (* the simplified active clauses are removed from active set and
         added to the set of new clauses. Their descendants are also removed
         from passive set *)
      Env.remove_active ~env simplified_actives;
      Env.remove_simpl ~env simplified_actives;
      Env.remove_orphans ~env simplified_actives;
      Vector.append_seq new_clauses newly_simplified;
      (* add given clause to active set *)
      Env.add_active ~env (Sequence.singleton hc);
      (* do inferences between c and the active set (including c),
         if [generate] is set to true *)
      let inferred_clauses = if generating
        then Env.generate ~env hc
        else Sequence.empty in
      (* simplification of inferred clauses w.r.t active set; only the non-trivial ones
         are kept (by list-simplify) *)
      let inferred_clauses = Sequence.flatMap
        (fun hc ->
          let cs = Env.forward_simplify ~env hc in
          (* keep clauses  that are not redundant *)
          let cs = Sequence.filter (fun hc -> not (Env.is_trivial ~env hc)) cs in
          cs)
        inferred_clauses
      in
      Vector.append_seq new_clauses inferred_clauses;
      (if Utils.debug_level () >= 2 then
        Vector.iter new_clauses
          (fun new_c -> Utils.debug 2 "    inferred new clause @[<hov 3>%a@]"
            !C.pp_clause#pp_h new_c));
      (* add new clauses (including simplified active clauses) to passive set and simpl_set *)
      Env.add_passive ~env (Vector.to_seq new_clauses);
      (* test whether the empty clause has been found *)
      match Env.get_some_empty_clause ~env with
      | None -> Unknown
      | Some empty_clause -> Unsat empty_clause

(** print progress *)
let print_progress ~env steps =
  let num_active, num_passive, num_simpl = Env.stats ~env in
  Format.printf "\r%% %d steps; %d active; %d passive; %d simpl; time %.1f s@?"
    steps num_active num_passive num_simpl (Utils.get_total_time ())

let given_clause ?(generating=true) ?steps ?timeout ~env =
  let rec do_step num =
    if check_timeout timeout then Timeout, num else
    match steps with
    | Some i when num >= i -> Unknown, num
    | _ ->
      begin
        (* print progress *)
        (if (Env.get_params ~env).param_progress && (num mod 10) = 0 then print_progress ~env num);
        (* some cleanup from time to time *)
        (if (num mod 1000 = 0)
          then begin
            Utils.debug 1 "%% perform cleanup of hashcons and passive set";
            Clauses.CHashcons.clean ();
            Terms.H.clean ();
            Env.clean_passive ~env;
          end);
        (* do one step *)
        let status = given_clause_step ~generating ~env num in
        match status with
        | Sat | Unsat _ | Error _ -> status, num (* finished *)
        | Timeout -> assert false
        | Unknown -> do_step (num+1)
      end
  in
  do_step 0

(** Simplifications to perform on initial clauses *)
let presaturate ~env =
  given_clause ?steps:None ?timeout:None ~generating:false ~env

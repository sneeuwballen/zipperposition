
(*
Zipperposition: a functional superposition prover for prototyping
Copyright (c) 2013, Simon Cruanes
All rights reserved.

Redistribution and use in source and binary forms, with or without
modification, are permitted provided that the following conditions are met:

Redistributions of source code must retain the above copyright notice, this
list of conditions and the following disclaimer.  Redistributions in binary
form must reproduce the above copyright notice, this list of conditions and the
following disclaimer in the documentation and/or other materials provided with
the distribution.

THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS "AS IS" AND
ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED
WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE ARE
DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT HOLDER OR CONTRIBUTORS BE LIABLE
FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL
DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR
SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER
CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY,
OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE
OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
*)

(** {1 Main saturation algorithm.}
    It uses inference rules and simplification rules from Superposition. *)

open Logtk
open Params

module C = Clause
module O = Ordering
module PS = ProofState
module Sup = Superposition
module Sel = Selection

let stat_redundant_given = Util.mk_stat "redundant given clauses"
let stat_processed_given = Util.mk_stat "processed given clauses"

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
  let ord = Ctx.ord ctx in
  let experts = Env.get_experts env in
  Util.debug 3 "@[<hov2>env for next given loop:@; %a@]@." Env.pp env;
  (* select next given clause *)
  match Env.next_passive ~env with
  | None -> Sat (* passive set is empty *)
  | Some c ->
    (* simplify given clause w.r.t. active set, then remove redundant clauses *)
    let c_list = Env.all_simplify ~env c in
    let c_list = List.filter
      (fun c' -> not (Env.is_redundant ~env c'))
      c_list
    in
    match c_list with
    | [] -> 
      Util.debug 2 "%% given clause %a is redundant" C.pp_debug c;
      Util.incr_stat stat_redundant_given;
      Unknown  (* all simplifications are redundant *)
    | c::_ when C.is_empty c ->
      Unsat c  (* empty clause found *)
    | c::new_clauses when Experts.Set.is_redundant experts c ->
      Util.debug 2 "%% given clause %a is redundant" C.pp_debug c;
      Env.add_simpl ~env (Sequence.singleton c);
      Env.add_passive ~env (Sequence.of_list new_clauses);
      Unknown  (* redundant given clause *)
    | c::new_clauses ->
      let new_clauses = Vector.from_list new_clauses in
      (* select first clause, the other ones are passive *) 
      assert (not (Env.is_redundant ~env c));
      (* process the given clause! *)
      Util.incr_stat stat_processed_given;
      C.check_ord ~ord c;
      Util.debug 2 "%% ============ step %5d  ============" num;
      Util.debug 1 "%% %a" C.pp_tstp c;
      (* yield control to meta-prover *)
      Vector.append_seq new_clauses (Env.meta_step ~env c);
      (* find clauses that are subsumed by given in active_set *)
      let subsumed_active = Sequence.of_list (Env.subsumed_by ~env c) in
      Env.remove_active ~env subsumed_active;
      Env.remove_simpl ~env subsumed_active;
      Env.remove_orphans ~env subsumed_active; (* orphan criterion *)
      (* add given clause to simpl_set *)
      Env.add_simpl ~env (Sequence.singleton c);
      (* simplify active set using c *)
      let simplified_actives, newly_simplified = Env.backward_simplify ~env c in
      let simplified_actives = C.CSet.to_seq simplified_actives in
      (* the simplified active clauses are removed from active set and
         added to the set of new clauses. Their descendants are also removed
         from passive set *)
      Env.remove_active ~env simplified_actives;
      Env.remove_simpl ~env simplified_actives;
      Env.remove_orphans ~env simplified_actives;
      Vector.append_seq new_clauses newly_simplified;
      (* add given clause to active set *)
      Env.add_active ~env (Sequence.singleton c);
      (* do inferences between c and the active set (including c),
         if [generate] is set to true *)
      let inferred_clauses = if generating
        then Env.generate ~env c
        else Sequence.empty in
      (* simplification of inferred clauses w.r.t active set; only the non-trivial ones
         are kept (by list-simplify) *)
      let inferred_clauses = Sequence.flatMap
        (fun c ->
          let cs = Env.forward_simplify ~env c in
          (* keep clauses  that are not redundant *)
          let cs = Sequence.filter (fun c -> not (Env.is_trivial ~env c)) cs in
          cs)
        inferred_clauses
      in
      Vector.append_seq new_clauses inferred_clauses;
      (if Util.get_debug () >= 2
        then Vector.iter new_clauses
          (fun new_c -> Util.debug 2 "    inferred new clause %a" C.pp_debug new_c));
      (* add new clauses (including simplified active clauses) to passive set and simpl_set *)
      Env.add_passive ~env (Vector.to_seq new_clauses);
      (* test whether the empty clause has been found *)
      match Env.get_some_empty_clause ~env with
      | None -> Unknown
      | Some empty_clause -> Unsat empty_clause

(** print progress *)
let print_progress ~env steps =
  let num_active, num_passive, num_simpl = Env.stats ~env in
  Printf.printf "\r%% %d steps; %d active; %d passive; %d simpl; time %.1f s"
    steps num_active num_passive num_simpl (Util.get_total_time ());
  flush stdout;
  ()

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
            Util.debug 1 "%% perform cleanup of passive set";
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

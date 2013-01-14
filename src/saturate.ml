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
module Sup = Superposition
module Sel = Selection
module Utils = FoUtils
module Delayed = Delayed

let enable = true
let prof_generate = Utils.mk_profiler "generate"
let prof_generate_unary = Utils.mk_profiler "generate_unary"
let prof_generate_binary = Utils.mk_profiler "generate_binary"
let prof_back_simplify = Utils.mk_profiler "back_simplify"
let prof_simplify = Utils.mk_profiler "simplify"
let prof_all_simplify = Utils.mk_profiler "all_simplify"
let prof_is_redundant = Utils.mk_profiler "is_redundant"
let prof_subsumed_by = Utils.mk_profiler "subsumed_by"

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

(** simplify the hclause using the active_set. Returns both the
    hclause and the simplified hclause. *)
let simplify ~calculus ~select active_set simpl_set old_hc =
  Utils.enter_prof prof_simplify;
  let ord = active_set#ord in
  (* simplify with unit clauses, then all active clauses *)
  let hc = calculus#rw_simplify ~select simpl_set old_hc in
  let hc = calculus#basic_simplify ~ord hc in
  let hc = calculus#active_simplify ~select active_set hc in
  let hc = C.select_clause ~select hc in
  (if not (C.eq_hclause hc old_hc)
    then Utils.debug 2 (lazy (Utils.sprintf "@[<hov 4>clause @[<h>%a@]@ simplified into @[<h>%a@]@]"
                        !C.pp_clause#pp_h old_hc !C.pp_clause#pp_h hc)));
  Utils.exit_prof prof_simplify;
  old_hc, hc

(** Perform backward simplification with the given clause *)
let backward_simplify ~calculus ~select (active_set : ProofState.active_set) simpl_set given =
  Utils.enter_prof prof_back_simplify;
  let before, after = Calculus.backward_simplify ~calculus ~select active_set simpl_set given in
  let after = List.map (C.select_clause ~select) after in
  Utils.exit_prof prof_back_simplify;
  before, after

(** generate all clauses from binary inferences *)
let generate_binary ~calculus active_set clause =
  Utils.enter_prof prof_generate_binary;
  let new_clauses = Calculus.do_binary_inferences active_set calculus#binary_rules clause in
  Utils.exit_prof prof_generate_binary;
  new_clauses

(** generate all clauses from unary inferences *)
let generate_unary ~calculus ~ord clause =
  Utils.enter_prof prof_generate_unary;
  let new_clauses = Calculus.do_unary_inferences ~ord calculus#unary_rules clause in
  Utils.exit_prof prof_generate_unary;
  new_clauses

(** depth at which unary inferences are performed (max number
    of times inferences are applied recursively to a clause) *)
let unary_max_depth = ref 2

(** generate all clauses from inferences *)
let generate ~calculus active_set given =
  Utils.enter_prof prof_generate;
  let ord = active_set#ord in
  (* binary clauses *)
  let binary_clauses = generate_binary ~calculus active_set given in
  (* unary inferences *)
  let unary_clauses = ref []
  and unary_queue = Queue.create () in
  Queue.push (given, 0) unary_queue;
  while not (Queue.is_empty unary_queue) do
    let hc, depth = Queue.pop unary_queue in
    let hc = calculus#basic_simplify ~ord hc in (* simplify a bit the clause *)
    if not (Sup.is_tautology hc) then begin
      (* add the clause to set of inferred clauses, if it's not the original clause *)
      (if depth > 0 then unary_clauses := hc :: !unary_clauses);
      if depth < !unary_max_depth
        then begin
          (* infer clauses from c, add them to the queue *)
          let new_clauses = generate_unary ~calculus ~ord hc in
          List.iter (fun hc' -> Queue.push (hc', depth+1) unary_queue) new_clauses
        end
    end
  done;
  let new_clauses =  List.rev_append !unary_clauses binary_clauses in
  Utils.exit_prof prof_generate;
  new_clauses

(** remove direct descendants of the clauses from the passive set *)
let remove_orphans passive_set removed_clauses =
  List.iter
    (fun removed_clause -> passive_set#remove removed_clause.hcdescendants)
    removed_clauses

(** check whether the clause is redundant w.r.t the active_set *)
let is_redundant ~calculus active_set hc =
  Utils.enter_prof prof_is_redundant;
  let res = calculus#redundant active_set hc in
  Utils.exit_prof prof_is_redundant;
  res

(** find redundant clauses in active_set, w.r.t clause c *)
let subsumed_by ~calculus active_set hc =
  Utils.enter_prof prof_subsumed_by;
  let res = calculus#backward_redundant active_set hc in
  Utils.exit_prof prof_subsumed_by;
  res

(** Use all simplification rules to convert a clause into a list of maximally
    simplified clauses (possibly empty, if trivial). *)
let all_simplify ~ord ~calculus ~select active_set simpl_set hc =
  Utils.enter_prof prof_all_simplify;
  let clauses = calculus#list_simplify ~ord ~select hc in
  let clauses = List.map
    (fun hc ->
      (* simplify this clause *)
      let _, hc' = simplify ~calculus ~select active_set simpl_set hc in
      let hc' = C.select_clause ~select hc' in
      hc')
    clauses
  in
  Utils.exit_prof prof_all_simplify;
  clauses

(** Find the lemmas that can be deduced if we consider this new clause *)
let find_lemmas state hc = 
  match state#meta_prover with
  | None -> []  (* lemmas detection is disabled *)
  | Some meta ->
    let lemmas = Theories.scan_clause meta hc in
    lemmas

(** One iteration of the main loop ("given clause loop") *)
let given_clause_step ?(generating=true) ~(calculus : Calculus.calculus) num state =
  let ord = state#ord
  and select = state#select in
  (* select next given clause *)
  match state#passive_set#next () with
  | None -> Sat (* passive set is empty *)
  | Some hc ->
    (* simplify given clause w.r.t. active set, then remove redundant clauses *)
    let c_list = all_simplify ~ord ~calculus ~select state#active_set state#simpl_set hc in
    let c_list = List.filter
      (fun hc' -> not (is_redundant ~calculus state#active_set hc'))
      c_list in
    match c_list with
    | [] -> Unknown  (* all simplifications are redundant *)
    | hc::new_clauses ->     (* select first clause, the other ones are passive *) 
    (* empty clause found *)
    if hc.hclits = [||]
    then (state#active_set#add [hc]; Unsat hc)
    else begin
      assert (not (is_redundant ~calculus state#active_set hc));
      (* forward contextual literal cutting *)
      let hc = Sup.contextual_literal_cutting state#active_set hc in
      (* select literals *)
      let hc = C.select_clause select hc in
      Sel.check_selected hc;
      C.check_ord_hclause ~ord hc;
      Utils.debug 1 (lazy (Utils.sprintf
                    "%% ============ step %5d with given clause @[<h>%a@] ============"
                    num !C.pp_clause#pp_h hc));
      (* scan clause within meta-prover *)
      let lemmas = find_lemmas state hc in
      Utils.debug 1 (lazy (Utils.sprintf "%% found %d lemmas" (List.length lemmas)));
      let new_clauses = List.rev_append lemmas new_clauses in
      (* find clauses that are subsumed by given in active_set *)
      let subsumed_active = subsumed_by ~calculus state#active_set hc in
      state#active_set#remove subsumed_active;
      state#simpl_set#remove subsumed_active;
      remove_orphans state#passive_set subsumed_active; (* orphan criterion *)
      (* add given clause to simpl_set *)
      state#simpl_set#add [hc];
      (* simplify active set using c *)
      let simplified_actives, newly_simplified =
        backward_simplify ~calculus ~select state#active_set state#simpl_set hc in
      let simplified_actives = C.CSet.to_list simplified_actives in
      (* the simplified active clauses are removed from active set and
         added to the set of new clauses. Their descendants are also removed
         from passive set *)
      state#active_set#remove simplified_actives;
      state#simpl_set#remove simplified_actives;
      remove_orphans state#passive_set simplified_actives;
      let new_clauses = List.rev_append newly_simplified new_clauses in
      (* add given clause to active set *)
      state#active_set#add [hc];
      (* do inferences between c and the active set (including c), if [generate] is set to true *)
      let inferred_clauses = if generating then generate ~calculus state#active_set hc else [] in
      (* TODO find lemmas, by matching given clause with pattern clauses *)
      let pclause = Patterns.pclause_of_clause hc in
      Utils.debug 1 (lazy (Utils.sprintf "given clause pattern is @[<h>%a@] (elegance %.2f)"
                   Patterns.pp_pclause pclause (Theories.rate_clause pclause)));
      (* simplification of inferred clauses w.r.t active set; only the non-trivial ones
         are kept (by list-simplify) *)
      let inferred_clauses = List.fold_left
        (fun acc hc ->
          let cs = calculus#list_simplify ~ord ~select hc in
          let cs = List.map (calculus#rw_simplify ~select state#simpl_set) cs in
          List.rev_append cs acc)
        [] inferred_clauses
      in
      let new_clauses = List.rev_append inferred_clauses new_clauses in
      List.iter (fun new_c -> Utils.debug 2 (lazy (Utils.sprintf
                "    inferred new clause @[<hov 3>%a@]" !C.pp_clause#pp_h new_c)))
        new_clauses;
      (* add new clauses (including simplified active clauses) to passive set and simpl_set *)
      state#passive_set#add new_clauses;
      (* test whether the empty clause has been found *)
      try
        let empty_clause = List.find (fun hc -> hc.hclits = [||]) new_clauses in
        (state#active_set#add [empty_clause]; Unsat empty_clause)
      with Not_found ->
      (* empty clause not found, return unknown *)
      Unknown
    end

(** Time elapsed since initialization of the program, and time of start *)
let get_total_time, get_start_time =
  let start = Unix.gettimeofday () in
  (function () ->
    let stop = Unix.gettimeofday () in
    stop -. start),
  (function () -> start)

(** print progress *)
let print_progress steps state =
  let num_active, num_passive = PS.stats state in
  Format.printf "\r%% %d steps; %d active; %d passive; time %.1f s@?"
    steps num_active num_passive (get_total_time ())

let given_clause ?(generating=true) ?steps ?timeout ?(progress=false) ~calculus state =
  let rec do_step num =
    if check_timeout timeout then Timeout, num else
    match steps with
    | Some i when num >= i -> Unknown, num
    | _ ->
      begin
        (* print progress *)
        (if progress && (num mod 10) = 0 then print_progress num state);
        (* some cleanup from time to time *)
        (if (num mod 500 = 0)
          then (
            Utils.debug 1 (Lazy.lazy_from_val "% perform cleanup of passive set");
            state#passive_set#clean ();
            Gc.full_major ()
          ));
        (* do one step *)
        let status = given_clause_step ~generating ~calculus num state in
        match status with
        | Sat | Unsat _ | Error _ -> status, num (* finished *)
        | Timeout -> assert false
        | Unknown -> do_step (num+1)
      end
  in
  do_step 0

(** Simplifications to perform on initial clauses *)
let presaturate ~calculus state =
  given_clause ~generating:false ~progress:false ~calculus state

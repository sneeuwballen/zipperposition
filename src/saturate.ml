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
let prof_generate = HExtlib.profile ~enable "generate"
let prof_generate_unary = HExtlib.profile ~enable "generate_unary"
let prof_generate_binary = HExtlib.profile ~enable "generate_binary"
let prof_back_simplify = HExtlib.profile ~enable "back_simplify"
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
let simplify_ ~calculus ~select active_set simpl_set old_hc =
  let ord = active_set#ord in
  let hc = calculus#simplify ~select active_set simpl_set old_hc in
  let hc = calculus#basic_simplify ~ord hc in
  let hc = C.select_clause ~select hc in
  (if not (C.eq_hclause hc old_hc)
    then Utils.debug 2 (lazy (Utils.sprintf "@[<hov 4>clause @[<h>%a@]@ simplified into @[<h>%a@]@]"
                        !C.pp_clause#pp_h old_hc !C.pp_clause#pp_h hc)));
  old_hc, hc

let simplify ~calculus ~select active_set simpl_set hc =
  prof_simplify.HExtlib.profile (simplify_ ~calculus ~select active_set simpl_set) hc

(** Simplify the active set using the given clause. Simplified clauses are
    removed from the sets, and the function returns
    list of removed clauses, list of those clauses after simplification *)
let backward_simplify_ ~calculus ~select (active_set : ProofState.active_set) simpl_set given =
  let given = active_set#relocate given in
  (* clauses that can be simplified using given *)
  let simplified_actives = ref C.CSet.empty
  and not_simplifiable = ref C.CSet.empty
  and newly_simplified = ref [] in
  (* simplify clauses that have a subterm matching t. If a clause
     can be simplified, it is added to simplified_actives and its
     simplification is added to newly_simplified *)
  let gather t =
    active_set#idx_back_demod#retrieve_specializations t ()
      (fun () t' clauses ->
        try
          let _ = FoUnif.matching FoSubst.id_subst t t' in
          Index.ClauseSet.iter
            (fun (hc,_,_) ->
              if C.CSet.mem !simplified_actives hc then ()  (* already simplified *)
              else if C.CSet.mem !not_simplifiable hc then () (* could not simplify *)
              else (* try to simplify the clause *)
                let _, simplified = simplify ~calculus ~select active_set simpl_set hc in
                if C.eq_hclause hc simplified
                  then not_simplifiable := C.CSet.add !not_simplifiable hc
                  else (
                    Utils.debug 2 (lazy (Utils.sprintf
                                 "@[<hov 4>active clause @[<h>%a@]@ simplified into @[<h>%a@]@]"
                                 !C.pp_clause#pp_h hc !C.pp_clause#pp_h simplified));
                    simplified_actives := C.CSet.add !simplified_actives hc;
                    newly_simplified := simplified :: !newly_simplified))
            clauses
        with UnificationFailure -> ())
    in
    (match given.clits with
    | [|Equation (l,r,true,Gt)|] -> gather l
    | [|Equation (l,r,true,Lt)|] -> gather r
    | [|Equation (l,r,true,_)|] -> gather l; gather r
    | [|Equation (l,r,false,_)|] -> gather l; gather r
    | _ -> ());  (* no simplification with non-unit clauses (no backward clc) *)
    (* list of clauses before simplification, list of clauses after *)
    C.CSet.to_list !simplified_actives, !newly_simplified

let backward_simplify ~calculus ~select active_set simpl_set given =
  prof_back_simplify.HExtlib.profile (backward_simplify_ ~calculus ~select active_set simpl_set) given

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
  let ord = state#ord in
  (* binary clauses *)
  let binary_clauses = generate_binary ~calculus state#active_set given in
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
  new_clauses

let generate ~calculus state hc =
  prof_generate.HExtlib.profile (generate_ ~calculus state) hc

(** remove direct descendants of the clauses from the passive set *)
let remove_orphans state removed_clauses =
  List.iter
    (fun removed_clause -> state#passive_set#remove removed_clause.hcdescendants)
    removed_clauses

(** check whether the clause is redundant w.r.t the active_set *)
let is_redundant ~calculus active_set hc =
  prof_is_redundant.HExtlib.profile (calculus#redundant active_set) hc

(** find redundant clauses in active_set, w.r.t clause c *)
let subsumed_by ~calculus active_set hc =
  prof_subsumed_by.HExtlib.profile (calculus#redundant_set active_set) hc

(** Use all simplification rules to convert a clause into a list of maximally
    simplified clauses (possibly empty, if trivial).
    This is used on generated clauses, and on the given clause. *)
let all_simplify_ ~ord ~calculus ~select active_set simpl_set hc =
  let clauses = ref []
  and queue = Queue.create () in
  Queue.push hc queue;
  while not (Queue.is_empty queue) do
    let hc = Queue.pop queue in
    (* usual simplifications *)
    let hc = C.select_clause ~select hc in
    let _, hc = simplify ~calculus ~select active_set simpl_set hc in
    let hc = C.select_clause ~select hc in
    (* do not keep tautologies *)
    if Sup.is_tautology hc then () else
    (* list simplification *)
    match calculus#list_simplify ~ord ~select hc with
    | None -> clauses := hc :: !clauses (* totally simplified clause *)
    | Some clauses ->
      List.iter (fun hc' -> Queue.push hc' queue) clauses (* process new clauses *)
  done;
  !clauses

let all_simplify ~ord ~calculus ~select active_set simpl_set clause =
  prof_all_simplify.HExtlib.profile (all_simplify_ ~ord ~calculus ~select active_set simpl_set) clause

(** Simplifications to perform on initial clauses *)
let initial_simplifications ~ord ~select ~calculus active_set simpl_set clauses =
  List.fold_left
    (fun acc c ->
      let clauses = all_simplify ~ord ~calculus ~select active_set simpl_set c in
      let clauses = List.filter (fun hc -> not (Sup.is_tautology hc)) clauses in
      List.rev_append clauses acc)
    [] clauses

let given_clause_step ~calculus state =
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
      ignore (Lazy.force hc.hcmaxlits);
      Utils.debug 1 (lazy (Utils.sprintf
                    "============ step with given clause @[<h>%a@] =========="
                    !C.pp_clause#pp_h hc));
      (* find clauses that are subsumed by given in active_set *)
      let subsumed_active = subsumed_by ~calculus state#active_set hc in
      state#active_set#remove subsumed_active;
      state#simpl_set#remove subsumed_active;
      remove_orphans state subsumed_active; (* orphan criterion *)
      (* add given clause to simpl_set *)
      state#simpl_set#add [hc];
      (* simplify active set using c *)
      let simplified_actives, newly_simplified =
        backward_simplify ~calculus ~select state#active_set state#simpl_set hc in
      (* the simplified active clauses are removed from active set and
         added to the set of new clauses. Their descendants are also removed
         from passive set *)
      state#active_set#remove simplified_actives;
      state#simpl_set#remove simplified_actives;
      remove_orphans state simplified_actives;
      let new_clauses = List.rev_append simplified_actives new_clauses in
      (* add given clause to active set *)
      state#active_set#add [hc];
      (* do inferences between c and the active set (including c) *)
      let inferred_clauses = generate ~calculus state hc in
      (* simplification of inferred clauses w.r.t active set; only the non-trivial ones
         are kept (by list-simplify) *)
      let inferred_clauses = List.fold_left
        (fun acc hc ->
          let cs = all_simplify ~ord ~select ~calculus state#active_set state#simpl_set hc in
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
  Format.printf "\r%% %d steps; %d active; %d passive; time %.1f s"
    steps num_active num_passive (get_total_time ());
  Format.print_flush ()

let given_clause ?steps ?timeout ?(progress=false) ~calculus state =
  let rec do_step num =
    if check_timeout timeout then Timeout, num else
    begin
    Utils.debug 1 (lazy (Format.sprintf "# iteration %d" num));
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
            Gc.major ();
            state#passive_set#clean ()));
        (* do one step *)
        let status = given_clause_step ~calculus state in
        match status with
        | Sat | Unsat _ | Error _ -> status, num (* finished *)
        | Timeout -> assert false
        | Unknown -> do_step (num+1)
      end
    end
  in
  do_step 0

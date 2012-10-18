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

open Types

module T = Terms
module C = Clauses
module S = FoSubst
module Utils = FoUtils

(** Management of opaque terms, that are terms modulo some congruence *)

(** A theory that generates finite congruences on terms *)
type theory =
  < congruence : term -> term list;
      (** Compute the finite congruence of t *)
    ordering : ordering;
      (** Ordering on terms *)
  >

(** The representation of opaque terms *)

type oterm = {
  ot_theory : theory;
  ot_vars : T.varlist;
  ot_terms : term list;
  mutable ot_minimal_terms : term list;
  mutable ot_points_to : oterm list;
  mutable ot_up_to_date : bool;         (** indicates whether minimal_terms is up-to-date *)
} (** opaque term *)

type oclause = {
  oc_theory : theory;
  oc_vars : T.varlist;
  oc_clauses : clause list;
  mutable oc_active_clauses : clause list;
  mutable oc_up_to_date : bool;         (** indicates whether active_clauses is up-to-date *)
} (** opaque clause *)

let oterms = T.THashtbl.create 17
  (* stores the mapping term -> oterm *)

let oclauses = C.CHashtbl.create 17
  (* store the mapping clause -> oclause *)

let mk_oterm ~theory t =
  try
    T.THashtbl.find oterms t
  with Not_found -> begin
    let vars = T.vars_of_term t in
    let congruence = theory#congruence t in
    let ot = {
      ot_vars = vars;
      ot_terms = congruence;
      ot_minimal_terms = congruence;  (* will be computed on demand *)
      ot_points_to = [];
      ot_up_to_date = false;
    } in
    (* associate opaque term with all terms of the congruence *)
    List.iter
      (fun t -> Hashtbl.replace oterms t ot)
      congruence;
    ot
  end

let mk_oclause ~theory clause = failwith "not implemented"

let rewrite ot1 ot2 =
  if ot1 == ot2 then () else begin
    assert (Utils.list_subset ot2.ot_vars ot1.ot_vars);
    if Utils.list_mem (fun x y -> x == y) ot2 ot1.ot_points_to
      then ()
      else (ot1.ot_points_to <- ot2 :: ot1.ot_points_to;
            ot1.ot_up_to_date <- false)  (* we will have to recompute minimal terms *)
  end

let minimal_terms ot =
  (* recurse in pointed opaque terms, updating the ones
     that need it *)
  let rec update has_changed ot =
    let has_changed' = ref false in
    (* first update pointed opaque terms *)
    List.iter (update has_changed') ot.ot_points_to;
    (* now if some change occurred, or ot is not up-to-date, recompute *)
    if (not !has_changed') && ot.ot_up_to_date
      then ()
      else begin
        has_changed := true;   (* notify that something changed *)
        let all_terms = gather_minimal_terms [] ot in
        let minimal = Utils.list_min ot.ot_theory#ordering#compare all_terms in
        ot.ot_minimal_terms <- minimal
      end
  (* gather all current minimal terms *)
  and gather_minimal_terms acc ot =
    let acc = List.rev_append ot.ot_minimal_terms acc in
    List.fold_left gather_minimal_terms acc ot.ot_points_to
  (* update; then return *)
  update (ref false) ot;
  ot.ot_minimal_terms
  and

let active_clauses oc =
  (* TODO update *)
  oc.oc_active_clauses

let vars ot = ot.ot_vars

let oclause_vars oc = oc.oc_vars

(* ot1 and ot2 are congruent if there is ot3 such that
   ot1 ->* ot3 and ot2 ->* ot3 *)
let congruent ot1 ot2 =
  (* is this a root opaque term? (points to no other term) *)
  let rec is_root ot = ot.ot_points_to = []
  (* iterate on roots of ot1 *)
  and check_left_roots ot1 =
    if is_root ot1
      then check_right_roots ot1 ot2
      else List.exists check_left_roots ot1.ot_points_to
  (* iterate on roots of ot2, comparing with the left root *)
  and check_right_roots left_root ot2 =
    if is_root ot2
      then ot1 == ot2
      else List.exists (check_right_roots left_root) ot2.ot_points_to
  in
  if ot1 == ot2 then true else check_left_roots ot1

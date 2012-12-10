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

(** The state of a proof, contains a set of active clauses (processed),
    a set of passive clauses (to be processed), and an ordering
    that is used for redundancy elimination. *)

open Types

(** (empty) index to use for active sets *)
val cur_index : Index.clause_index ref
(** association name -> index *)
val choose_index : string -> Index.index
(** list of index names *)
val names_index : unit -> string list

(** set of active clauses *)
type active_set = {
  a_ord : ordering;
  active_clauses : Clauses.CSet.t;    (** set of active clauses *)
  idx : Index.clause_index;           (** term index *)
  fv_idx : FeatureVector.fv_index;    (** feature index, for subsumption *)
}

(** set of passive clauses *)
type passive_set = {
  p_ord : ordering;
  passive_clauses : Clauses.CSet.t;
  queues : (ClauseQueue.queue * int) list;
  queue_state : int * int;  (** position in the queue/weight *)
}

(** state of a superposition calculus instance.
    It contains a set of active clauses, a set of passive clauses,
    and is parametrized by an ordering. *)
type state = {
  ord : ordering;
  state_select : selection_fun;
  state_index : Index.unit_index; (** index used for unit simplification *)
  active_set : active_set;        (** active clauses, indexed *)
  passive_set : passive_set;      (** passive clauses *)
}

(** create a state from the given ordering and selection function*)
val mk_state : ord:ordering -> Params.parameters -> state

(** add (unit) clause to the index *)
val add_rule : state -> hclause -> state
val add_rules : state -> hclause list -> state
val remove_rule : state -> hclause -> state
val remove_rules : state -> hclause list -> state

(** add clauses to the active set *)
val add_active : active_set -> hclause -> active_set
val add_actives : active_set -> hclause list -> active_set

(** remove clause from the active set *)
val remove_active : active_set -> hclause -> active_set
val remove_actives : active_set -> hclause list -> active_set
val remove_active_set : active_set -> Clauses.CSet.t -> active_set

(** get a clause that shares no variable with the active set *)
val relocate_active : active_set -> hclause -> clause
(** get a clause that shares no variable with the unit index *)
val relocate_rules : ord:ordering -> Index.unit_index -> hclause -> clause

(** create an active_set that contains one clause *)
val singleton_active_set : ord:ordering -> hclause -> active_set

(** add clauses to the passive set *)
val add_passive : passive_set -> hclause -> passive_set
val add_passives : passive_set -> hclause list -> passive_set
(** remove clause from passive set *)
val remove_passive : passive_set -> hclause -> passive_set
val remove_passives : passive_set -> hclause list -> passive_set
val remove_passives_set : passive_set -> Ptset.t -> passive_set
(** cleanup memory in passive set *)
val clean_passive : passive_set -> passive_set

(** pop the next passive clause, if any *)
val next_passive_clause : passive_set -> (passive_set * hclause option)

(** maximum variable index in the set *)
val maxvar_active : active_set -> int

(** statistics on the state (num active, num passive) *)
type state_stats = int * int
val stats : state -> state_stats

(** pretty print the content of the state *)
val pp_state : Format.formatter -> state -> unit
(** debug functions: much more detailed printing *)
val debug_state : Format.formatter -> state -> unit

(** print to dot (if empty clause is present, only print a proof,
    otherwise print the active set and its proof) *)
val pp_dot : ?name:string -> Format.formatter -> state -> unit
(** print to dot into a file *)
val pp_dot_file : ?name:string -> string -> state -> unit

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

(** set of active clauses *)
type active_set = {
  a_ord : ordering;
  active_clauses : Clauses.bag;       (** set of active clauses *)
  idx : Index.clause_index;           (** term index *)
  fv_idx : FeatureVector.fv_index;    (** feature index, for subsumption *)
}

(** set of passive clauses *)
type passive_set = {
  p_ord : ordering;
  passive_clauses : Clauses.bag;
  queues : (ClauseQueue.queue * int) list;
  queue_state : int * int;  (** position in the queue/weight *)
}

(** state of a superposition calculus instance.
    It contains a set of active clauses, a set of passive clauses,
    and is parametrized by an ordering. *)
type state = {
  ord : ordering;
  state_select : selection_fun;
  active_set : active_set;      (** active clauses, indexed *)
  axioms_set : active_set;      (** set of support, indexed *)
  passive_set : passive_set;    (** passive clauses *)
}

(** create a state from the given ordering and selection function*)
val make_state : ordering -> (ClauseQueue.queue * int) list -> selection_fun -> state

(** add clauses to the active set *)
val add_active : active_set -> clause -> active_set * hclause
val add_actives : active_set -> clause list -> active_set

(** remove clause from the active set *)
val remove_active : active_set -> hclause -> active_set
val remove_actives : active_set -> hclause list -> active_set
val remove_active_bag : active_set -> Clauses.bag -> active_set

(** create an active_set that contains one clause *)
val singleton_active_set : ord:ordering -> clause -> active_set

(** add clauses to the passive set *)
val add_passive : passive_set -> clause -> passive_set * hclause
val add_passives : passive_set -> clause list -> passive_set

(** pop the next passive clause, if any *)
val next_passive_clause : passive_set -> (passive_set * hclause option)

(** relocate clause w.r.t clauses in the active_set *)
val relocate_active : active_set -> clause -> clause

(** statistics on the state *)
type state_stats = {
  stats_active_clauses : int;
  stats_sos_clauses: int;
  stats_passive_clauses : int;
}
val stats : state -> state_stats

(** pretty print the content of the state *)
val pp_state : Format.formatter -> state -> unit
(** debug functions: much more detailed printing *)
val debug_state : Format.formatter -> state -> unit

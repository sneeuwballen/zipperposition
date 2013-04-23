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

(** Heuristic selection of clauses, using queues. Note that some
    queues do not need accept all clauses, as long as one of them does
    (for completeness). Anyway, a fifo queue should always be present,
    and presents this property. *)

open Basic

(** A priority queue of clauses, purely functional *)
type t

val add : t -> hclause -> t
  (** Add a clause to the Queue *)

val adds : t -> hclause Sequence.t -> t
  (** Add clauses to the queue *)

val is_empty : t -> bool
  (** check whether the queue is empty *)

val take_first : t -> (t * hclause)
  (** Take first element of the queue, or raise Not_found *)

val clean : t -> Clauses.CSet.t -> t
  (** remove all clauses that are not in the set *)

val name : t -> string
  (** Name of the implementation/role of the queue *)

val fifo : t
  (** select by increasing age (for fairness) *)

val clause_weight : t
  (** select by increasing weight of clause *)

val goals : t
  (** only select goals (clauses with only negative lits) *)

val ground : t
  (** only select ground clauses *)

val pos_unit_clauses : t
  (** only select positive unit clauses *)

val horn : t
  (** select horn clauses *)

val lemmas : t
  (** only select lemmas *)

val mk_queue : ?accept:(hclause -> bool) -> weight:(hclause -> int) -> string -> t
  (** Bring your own implementation of queue *)

(** default combination of heuristics *)
val default_queues : (t * int) list  (* TODO array *)

val pp_queue : Format.formatter -> t -> unit
val pp_queues : Format.formatter -> (t * int) Sequence.t -> unit


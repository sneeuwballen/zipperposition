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

(* heuristic selection of clauses *)

open Types

(* a queue of clauses, as a pure object *)
class type queue =
  object
    method add : hclause -> queue
    method is_empty: bool
    method take_first : (queue * hclause)
    method remove : hclause list -> queue  (* slow *)
    method name : string
  end

(* select by increasing age (for fairness) *)
val fifo : ord:ordering -> queue

(* select by increasing weight of clause *)
val clause_weight : ord:ordering -> queue

(* default combination of heuristics *)
val default_queues : ord:ordering -> (queue * int) list

val pp_queue : Format.formatter -> queue -> unit
val pp_queues : Format.formatter -> (queue * int) list -> unit


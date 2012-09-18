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

(** E-graph imperative data structure *)

open Types

type egraph_node
type egraph

val empty: unit -> egraph                               (** get a new, empty E-graph *)
val push: egraph -> unit                                (** push a backtracking point *)
val pop: egraph -> unit                                 (** pop last backtracking point, or Invalid_argument *)

val term_of_node: egraph_node -> foterm                 (** get the term back from the node *)
val node_of_term: egraph -> foterm -> egraph_node       (** get the node representing the term *)

val are_equal: egraph_node -> egraph_node -> bool       (** test whether nodes are equal *)
val equiv_class: egraph_node -> egraph_node list        (** equivalence class of the node *)
val representative: egraph_node -> egraph_node          (** representative of the node *)
val merge: egraph -> egraph_node -> egraph_node -> unit (** merge two nodes in the E-graph (and propagate) *)

val to_dot: name:string -> egraph -> string

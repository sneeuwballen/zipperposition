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

(** Term indexing, using non-perfect discrimination trees with no jumplist *)

open Types

type path_string_elem = 
  | Constant of symbol * int (* name, arity *)
  | Bound of int * int (* rel, arity *)
  | Variable (* arity is 0 *)
  | Proposition (* arity is 0 *) 
  | Datatype (* arity is 0 *) 
  | Dead (* arity is 0 *) 

type path = path_string_elem list

module ClauseSet : Set.S with type elt = hclause * position * foterm

type input = foterm                         (** indexed type *)
type data = ClauseSet.elt                   (** value associated with input *)
type dataset = ClauseSet.t                  (** set of values *)
type constant_name = symbol                 (** constant terms (leaves) *)
type tree                                   (** the tree itself *)

val iter : tree -> (path -> dataset -> unit) -> unit
val fold : tree -> (path -> dataset -> 'b -> 'b) -> 'b -> 'b

val empty : tree
val index : tree -> input -> data -> tree
val remove_index : tree -> input -> data -> tree
val in_index : tree -> input -> (data -> bool) -> bool
val num_keys : tree -> int                  (** number of indexed keys (paths) *)
val num_elems : tree -> int                 (** number of elements for any key *)

val index : Index.index                     (** the index *)
val clause_index : Index.clause_index       (** the clause index *)

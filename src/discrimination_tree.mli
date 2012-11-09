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

val path_string_of : term -> path
val string_of_path : path -> string
val arity_of : path_string_elem -> int 
val skip : int -> path -> path

module PSMap : Trie.Map with type key = path_string_elem
module DiscriminationTree : Trie.S with module M = PSMap

val index : Index.index                     (** the index *)

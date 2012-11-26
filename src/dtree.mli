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

(** Efficient perfect discrimination trees for matching *)

type 'a dtree

val empty : ('a -> 'a -> bool) -> 'a dtree
  (** empty discrimination tree (with a comparison function) *)

val add : 'a dtree -> ?priority:int -> term -> 'a -> 'a dtree
  (** add a term and a value to the discrimination tree. The priority
      is used to sort index values (by increasing number, the lowest
      are iterated on the first). *)

val remove : 'a dtree -> term -> 'a -> 'a dtree
  (** remove the term -> value from the tree *)

val min_var : 'a dtree -> int
  (** maximum variable in the tree (surapproximation) *)

val max_var : 'a dtree -> int
  (** minimum variable in the tree (surapproximation) *)

val iter_match : 'a dtree -> term -> (term -> 'a -> substitution -> unit) -> unit
  (** iterate on all (term -> value) such that subst(term) = input_term *)

val iter : 'a dtree -> (term -> 'a -> unit) -> unit
  (** iterate on all (term -> value) in the tree *)

val pp_term_hclause_tree : Format.formatter -> (term * hclause) dtree -> unit
  (** pretty print a tree of terms and clauses in ascii *)
val pp_term_tree : Format.formatter -> term dtree -> unit
  (** pretty print a tree of terms in ascii *)

val unit_index : Index.unit_index
  (** Indexing structure for simplifications *)

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

(** Term indexing, using discrimination trees *)

open Types

(* a set of (clause, position in clause). A position is a
 * list [lit index, 1|2 (left or right), ...]
 * where ... is the path in the term *)
module ClauseSet : Set.S with
  type elt = hclause * position * foterm

(* make terms indexable by discrimination_tree *)
module FotermIndexable : Discrimination_tree.Indexable with
  type constant_name = symbol and
  type input = foterm

module DT : Discrimination_tree.DiscriminationTree with
  type constant_name = symbol and
  type input = foterm and
  type data = ClauseSet.elt and
  type dataset = ClauseSet.t

type input = DT.input
type data = DT.data
type dataset = DT.dataset

(** the main index type. It contains 3 trees, that are
    used to index all subterms of a clause, maximal sides
    of unit equations, and terms that occur directly under an equation. *)
type t = {
  root_index : DT.t;
  unit_root_index : DT.t;
  subterm_index : DT.t;
}

val empty : t                           (** empty index *)

val index_clause : t -> hclause -> t    (** add the clause to the index *)

val remove_clause : t -> hclause -> t   (** remove the clause from the index *)

val fold :
  DT.t ->
  (symbol Discrimination_tree.path -> ClauseSet.t -> 'a -> 'a)
  -> 'a -> 'a

val elems : DT.t -> ClauseSet.t

val pp_index : ?all_clauses:bool -> Format.formatter -> t -> unit

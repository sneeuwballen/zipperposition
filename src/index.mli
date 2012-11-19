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

(** Generic term indexing *)

open Types

type data = hclause * position * term

(** a set of (hashconsed clause, position in clause, term). *)
module ClauseSet : Set.S with type elt = data

(** a leaf of an index is generally a map of terms to data *)
type index_leaf

val empty_leaf : index_leaf
val add_leaf : index_leaf -> term -> data -> index_leaf
val remove_leaf : index_leaf -> term -> data -> index_leaf
val is_empty_leaf : index_leaf -> bool
val iter_leaf : index_leaf -> (term -> ClauseSet.t -> unit) -> unit
val fold_leaf : index_leaf -> ('a -> term -> ClauseSet.t -> 'a) -> 'a -> 'a
val size_leaf : index_leaf -> int

(** A term index *)
class type index =
  object ('b)
    method name : string
    method add : term -> data -> 'b
    method remove: term -> data -> 'b

    method iter : (term -> ClauseSet.t -> unit) -> unit
    method fold : 'a. ('a -> term -> ClauseSet.t -> 'a) -> 'a -> 'a

    method retrieve_unifiables : 'a. term -> 'a ->
                                 ('a -> term -> ClauseSet.t -> 'a) -> 'a
    method retrieve_generalizations : 'a. term -> 'a ->
                                      ('a -> term -> ClauseSet.t -> 'a) -> 'a
    method retrieve_specializations : 'a. term -> 'a ->
                                      ('a -> term -> ClauseSet.t -> 'a) -> 'a

    method pp : all_clauses:bool -> Format.formatter -> unit -> unit
  end

(** A simplification index *)
class type unit_index = 
  object ('b)
    method name : string
    method add : term -> term -> bool -> hclause -> 'b    (** add (in)equation (with given ID) *)
    method remove : term -> term -> bool -> hclause ->'b  (** remove (in)equation (with given ID) *)
    method retrieve : sign:bool -> term ->
                      (term -> term -> substitution -> hclause -> unit) ->
                      unit                      (** iter on (in)equations of given sign l=r
                                                    where subst(l) = query term *)
    method pp : Format.formatter -> unit -> unit
  end

(** A global index, that operates on hashconsed clauses *)
class type clause_index =
  object ('a)
    method index_clause : hclause -> 'a
    method remove_clause : hclause -> 'a

    method root_index : index
    method unit_root_index : unit_index (** for simplifications that only require matching *)
    method ground_rewrite_index : (term * data) Ptmap.t (** to rewrite ground terms *)
    method subterm_index : index

    method pp : all_clauses:bool -> Format.formatter -> unit -> unit
  end

(** build a clause index from an index and a unit index *)
val mk_clause_index : index -> unit_index -> clause_index


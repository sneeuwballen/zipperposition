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

type data = hclause * position * foterm

(** A term index *)
class type index =
  object ('b)
    method add : foterm -> data -> 'b
    method remove: foterm -> data -> 'b

    method iter : (data -> unit) -> unit
    method fold : 'a. ('a -> data -> 'a) -> 'a -> 'a

    method retrieve_unifiables : 'a. foterm -> 'a -> ('a -> data -> 'a) -> 'a
    method retrieve_generalizations : 'a. foterm -> 'a -> ('a -> data -> 'a) -> 'a
    method retrieve_specializations : 'a. foterm -> 'a -> ('a -> data -> 'a) -> 'a

    method pp : all_clauses:bool -> Format.formatter -> unit -> unit
  end

(** A global index for clauses *)
class type clause_index =
  object ('a)
    method name : string
    method index_clause : hclause -> 'a
    method remove_clause : hclause -> 'a

    method root_index : index
    method unit_root_index : index
    method subterm_index : index

    method pp : all_clauses:bool -> Format.formatter -> unit -> unit
  end

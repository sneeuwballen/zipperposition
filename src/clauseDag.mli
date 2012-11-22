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

(** DAG of the 'descendant' relation between clauses *)

open Types

(** a DAG of clauses, in which links represent the 'descendant of' relation *)
type clause_dag

(** empty dag *)
val empty : clause_dag

(** [parent_of dag parent child] means that child descends from parent *)
val parent_of : cs:Clauses.clause_state -> clause_dag -> clause -> clause -> clause_dag

(** update the DAG using the list of parents of the clause *)
val update : cs:Clauses.clause_state -> clause_dag -> clause -> clause_dag
val updates : cs:Clauses.clause_state -> clause_dag -> clause list -> clause_dag

(** get the list of descendants of clause. Selected literals are not
    taken into account in hashconsing. *)
val descendants : cs:Clauses.clause_state -> clause_dag -> clause -> hclause list

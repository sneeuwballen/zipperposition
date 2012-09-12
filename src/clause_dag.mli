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

(** [descends_from dag parent child] means that child descends from parent *)
val descends_from : clause_dag -> hclause -> hclause -> clause_dag

(** [simplification_of dag parent child] means that child is a simplification of parent *)
val simplification_of : clause_dag -> hclause -> hclause -> clause_dag

(** get the list of descendants of clause *)
val descendants : clause_dag -> hclause -> hclause list

(** [simplified_to c d] checks whether b is obtained by simplifications on a *)
val simplified_to: clause_dag -> hclause -> hclause -> bool

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

(** Reduction to CNF, and simplifications. See "computing small normal forms",
    in "handbook of automated reasoning". *)


open Types

(** Simplify a boolean term (a formula) *)
val simplify_term : term -> term

(** Simplify the inner formula (double negation, trivial equalities...) *)
val simplify : ord:ordering -> hclause -> hclause

(** Apply miniscoping (push quantifiers as deep as possible in the formula) to the term *)
val miniscope_term : term -> term

(** Apply miniscoping transformation to the clause *)
val miniscope : ord:ordering -> hclause -> hclause

(** Transform the clause into proper CNF; returns a list of clauses *)
val cnf_of : ord:ordering -> hclause -> hclause list

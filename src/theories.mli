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

(** Recognition of theories *)

open Types
open Symbols

val is_RR_horn_clause : clause -> bool
  (** Recognized whether the clause is a Range-Restricted Horn clause *)

val is_definition : clause -> (term * term) option
  (** Check whether the clause defines a symbol, e.g.
      subset(X,Y) = \forall Z(Z in X -> Z in Y). It means the LHS
      is a flat symbol with variables, and all variables in RHS
      are also in LHS *)

val is_rewrite_rule : clause -> (term * term) list
  (** More general than definition. It means the clause is an
      equality where all variables in RHS are also in LHS. It
      can return two rewrite rules if the clause can be oriented
      in both ways, e.g. associativity axiom. *)

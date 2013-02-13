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
open Symbols

(** Partial ordering on terms. This provides several
    simplification orderings (compatible with substitution,
    with the subterm property, and monotonic) *)

val kbo : precedence -> ordering
  (** Knuth-Bendix ordering *)

val rpo : precedence -> ordering
  (** Recursive Path Ordering *)

val rpo6 : precedence -> ordering
  (** Efficient implementation of RPO *)

val choose : string -> precedence -> ordering
  (** Choose ordering by name, or raise Failure *)

val default_ordering : signature -> ordering
  (** default ordering on terms (RPO6) *)

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

(** incremental E-unification *)

open Types

(** an equational theory *)
type e_theory

val empty_theory : e_theory                         (** the empty theory *)
val add_axiom : e_theory -> clause -> e_theory      (** clause must be positive unit *)

(** Abstract type for a lazy set of E-unifiers, It is associated with
    a E-unification problem. *)
type e_unifiers

(** E-unify two terms yields a lazy set of E-unifiers *)
val e_unify : e_theory -> foterm -> foterm -> e_unifiers

(** status of a set of E-unifiers *)
type e_status =
  | ESat of substitution list           (** satisfiable, with the given set of unifiers *)
  | EUnsat                              (** no unifier *)
  | EUnknown of substitution list       (** substitutions already computed *)

(** get the current state *)
val e_state : e_unifiers -> e_status
(** make some progress in the computation of E-unifiers. It returns
    the status of the set, plus the list of new substitutions computed. *)
val e_compute : ?steps:int -> e_unifiers -> e_status * substitution list

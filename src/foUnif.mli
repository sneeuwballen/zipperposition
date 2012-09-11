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

(** Unification and matching algorithms *)

open Types

(* unify terms, returns a substitution or raise an exception *)
val unification: foterm -> foterm -> substitution

(* [matching a b] returns sigma such that sigma(a) = b *)
val matching: foterm -> foterm -> substitution

(** [matching_locked ~locked subst a b] returns sigma such that sigma(a) = b
    and sigma does not bind any variable in locked and subst \in sigma,
    or raise UnificationFailure *)
val matching_locked: locked:varlist -> substitution -> foterm -> foterm -> substitution

(* alpha_eq [a] [b] returns sigma with sigma(a) = b, and sigma
 * a variable renaming, or raise UnificationFailure *)
val alpha_eq: foterm -> foterm -> substitution


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

(** Management of opaque terms, that are terms modulo some congruence *)

(** A theory that generates finite congruences on terms *)
type theory =
  < congruence : term -> term list;
      (** Compute the finite congruence of t *)
    ordering : ordering;
      (** Ordering on terms *)
  >

(** The representation of opaque terms *)

type oterm
  (** An opaque term *)

type oclause
  (** Opaque clause *)

val mk_oterm : theory:theory -> term -> oterm
  (** Build or find the opaque term associated with the term *)

val mk_oclause : theory:theory -> clause -> oclause
  (** Build or find the opaque clause associated with the clause *)

val rewrite : oterm -> oterm -> unit
  (** Inform the first opaque term that it rewrites into the second term
      (the latter must therefore be smaller in the ordering). *)

val minimal_terms : oterm -> term list
  (** List of minimal terms reachable from (and congruent to) opaque term *)

val active_clauses : oclause -> clause list
  (** List of non-redundant clauses that this opaque clause is congruent to *)

val vars : oterm -> varlist
  (** Variables for this opaque term *)

val oclause_vars : oclause -> varlist
  (** Variables for this opaque clause *)

val congruent : oterm -> oterm -> bool
  (** Are those two opaque terms congruent modulo the theory? *)

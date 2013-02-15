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

(** {1 Experts for theories} *)

(** The "experts" are programs that have specific knowledge of some theory,
    and that are able to perform some reasoning over terms that belong to
    this theory. They must be correct, but not necessarily complete, on
    the theory. *)

open Types
open Symbols

(** {2 General interface} *)

type expert
  (** An expert for some theory *)

val expert_compatible : expert -> expert -> bool
  (** Simple syntaxic criterion to decide whether two experts
      are compatibles: check whether they have no symbol in common. *)

val expert_combine : expert -> expert -> expert
  (** Combine two experts into a new one, that works on
      the combination of their theories, assuming they are compatible. *)

val expert_more_specific : expert -> expert -> bool
  (** [expert_more_specific e1 e2] returns true if [e1] decides a theory
      whose symbols are included in the theory of [e2]. Heuristically, that
      means that we can ignore [e1] and focus on [e2] *)

val expert_canonize : expert -> term -> term
  (** Get the normal form of the term *)

val expert_equal : expert -> term -> term -> bool
  (** Check whether the terms are equal modulo theory *)

val expert_sig : expert -> SSet.t
  (** Symbols of the theory associated to the expert *)

val expert_is_redundant : expert -> hclause -> bool
  (** Decide whether this clause is redundant *)

val expert_simplify : ctx:context -> expert -> hclause -> hclause
  (** Simplify the clause *)

val expert_clauses : expert -> hclause list
  (** Get a list of clauses this expert needs to be present in the
      superposition prover (additional axioms). *)

(** {2 Ground joinable sets of equations} *)

(** We use ground convergent sets of equations to decide some equational
    theories. See
    "On using ground joinable equations in equational theorem proving", by
    Avenhaus, Hillenbrand, Lochner *)

type gnd_convergent = {
  gc_ord : string;                    (** name of the ordering *)
  gc_prec : symbol list;              (** Precedence *)
  gc_sig : SSet.t;                    (** Symbols of the theory *)
  gc_equations : literal list;        (** Equations of the system *)
} (** A set of ground convergent equations, for some order+precedence *)

val mk_gc : string -> symbol list -> literal list -> gnd_convergent
  (** Create a ground-convergent system from a list of equations
      and informations on the ordering. *)

val gc_expert : gnd_convergent -> expert
  (** From a set of ground convergent equations, create an expert for
      the associated theory. *)

val pp_gc : Format.formatter -> gnd_convergent -> unit
  (** Pretty-print the system of ground convergent equations *)

(** {3 JSON encoding} *)

val gc_to_json : gnd_convergent -> json
val gc_of_json : ctx:context -> json -> gnd_convergent

(** {2 Some builtin theories} *)

val ac : symbol -> expert
  (** Theory of Associative-Commutative symbols, for the given symbol *)

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

type t
  (** An expert for some theory *)

val compatible_ord : t -> ord:ordering -> bool
  (** Check whether using this expert is possible in the given ordering *)

val update_ctx : t -> ctx:context -> t
  (** Copy of the expert, that uses the new context *)

val compatible : t -> t -> bool
  (** Simple syntaxic criterion to decide whether two experts
      are compatibles: check whether they have no symbol in common. *)

val combine : t -> t -> t
  (** Combine two experts into a new one, that works on
      the combination of their theories, assuming they are compatible. *)

val more_specific : t -> t -> bool
  (** [expert_more_specific e1 e2] returns true if [e1] decides a theory
      whose symbols are included in the theory of [e2]. Heuristically, that
      means that we can ignore [e1] and focus on [e2] *)

val canonize : t -> term -> term
  (** Get the normal form of the term *)

val equal : t -> term -> term -> bool
  (** Check whether the terms are equal modulo theory *)

val signature : t -> SSet.t
  (** Symbols of the theory associated to the expert *)

val is_redundant : t -> hclause -> bool
  (** Decide whether this clause is redundant *)

val simplify : t -> hclause -> hclause
  (** Simplify the clause *)

val clauses : t -> hclause list
  (** Get a list of clauses this expert needs to be present in the
      superposition prover (additional axioms). *)

val pp_expert : Format.formatter -> t -> unit

(** {2 Set of experts} *)

module Set : sig
  type expert = t (* alias *)

  type t
    (** A set of experts *)

  val empty : t

  val add : t -> expert -> t

  val update_ctx : t -> ctx:context -> t

  val is_redundant : t -> hclause -> bool

  val simplify : t -> hclause -> hclause

  val pp : Format.formatter -> t -> unit
end

(** {2 Ground joinable sets of equations} *)

(** We use ground convergent sets of equations to decide some equational
    theories. See
    "On using ground joinable equations in equational theorem proving", by
    Avenhaus, Hillenbrand, Lochner *)

type gnd_convergent = {
  gc_ord : string;              (** name of the ordering *)
  gc_theory : string;           (** Theory that is decided *)
  gc_prec : symbol list;        (** Precedence *)
  gc_sig : SSet.t;              (** Symbols of the theory *)
  gc_eqns : hclause list;       (** Equations of the system *)
} (** A set of ground convergent equations, for some order+precedence *)

val mk_gc : theory:string -> ord:string -> prec:symbol list -> hclause list -> gnd_convergent
  (** Create a ground-convergent system from a list of equations
      and informations on the ordering. *)

val compatible_gc : ord:ordering -> gnd_convergent -> bool
  (** check compatibility of ord with gc.gc_ord,gc.gc_prec! *)

val ground_pair : term -> term -> term * term
  (** Replace variables of terms by fresh constants *)

val gc_expert : ctx:context -> gnd_convergent -> t
  (** From a set of ground convergent equations, create an expert for
      the associated theory. *)

val pp_gc : Format.formatter -> gnd_convergent -> unit
  (** Pretty-print the system of ground convergent equations *)

(** {3 JSON encoding} *)

val gc_to_json : gnd_convergent -> json
val gc_of_json : ctx:context -> json -> gnd_convergent

(** {2 Some builtin theories} *)

val ac : ctx:context -> symbol -> t
  (** Theory of Associative-Commutative symbols, for the given symbol *)

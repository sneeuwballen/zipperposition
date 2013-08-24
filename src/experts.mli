(*
Zipperposition: a functional superposition prover for prototyping
Copyright (c) 2013, Simon Cruanes
All rights reserved.

Redistribution and use in source and binary forms, with or without
modification, are permitted provided that the following conditions are met:

Redistributions of source code must retain the above copyright notice, this
list of conditions and the following disclaimer.  Redistributions in binary
form must reproduce the above copyright notice, this list of conditions and the
following disclaimer in the documentation and/or other materials provided with
the distribution.

THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS "AS IS" AND
ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED
WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE ARE
DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT HOLDER OR CONTRIBUTORS BE LIABLE
FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL
DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR
SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER
CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY,
OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE
OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
*)

(** {1 Experts for theories} *)

(** The "experts" are programs that have specific knowledge of some theory,
    and that are able to perform some reasoning over terms that belong to
    this theory. They must be correct, but not necessarily complete, on
    the theory. *)

open Logtk

(** {2 General interface} *)

type t = {
  expert_name : string;                 (** Theory the expert works on *)
  expert_descr : string;                (** Description of the expert *)
  expert_equal : Term.t -> Term.t -> bool;  (** Check whether two terms are equal *)
  expert_sig : Symbol.SSet.t;           (** Symbols of the theory *)
  expert_clauses : Clause.t list;        (** Additional axioms *)
  expert_canonize : Term.t -> Term.t;       (** Get a canonical form of the term *)
  expert_ord : Ordering.t -> bool;        (** Compatible with ord? *)
  expert_update_ctx : Clause.context -> t list;(** How to update the context *)
  expert_ctx : Clause.context;                 (** Context used by the expert *)
  expert_solve : ((Term.t*Term.t) list -> Substs.t list) option;
    (** The expert may be able to solve systems of equations, returning
        a list of substitutions. Example: the simplex. *)
} (** An expert for some theory *)

val compatible_ord : t -> ord:Ordering.t -> bool
  (** Check whether using this expert is possible in the given ordering *)

val update_ctx : t -> ctx:Clause.context -> t list
  (** Copy of the expert, that uses the new context. The expert
      can be broken into several experts (in case it was a combination
      that is no longer possible with the new ordering) *)

val compatible : t -> t -> bool
  (** Simple syntaxic criterion to decide whether two experts
      are compatibles: check whether they have no symbol in common. *)

val combine : t -> t -> t
  (** Combine two experts into a new one, that works on
      the combination of their theories, assuming they are compatible. *)

val canonize : t -> Term.t -> Term.t
  (** Get the normal form of the term *)

val equal : t -> Term.t -> Term.t -> bool
  (** Check whether the terms are equal modulo theory *)

val signature : t -> Symbol.SSet.t
  (** Symbols of the theory associated to the expert *)

val is_redundant : t -> Clause.t -> bool
  (** Decide whether this clause is redundant *)

val simplify : t -> Clause.t -> Clause.t
  (** Simplify the clause *)

val clauses : t -> Clause.t list
  (** Get a list of clauses this expert needs to be present in the
      superposition prover (additional axioms). *)

val pp : Buffer.t -> t -> unit
val to_string : t -> string
val fmt : Format.formatter -> t -> unit

(** {2 Set of experts} *)

module Set : sig
  type expert = t (* alias *)

  type t
    (** A set of experts *)

  val empty : ctx:Clause.context -> t

  val add : t -> expert -> t

  val add_list : t -> expert list -> t

  val size : t -> int

  val to_seq : t -> expert Sequence.t
  val of_seq : t -> expert Sequence.t -> t

  val iter : t -> (expert -> unit) -> unit

  val update_ctx : t -> ctx:Clause.context -> t

  val is_redundant : t -> Clause.t -> bool

  val simplify : t -> Clause.t -> Clause.t

  val pp : Buffer.t -> t -> unit
  val fmt : Format.formatter -> t -> unit
end

(** {2 Ground joinable sets of equations} *)

(** We use ground convergent sets of equations to decide some equational
    theories. See
    "On using ground joinable equations in equational theorem proving", by
    Avenhaus, Hillenbrand, Lochner *)

type gnd_convergent = {
  gc_ord : string;              (** name of the ordering *)
  gc_theory : string;           (** Theory that is decided *)
  gc_prec : Symbol.t list;        (** Precedence *)
  gc_sig : Symbol.SSet.t;              (** Symbols of the theory *)
  gc_eqns : Clause.t list;       (** Equations of the system *)
} (** A set of ground convergent equations, for some order+precedence *)

val mk_gc : theory:string -> ord:string -> prec:Symbol.t list ->
            Clause.t list -> gnd_convergent
  (** Create a ground-convergent system from a list of equations
      and informations on the ordering. *)

val compatible_gc : ord:Ordering.t -> gnd_convergent -> bool
  (** check compatibility of ord with gc.gc_ord,gc.gc_prec! *)

val ground_pair : Term.t -> Term.t -> Term.t * Term.t
  (** Replace variables of terms by fresh constants *)

val gc_expert : ctx:Clause.context -> gnd_convergent -> t
  (** From a set of ground convergent equations, create an expert for
      the associated theory. *)

val pp_gc : Buffer.t -> gnd_convergent -> unit
val fmt_gc : Format.formatter -> gnd_convergent -> unit
  (** Pretty-print the system of ground convergent equations *)

(** {2 Some builtin theories} *)

val ac : ctx:Clause.context -> Symbol.t -> t
  (** Theory of Associative-Commutative symbols, for the given symbol *)

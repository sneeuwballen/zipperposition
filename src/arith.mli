
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

(** {6 Arithmetic Manipulations}
  
    This module contains builtin knownledge about (linear) arithmetic
    with the TPTP syntax and semantic. It mainly deals with two topics:

    - simplification (evaluation) of arithmetic terms/propositions
    - extraction of single subterms from (in)equations
*)

open Logtk

(** {2 Terms} *)

module T : sig
  val sum_list : Term.t list -> Term.t
    (** Sum of those terms *)

  val is_arith : Term.t -> bool
    (** Is the term arithmetic? *)

  val simplify : signature:Signature.t -> Term.t -> Term.t
    (** Arithmetic simplifications *)
end

(** {2 Polynomes of order 1, over several variables}.

    Variables, in this module, are non-arithmetic terms, i.e. non-interpreted
    functions and predicates, that occur immediately under an arithmetic
    operator. For instance, in the term "f(X) + 1 + 3 × a", the variables
    are "f(X)" and "a", with coefficients "1" and "3".
*)

module Monome : sig
  type t = private {
    coeffs : Symbol.t Term.TMap.t;
    constant : Symbol.t;
    divby : Symbol.t;  (* divide everything by this constant (cool for ints) *)
  }

  val const : Symbol.t -> t           (** Empty monomial, from constant (decides type) *)
  val singleton : ?divby:Symbol.t ->
                  Symbol.t ->
                  Term.t -> t         (** One term. *)
  val of_list : Symbol.t -> (Symbol.t * Term.t) list -> t

  val find : t -> Term.t -> Symbol.t  (** @raise Not_found if not present *)
  val mem : t -> Term.t -> bool       (** Is the term in the monome? *)

  val reduce_same_divby : t -> t -> t * t
    (** Reduce the two monomes to the same denominator *)

  val sum : t -> t -> t
  val difference : t -> t -> t
  val uminus : t -> t
  val product : t -> Symbol.t -> t  (** Product with constant *)
  val divby : t -> Symbol.t -> t    (** Division by constant, must be != 0 *)

  val of_term : signature:Signature.t ->
                Term.t -> t option  (** try to get a monome from a term *)

  val to_term : t -> Term.t         (** convert back to a term *)

  val pp : Buffer.t -> t -> unit
  val to_string : t -> string
end

(** {2 View a Literal as an arithmetic Literal}.

    This module provides a given view of a literal, where some arith variable
    has been extracted *)

module Lit : sig
  type t = private
  | Eq of Term.t * Monome.t
  | Neq of Term.t * Monome.t
  | L_less of Term.t * Monome.t   (* term < monome *)
  | L_lesseq of Term.t * Monome.t
  | R_less of Monome.t * Term.t
  | R_lesseq of Monome.t * Term.t

  val is_arith : Literal.t -> bool
    (** Is this literal arithmetic (i.e., root predicate is equality or
        inequality, with arithmetic operators just underneath)? *)

  val extract : Literal.t -> t list
    (** Possible views of a literal *)

end

(** {2 Other transformations} *)

val purify : ord:Ordering.t -> Literal.t array -> Literal.t array
  (** Purify the literals, by replacing arithmetic terms that occur
      under a non-interpreted predicate of formula, by a fresh variable,
      and adding the constraint variable=arith subterm to the literals. *)

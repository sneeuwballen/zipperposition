
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

(** {1 Polynomes of order 1, over several variables}.

    Variables, in this module, are non-arithmetic terms, i.e. non-interpreted
    functions and predicates, that occur immediately under an arithmetic
    operator. For instance, in the term "f(X) + 1 + 3 × a", the variables
    are "f(X)" and "a", with coefficients "1" and "3".

    Monomes of integers {b must} satisfy the property that each
    coefficient is divisible by the [divby] field.
*)

open Logtk

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
val add : t -> Symbol.t -> Term.t -> t  (** Add term with coefficient. Sums coeffs. *)
val remove : t -> Term.t -> t           (** Remove the term *)

val terms : t -> Term.t list
  (** List of terms that occur in the monome with non-nul coefficients *)

val to_list : t -> (Symbol.t * Term.t) list
  (** Terms and their coefficients. Ignores the constant and divby! *)

val var_occurs : Term.t -> t -> bool
  (** Does the variable occur in the monome? *)

val reduce_same_divby : t -> t -> t * t
  (** Reduce the two monomes to the same denominator *)

val sum : t -> t -> t
val difference : t -> t -> t
val uminus : t -> t
val product : t -> Symbol.t -> t  (** Product with constant *)
val divby : t -> Symbol.t -> t    (** Division by constant, must be > 0 *)
val succ : t -> t                 (** +1 *)
val pred : t -> t                 (** -1 *)

exception NotLinear
  
val of_term : signature:Signature.t -> Term.t -> t
  (** try to get a monome from a term.
      @raise NotLinear if the term is not a proper monome. *)

val of_term_opt : signature:Signature.t -> Term.t -> t option
  (** Exceptionless versionf of {!of_term} *)

val to_term : t -> Term.t         (** convert back to a term *)

val pp : Buffer.t -> t -> unit
val to_string : t -> string
val fmt : Format.formatter -> t -> unit

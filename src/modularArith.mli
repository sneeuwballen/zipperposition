
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

(** {1 Modular integer Arithmetic} *)

open Logtk

(** {2 Linear int expressions}

All symbols are supposed to be integer constants, and all terms must be
of type int. *)

module Expr : sig
  type t = private {
    const : Symbol.t;
    terms : (Symbol.t * FOTerm.t) list;
  }

  val eq : t -> t -> bool

  val const : Symbol.t -> t
  val singleton : Symbol.t -> FOTerm.t -> t  (* c * t *)

  val sum : t -> t -> t
  val diff : t -> t -> t
  val product : t -> Symbol.t -> t

  val add_const : t -> Symbol.t -> t
  
  val remove_const : t -> t (* remove the constant part, replace it by 0 *)

  val uminus : t -> t
  val succ : t -> t
  val pred : t -> t

  val is_const : t -> bool

  exception NotLinear

  val of_term : FOTerm.t -> t
    (** Convert a term into a linear expression
        @raise NotLinear if the term isn't a linear integer expression *)

  val to_term : t -> FOTerm.t
    (** Convert the expression back to a term *)

  val quotient : t -> Symbol.t -> t option
    (** [quotient e c] tries to divide [e] by [c], returning [e/c] if
        it is still an integer expression.
        For instance, [quotient (2x + 4y) 2] will return [Some (x + 2y)] *)

  val divisible : t -> Symbol.t -> bool
    (** [divisible e n] returns true if all coefficients of [e] are
        divisible by [n] and n is an int >= 2 *)

  val factorize : t -> (t * Symbol.t) option
    (** Factorize [e] into [Some (e',s)] if [e = e' x s], None
        otherwise (ie if s=1) *)

  val pp : Buffer.t -> t -> unit
  val to_string : t -> string
  val fmt : Format.formatter -> t -> unit
end

(** {2 Modular computations} *)

val modulo : n:Symbol.t -> Symbol.t -> Symbol.t
  (** Representative of the number in Z/nZ *)

val sum : n:Symbol.t -> Symbol.t -> Symbol.t -> Symbol.t
  (** Sum in Z/nZ *)

val uminus : n:Symbol.t -> Symbol.t -> Symbol.t
  (** Additive inverse in Z/nZ *)

val inverse : n:Symbol.t -> Symbol.t -> Symbol.t
  (** Multiplicative inverse in Z/nZ.
      TODO (only works if [n] prime) *)

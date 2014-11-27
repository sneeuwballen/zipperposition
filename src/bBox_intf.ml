
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

(** {1 Interface of BBox} *)

module type S = sig
  type inductive_cst

  type bool_lit = int
  (** Abstract boolean literal *)

  val neg : bool_lit -> bool_lit

  type injected = private
    | Clause_component of Literals.t
    | Provable of Literals.t * inductive_cst  (* clause provable within loop(i) *)
    | Name of string  (* name for CNF *)

  val inject_lits : Literals.t -> bool_lit
  (** Inject a clause into a boolean literal. No other clause will map
      to the same literal unless it is alpha-equivalent to this one.
      The boolean literal can be negative is the argument is a
      unary negative clause *)

  val inject_provable : Literals.t -> inductive_cst -> bool_lit
  (** Obtain the positive boolean literal such that [inject_provable lits n]
      represents "(bigOr lits) provable from S_loop(n)" *)

  val inject_name : string -> bool_lit
  val inject_name' : ('a, Buffer.t, unit, bool_lit) format4 -> 'a

  val extract : bool_lit -> injected option
  (** Recover the value that was injected into the literal, if any
      @raise Failure if the literal is <= 0 *)

  (** {2 Printers}
  Those printers print the content (injection) of a boolean literal, if any *)

  val pp : Buffer.t -> bool_lit -> unit
  val print : Format.formatter -> bool_lit -> unit
end

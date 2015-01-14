
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
  (** Negate the boolean literal *)

  val sign : bool_lit -> bool
  (** Current sign of the literal (positive or negative) *)

  val set_sign : bool -> bool_lit -> bool_lit
  (** Set the sign of the literal to the given boolean *)

  (** Predicate attached to a set of literals *)
  type lits_predicate =
    | TrailOk (** Some trail that proves lits is true *)
    [@@deriving ord]

  type ctx_predicate =
    | InLoop  (** ctx in loop(i) *)
    | InitOk (** Ctx is initialized *or* it's not in loop *)
    | ExpressesMinimality
        (** clause expresses the minimality of the model for S_loop(i) *)
    | ExpressesMinimalityAux
        (** helper for CNF, related to ExpressesMinimality *)
    [@@deriving ord]

  type injected = private
    | Clause_component of Literals.t
    | Lits of Literals.t * lits_predicate
    | Ctx of ClauseContext.t * inductive_cst * ctx_predicate
    | Name of string  (* name for CNF *)
    [@@deriving ord]

  val pp_injected : Buffer.t -> injected -> unit

  val inject_lits : Literals.t -> bool_lit
  (** Inject a clause into a boolean literal. No other clause will map
      to the same literal unless it is alpha-equivalent to this one.
      The boolean literal can be negative is the argument is a
      unary negative clause *)

  val inject_lits_pred : Literals.t -> lits_predicate -> bool_lit
  (** Inject the literals, predicate into a {!Lits} constructor *)

  val inject_ctx : ClauseContext.t -> inductive_cst -> ctx_predicate -> bool_lit
  (** Inject into {!Ctx} *)

  val inject_name : string -> bool_lit
  val inject_name' : ('a, Buffer.t, unit, bool_lit) format4 -> 'a

  val extract : bool_lit -> injected option
  (** Recover the value that was injected into the literal, if any
      @raise Failure if the literal is <= 0 *)

  val extract_exn : bool_lit -> injected
  (** Recover the value that was injected into the literal
      @raise Failure if the literal is <= 0 of it's not a proper boolean lit *)

  val inductive_cst : bool_lit -> inductive_cst option
  (** Obtain the inductive constant from this boolean lit, if any *)

  (** {2 Printers}
  Those printers print the content (injection) of a boolean literal, if any *)

  val pp : Buffer.t -> bool_lit -> unit
  val print : Format.formatter -> bool_lit -> unit
end

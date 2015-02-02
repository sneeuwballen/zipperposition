
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

type bool_lit = BoolSolver.lit
(** Abstract boolean literal *)

module type TERM = sig
  type t
  val to_term : t -> Logtk.FOTerm.t
  val equal : t -> t -> bool
  val compare : t -> t -> int
  val hash : t -> int
  val pp : Buffer.t -> t -> unit
  val print : Format.formatter -> t -> unit
end

module type S = sig
  module I : TERM
  module Case : TERM
  module Sub : TERM

  type inductive_cst = I.t
  type inductive_sub_cst = Sub.t
  type inductive_case = Case.t

  type t = BoolSolver.lit

  val neg : t -> t
  (** Negate the boolean literal *)

  val sign : t -> bool
  (** Current sign of the literal (positive or negative) *)

  val abs : t -> t
  (** Literal without its sign *)

  val set_sign : bool -> t -> t
  (** Set the sign of the literal to the given boolean *)

  type ctx_predicate =
    | InLoop  (** ctx in loop(i) *)
    | InitOk (** Ctx is initialized *or* it's not in loop *)
    | ExpressesMinimality of inductive_case
        (** clause expresses the minimality of the model for S_loop(i)
          in the case [t] *)
    [@@deriving ord]

  type injected = private
    | Clause_component of Literals.t
    | Ctx of ClauseContext.t * inductive_cst * ctx_predicate
    | Name of string  (* name for CNF *)
    | Input (** input marker *)
    [@@deriving ord]

  val pp_injected : Buffer.t -> injected -> unit

  val inject_lits : Literals.t -> t
  (** Inject a clause into a boolean literal. No other clause will map
      to the same literal unless it is alpha-equivalent to this one.
      The boolean literal can be negative is the argument is a
      unary negative clause *)

  val inject_ctx : ClauseContext.t -> inductive_cst -> ctx_predicate -> t
  (** Inject into {!Ctx} *)

  val inject_name : string -> t
  val inject_name' : ('a, Buffer.t, unit, t) format4 -> 'a

  val inject_input : t
  (** The special literal "input" *)

  val keep_in_splitting : t -> bool
  (** Returns [true] iff the literal should survive in the trail even
      after splitting (useful for special markers) *)

  val extract : t -> injected option
  (** Recover the value that was injected into the literal, if any
      @raise Failure if the literal is <= 0 *)

  val extract_exn : t -> injected
  (** Recover the value that was injected into the literal
      @raise Failure if the literal is <= 0 of it's not a proper boolean lit *)

  val inductive_cst : t -> inductive_cst option
  (** Obtain the inductive constant from this boolean lit, if any *)

  (** {2 Printers}
  Those printers print the content (injection) of a boolean literal, if any *)

  val pp : Buffer.t -> t -> unit
  val print : Format.formatter -> t -> unit
end


(*
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

(** {1 Evaluation of terms and formulas}
This module provides utilities to register functions to evaluate terms
on a per-symbol basis. Each symbol can have its own evaluator, that is
called to normalize terms whose head is the symbol. That is especially
handy for arithmetic, where there are many distinct interpreted symbols.
*)

(** {2 Signature of evaluator for some terms} *)

module type S = sig
  type term

  type eval_fun = Type.t -> Symbol.t -> term list -> term option
    (** An  evaluation function takes a symbol application, and a list of
        arguments, and can decide to return a new term based on that.
        If it returns None, it means that the term is already evaluated. *)

  type t
    (** An evaluator. It maps symbols to evaluators *)

  val create : unit -> t
    (** New evaluator *)

  val copy : t -> t
    (** Copy the evaluator *)

  val register : t -> Symbol.t -> eval_fun -> unit
    (** Add an evaluation function to the evaluator. If another function
        was already registered *)

  val register_list : t -> (Symbol.t * eval_fun) list -> unit

  val interpreted : t -> Symbol.t -> bool
    (** Is there a registered evaluation function for this symbol? *)

  val eval : t -> term -> term
    (** Recursively evaluate the term *)
end

(** {2 Evaluators for first-order typed terms} *)

module FO : sig
  include S with type term = FOTerm.t

  val eval_form : t -> FOFormula.t -> FOFormula.t
    (** Evaluate a formula *)

  val arith : (Symbol.t * eval_fun) list
    (** List of evaluators for arithmetic *)

  val with_arith : t -> unit
    (** Enrich the given evaluator with arithmetic evaluation *)
end


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

(** {6 Chaining Inferences} *)

(** We follow the paper "ordered chaining for total orderings" by
    L. Bachmair and H. Ganzinger.

    Chaining allows to deal with total order in a very efficient manner.
    Here it needs to know which symbols are orderings, an ordering instance
    being a pair of strict order symbol, and non-strict order symbol.
    Equality can only be the regular, "builtin" equality symbol.
*)

open Logtk

(** {2 Inference Rules} *)

(** Inference rules find the specification of the ordering in the
    given context/env, or in the clause they have to simplify. *)

val eq_chaining_active : Env.binary_inf_rule
  (** Equality chaining where the clause is active *)

val eq_chaining_passive : Env.binary_inf_rule
  (** Equality chaining where the clause is passive *)

val ineq_chaining_active : Env.binary_inf_rule
  (** Inequality chaining where the clause is active. *)

val ineq_chaining_passive : Env.binary_inf_rule
  (** Inequality chaining where the clause is passive. *)

val reflexivity_res : Env.unary_inf_rule
  (** Reflexivity resolution *)

val is_tautology : Clause.t -> bool
  (** Clause is always true in ordering models? *)

val simplify : Clause.t -> Clause.t
  (** Simplify the clause, by removing impossible literals *)

val axioms : ctx:Ctx.t -> instance:Theories.TotalOrder.instance -> Clause.t list
  (** Additional axioms *)

(** {2 Env} *)

val add_order : env:Env.t -> less:Symbol.t -> lesseq:Symbol.t -> unit
  (** Declare a new total ordering instance *)

val setup_env : env:Env.t -> unit
  (** Setup inference rules in Env. The specification that is used is
      the one from env.ctx. *)

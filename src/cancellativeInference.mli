
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

(** {1 Cancellative Inferences}

Superposition and chaining for linear expressions, with congruence classes
of terms and literals. Inferences are typically done with "scaled" literals,
i.e. literals that are multiplied by numeric coefficients so as to
bring the unified terms to the same coefficient. *)

open Logtk

val canc_sup_active: Env.binary_inf_rule 
  (** cancellative superposition where given clause is active *)

val canc_sup_passive: Env.binary_inf_rule
  (** cancellative superposition where given clause is passive *)

val cancellation: Env.unary_inf_rule
  (** cancellation (unifies some terms on both sides of a
      comparison operator) *)

val canc_equality_factoring: Env.unary_inf_rule
  (** cancellative equality factoring *)

val canc_ineq_chaining : Env.binary_inf_rule
  (** cancellative inequality chaining *)

val canc_reflexivity_res : Env.unary_inf_rule
  (** cancellative reflexivity resolution *)

val canc_ineq_factoring : Env.unary_inf_rule
  (** Factoring between two inequation literals *)

val is_tautology : Clause.t -> bool
  (** is the clause a tautology w.r.t linear expressions? *)

(** {2 Contributions to Env and Penv} *)

val setup_penv : ctx:Skolem.ctx -> penv:PEnv.t -> unit

val setup_env : env:Env.t -> unit
  (** Add rules to the environment. *)

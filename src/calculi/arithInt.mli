
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

(** {1 Cancellative Inferences on Integer Arithmetic}

Superposition, chaining and modulo reasoning for linear expressions, with
congruence classes of terms and literals. Inferences are typically done with
"scaled" literals, i.e. literals that are multiplied by numeric coefficients so
as to bring the unified terms to the same coefficient. *)

open Logtk

val case_switch_limit : int ref
  (** Positive integer: maximum width of an inequality case switch. Default: 30 *)

val div_case_switch_limit : int ref
  (** Positive integer: maximum prime number suitable for div_case_switch
      (ie maximum n for enumeration of cases in n^k | x) *)

module type S = sig
  module Env : Env.S
  module C : module type of Env.C
  module PS : module type of Env.ProofState

  (** {3 Equations and Inequations} *)

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
    (** cancellative inequality chaining.

        Also does case switch if conditions are present:
            C1 or a < b     C2 or b < c
        -------------------------------------
            C1 or C2 or or_{i=a+1....c-1} (b = i)
        if a and c are integer linear expressions whose difference is
        a constant. If a > c, then the range a...c is empty and the literal
        is just removed. *)

  val canc_ineq_factoring : Env.unary_inf_rule
    (** Factoring between two inequation literals *)

  val canc_inner_case_switch : Env.unary_inf_rule
    (** inference rule
          C1 or a <= b or b <= c
          ----------------------
              C1 or b!=i
          for each i in [c...a]. See (a <= b or b <= c or C1) as
          the rule  (b < a and c < b) -> C1, then make the head of the rule
          true *)

  val canc_lesseq_to_less : Env.lit_rewrite_rule
    (** Simplification:  a <= b  ----> a < b+1 *)

  (** {3 Divisibility} *)

  val canc_div_chaining : Env.binary_inf_rule
    (** Chain together two divisibility literals, assuming they share the
        same prime *)

  val canc_div_case_switch : Env.unary_inf_rule
    (** Eliminate negative divisibility literals within a power-of-prime
        quotient of Z:
        not (d^i | m) -----> *)

  val canc_div_prime_decomposition : Env.multi_simpl_rule
    (** Eliminate divisibility literals with a non-power-of-prime
        quotient of Z (for instance  [6 | a ---> { 2 | a, 3 | a }]) *)

  val canc_divisibility : Env.unary_inf_rule
    (** Infer divisibility constraints from integer equations,
        for instace   C or  2a=b ---->  C or 2 | b    if a is maximal *)

  (** {3 Other} *)

  val is_tautology : C.t -> bool
    (** is the clause a tautology w.r.t linear expressions? *)

  val purify : Env.simplify_rule
    (** Purify clauses by replacing arithmetic expressions occurring
        under terms by variables, and adding constraints *)

  val eliminate_unshielded : Env.multi_simpl_rule
    (** Eliminate unshielded variables using an adaptation of
        Cooper's algorithm *)

  (** {2 Contributions to Env} *)

  val register : unit -> unit
end

module Make(E : Env.S) : S with module Env = E

val extension : Extensions.t

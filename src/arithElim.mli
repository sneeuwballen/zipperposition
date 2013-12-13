
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

(** {1 Elimination of Arithmetic} 

  This module provides inferences that attempt to eliminate or isolate
  subterms of arithmetic expressions. It also provides simplification rules
  for clauses and literals.
*)

open Logtk

(** {2 Inference Rules for General Arithmetic} *)

val rewrite_lit : Env.lit_rewrite_rule
  (** Simplify literals by evaluating them; in the case of integer monomes,
      also reduce them to common denominator. *)

val eliminate_arith : Env.unary_inf_rule
  (** Try to find instances that make one literal unsatisfiable,
      and thus eliminate it *)

val factor_arith : Env.unary_inf_rule
  (** Try to unify terms of arithmetic literals
      TODO: remove? (cancellation should do it) *)

val purify_arith : Env.unary_inf_rule
  (** Purification inference.
    TODO: only purify non-ground composite arith expressions (ground ones
    are ok if AC-normalized) *)

val factor_bounds : Env.simplify_rule
  (** simplification rule
          C or a < t1 or a < t2 ... or a < tn
          ===================================
            C or a < max(t1, ..., tn)
    if t_i are comparable linear expressions *)

val bounds_are_tautology : Clause.t -> bool
(* detect tautologies such as
         t1 < a or a < t2 or C
         =====================  if t1 < t2 *)

(* TODO: redundancy criterion:
   a<b subsumes c<d if c<a and b<=d, or c<=a and b<d *)

(** {2 Modular Integer Arithmetic} *)

val simplify_remainder_term : Evaluator.FO.eval_fun
  (** Evaluation of some $remainder_e expressions. *)

val simplify_remainder_lit : Env.lit_rewrite_rule
  (** Simplifications on remainder literals/constraints *)

val infer_remainder_of_divisors : Env.unary_inf_rule
  (** Given a clause with [a mod n = 0], infers the same clause with
      [a mod n' = 0] where [n'] ranges over the strict, non-trivial divisors
      of [n].
      For instance, from [C or a mod 6 = 0] we can deduce
      [C or a mod 3 = 0] and [C or a mod 2 = 0]

      Also negative version: [C or a mod n != 0]
      ----> [C or Or_{d|n} (a mod d != 0)] *)

val enum_remainder_cases : Env.unary_inf_rule
  (** When remainder(t, n) occurs somewhere with [n] a constant, add the
      clause Or_{i=0...n-1} remainder(t, n) = i
      assuming n is not too big.

      XXX: maybe it's better to resolve directly [C or a mod n != b]
      into [C or Or_{i=0..n-1, i != b} a mod n = i]? *)

val remainder_of_equality : Env.unary_inf_rule
  (** Infer from [n a = b] that [n] divides [b] *)

(* TODO: use diophantine equations for solving divisibility constraints on
         linear expressions that contain only variables? *)

val remainder_of_var_ineq : Env.simplify_rule
  (** [t != n.X or C], where x \not\in vars(C), can be simplified
        simplified to [t mod n != 0 or C] *)

(* TODO: bridge between cancellative arith and modulo expressions (to
        allow   a mod 3 = 1   &  2.a = b   ---> b mod 3 = 2
        or something like this *)

(** {2 Setup} *)

val axioms : PFormula.t list
  (** Set of axioms useful to do arithmetic *)

val setup_penv : penv:PEnv.t -> unit

val setup_env : ?ac:bool -> env:Env.t -> unit
  (** Add inference rules to env.
    @param [ac] if true, add AC axioms for addition and multiplication (default [false]) *)

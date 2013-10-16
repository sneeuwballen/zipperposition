
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

(** {2 Inference Rules} *)

val rewrite_lit : Env.lit_rewrite_rule
  (** Simplify literals by evaluating them; in the case of integer monomes,
      also reduce them to common denominator. *)

val eliminate_arith : Env.unary_inf_rule
  (** Try to find instances that make one literal unsatisfiable,
      and thus eliminate it *)

val factor_arith : Env.unary_inf_rule
  (** Try to unify terms of arithmetic literals *)

val pivot_arith : Env.unary_inf_rule
  (** Pivot arithmetic literals *)

val purify_arith : Env.unary_inf_rule
  (** Purification inference *)

val axioms : PFormula.t list
  (** Set of axioms useful to do arithmetic *)

(* TODO: rule that simplifies  t < a | t < b if a and b are constants?
  see if it's into t < b or t < a (check redundancy criterion) *)

(* TODO: redundancy criterion:
   a<b subsumes c<d if c<a and b<=d, or c<=a and b<d *)

(* TODO: inference rule
          C1 or a <= b   C2 or b <= c
          ---------------------------
              C1 or C2
        if a > c are arithmetic expressions *)

(* TODO: simplification rule
        C or a < t1 or a < t2 ... or a < tn
        ===================================
          C or a < max(t1, ..., tn)
  if t_i are constants
*)

(** {2 Setup} *)

val setup_penv : penv:PEnv.t -> unit

val setup_env : ?ac:bool -> env:Env.t -> unit
  (** Add inference rules to env.
    @param [ac] if true, add AC axioms for addition and multiplication (default [false]) *)

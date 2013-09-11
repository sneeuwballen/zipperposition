
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

(** {1 Inference and simplification rules for the superposition calculus} *)

open Logtk

(** {2 Helpers} *)

val all_positions : Position.t -> Term.t -> 'a ->
                    ('a -> Term.t -> Position.t -> 'a) -> 'a

val fold_lits : ?both:bool -> (int -> Literal.t -> bool) ->
                ('a -> Term.t -> Term.t -> bool -> Position.t -> 'a) -> 'a ->
                Literal.t array -> 'a
  (** fold on equation sides of literals that satisfy predicate *)

val get_equations_sides : Clause.t -> Position.t -> Term.t * Term.t * bool
  (** get the term l at given position in clause, and r such that l ?= r
      is the Literal.t at the given position *)

(** {2 Inference rules} *)

val infer_active: Env.binary_inf_rule 
  (** superposition where given clause is active *)

val infer_passive: Env.binary_inf_rule
  (** superposition where given clause is passive *)

val infer_equality_resolution: Env.unary_inf_rule

val infer_equality_factoring: Env.unary_inf_rule

val infer_split : Env.unary_inf_rule
  (** hyper-splitting *)

(* TODO branch rewriting? *)

(** {2 Simplifications rules} *)

val is_tautology : Clause.t -> bool
  (** Check whether the clause is a (syntactic) tautology, ie whether
      it contains true or "A" and "not A" *)

(*
val is_semantic_tautology : Clause.t -> bool
  (** semantic tautology deletion, using a congruence closure algorithm
      to see if negative literals imply some positive Literal.t *)
*)

val basic_simplify : Clause.t -> Clause.t
  (** basic simplifications (remove duplicate literals, trivial literals,
      destructive equality resolution...) *)

val demodulate : ProofState.SimplSet.t -> Clause.t -> Clause.t
  (** rewrite clause using orientable unit equations *)

val backward_demodulate : ProofState.ActiveSet.t -> Clause.CSet.t -> Clause.t -> Clause.CSet.t
  (** backward version of demodulation: add to the set active clauses that
      can potentially be rewritten by the given clause *)

val positive_simplify_reflect : ProofState.SimplSet.t -> Clause.t -> Clause.t
val negative_simplify_reflect : ProofState.SimplSet.t -> Clause.t -> Clause.t

val subsumes : Literal.t array -> Literal.t array -> bool
  (** subsumes c1 c2 iff c1 subsumes c2 *)

val subsumes_with : Literal.t array Substs.scoped ->
                    Literal.t array Substs.scoped ->
                    Substs.t option
  (** returns subsuming subst if the first clause subsumes the second one *)

val eq_subsumes : Literal.t array -> Literal.t array -> bool
  (** equality subsumption *)

val subsumed_by_set : ProofState.ActiveSet.t -> Clause.t -> bool
  (** check whether the clause is subsumed by any clause in the set *)

val subsumed_in_set : ProofState.ActiveSet.t -> Clause.t -> Clause.t list
  (** list of clauses in the active set that are subsumed by the clause *)

val contextual_literal_cutting : ProofState.ActiveSet.t -> Clause.t -> Clause.t
  (** contexual Literal.t cutting *)

val condensation : Clause.t -> Clause.t
  (** condensation *)

(** {2 Contributions to Env} *)

val setup_env : env:Env.t -> unit
  (** Add rules to the environment. *)

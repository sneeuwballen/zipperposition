(*
Zipperposition: a functional superposition prover for prototyping
Copyright (C) 2012 Simon Cruanes

This is free software; you can redistribute it and/or
modify it under the terms of the GNU General Public License
as published by the Free Software Foundation; either version 2
of the License, or (at your option) any later version.

This is distributed in the hope that it will be useful,
but WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
GNU General Public License for more details.

You should have received a copy of the GNU General Public License
along with this program; if not, write to the Free Software
Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA
02110-1301 USA.
*)

(** Inference and simplification rules for the superposition calculus *)

open Types

val all_positions : position -> term ->  (* combinator *)
                    (term -> position -> 'b list) -> 'b list

val infer_active: Calculus.binary_inf_rule (** superposition where given clause is active *)

val infer_passive: Calculus.binary_inf_rule(** superposition where given clause is passive *)

val infer_equality_resolution: Calculus.unary_inf_rule

val infer_equality_factoring: Calculus.unary_inf_rule

(* TODO splitting without renaming *)
(* TODO branch rewriting *)

(** simplifications *)

val is_tautology : hclause -> bool
  (** Check whether the clause is a (syntactic) tautology, ie whether
      it contains true or "A" and "not A" *)

val is_semantic_tautology : hclause -> bool
  (** semantic tautology deletion, using a congruence closure algorithm
      to see if negative literals imply some positive literal *)

val basic_simplify : ord:ordering -> hclause -> hclause
  (** basic simplifications (remove duplicate literals, trivial literals,
      destructive equality resolution...) *)

val demodulate : ProofState.simpl_set -> clause -> hclause
  (** rewrite clause using orientable unit equations *)

val backward_demodulate : ProofState.active_set -> Clauses.CSet.t -> clause -> Clauses.CSet.t
  (** backward version of demodulation: add to the set active clauses that
      can potentially be rewritten by the given clause *)

val positive_simplify_reflect : ProofState.simpl_set -> clause -> hclause
val negative_simplify_reflect : ProofState.simpl_set -> clause -> hclause

(** subsumes c1 c2 iff c1 subsumes c2 *)
val subsumes : literal array -> literal array -> bool
val subsumes_with : literal array -> literal array -> substitution option   (** returns subsuming subst *)

(** equality subsumption *)
val eq_subsumes : literal array -> literal array -> bool

(** check whether the clause is subsumed by any clause in the set *)
val subsumed_by_set : ProofState.active_set -> clause -> bool

(** list of clauses in the active set that are subsumed by the clause *)
val subsumed_in_set : ProofState.active_set -> clause -> hclause list

(** contexual literal cutting *)
val contextual_literal_cutting : ProofState.active_set -> hclause -> hclause

(** condensation *)
val condensation : ord:ordering -> hclause -> hclause

(** Transform the clause into proper CNF; returns a list of clauses *)
val cnf_of : ord:ordering -> hclause -> hclause list

(** The superposition calculus *)
val superposition : Calculus.calculus

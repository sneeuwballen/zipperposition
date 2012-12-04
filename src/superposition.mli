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

(* statistics *)
val stat_superposition_call : statistics
val stat_equality_resolution_call : statistics
val stat_equality_factoring_call : statistics
val stat_subsumption_call : statistics
val stat_subsumed_in_set_call : statistics
val stat_subsumed_by_set_call : statistics
val stat_demodulate_call : statistics
val stat_demodulate_step : statistics

val print_stats : unit -> unit

val all_positions : position -> term ->  (* combinator *)
                    (term -> position -> 'b list) -> 'b list

val infer_active: Calculus.binary_inf_rule (** superposition where given clause is active *)

val infer_passive: Calculus.binary_inf_rule(** superposition where given clause is passive *)

val infer_equality_resolution: Calculus.unary_inf_rule

val infer_equality_factoring: Calculus.unary_inf_rule

(** simplifications *)

val is_tautology : clause -> bool

(** semantic tautology deletion, using a congruence closure algorithm
    to see if negative literals imply some positive literal *)
val is_semantic_tautology : clause -> bool

val basic_simplify : ord:ordering -> clause -> clause   (** basic simplifications *)

val demodulate : ord:ordering -> Index.unit_index -> clause -> clause
  (** rewrite clause using orientable unit equations *)

(* TODO splitting without renaming *)
(* TODO (forward) contextual literal cutting *)
(* TODO branch rewriting *)

val positive_simplify_reflect : ord:ordering -> Index.unit_index -> clause -> clause
val negative_simplify_reflect : ord:ordering -> Index.unit_index -> clause -> clause

(** subsumes c1 c2 iff c1 subsumes c2 *)
val subsumes : clause -> clause -> bool
val subsumes_with : clause -> clause -> substitution option   (** returns subsuming subst *)

(** check whether the clause is subsumed by any clause in the set *)
val subsumed_by_set : ProofState.active_set -> clause -> bool

(** list of clauses in the active set that are subsumed by the clause *)
val subsumed_in_set : ProofState.active_set -> clause -> hclause list

(** Transform the clause into proper CNF; returns a list of clauses *)
val cnf_of : ord:ordering -> clause -> clause list

(** The superposition calculus *)
val superposition : Calculus.calculus

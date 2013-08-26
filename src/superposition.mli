(*
ZipperPosition.t: a functional superposition prover for prototyping
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

(** {1 Inference and simplification rules for the superposition calculus} *)

open Logtk

(** {2 Helpers} *)

val all_positions : Position.t -> Term.t ->  (* combinator *)
                    (Term.t -> Position.t -> 'b list) -> 'b list

(** fold on equation sides of literals that satisfy predicate *)
val fold_lits : ?both:bool -> (int -> Literal.t -> bool) ->
                ('a -> Term.t -> Term.t -> bool -> Position.t -> 'a) -> 'a ->
                Literal.t array -> 'a

(** get the term l at given position in clause, and r such that l ?= r
    is the Literal.t at the given position *)
val get_equations_sides : Clause.t -> Position.t -> Term.t * Term.t * bool

(** {2 Inference rules} *)

val infer_active: Env.binary_inf_rule  (** superposition where given clause is active *)

val infer_passive: Env.binary_inf_rule (** superposition where given clause is passive *)

val infer_equality_resolution: Env.unary_inf_rule

val infer_equality_factoring: Env.unary_inf_rule

val infer_split : Env.unary_inf_rule   (** hyper-splitting *)

(* TODO branch rewriting *)

(** {2 Simplifications rules} *)

val is_tautology : Clause.t -> bool
  (** Check whether the clause is a (syntactic) tautology, ie whether
      it contains true or "A" and "not A" *)

val is_semantic_tautology : Clause.t -> bool
  (** semantic tautology deletion, using a congruence closure algorithm
      to see if negative literals imply some positive Literal.t *)

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

(** subsumes c1 c2 iff c1 subsumes c2 *)
val subsumes : Literal.t array -> Literal.t array -> bool
val subsumes_with : Literal.t array Substs.scoped -> Literal.t array Substs.scoped ->
                    Substs.t option   (** returns subsuming subst *)

(** equality subsumption *)
val eq_subsumes : Literal.t array -> Literal.t array -> bool

(** check whether the clause is subsumed by any clause in the set *)
val subsumed_by_set : ProofState.ActiveSet.t -> Clause.t -> bool

(** list of clauses in the active set that are subsumed by the clause *)
val subsumed_in_set : ProofState.ActiveSet.t -> Clause.t -> Clause.t list

(** contexual Literal.t cutting *)
val contextual_literal_cutting : ProofState.ActiveSet.t -> Clause.t -> Clause.t

(** condensation *)
val condensation : Clause.t -> Clause.t

(** {2 Contributions to Env} *)

val setup_env : env:Env.t -> unit
  (** Add rules to the environment. *)

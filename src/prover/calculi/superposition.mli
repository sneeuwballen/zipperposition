
(* This file is free software, part of Zipperposition. See file "license" for more details. *)

(** {1 Inference and simplification rules for the superposition calculus} *)

open Libzipperposition

(** {2 Inference rules} *)

val section : Util.Section.t

module type S = sig
  module Env : Env.S
  module C : module type of Env.C with type t = Env.C.t
  module PS : module type of Env.ProofState with type C.t = Env.C.t

  (** {6 Term Indices} *)

  val idx_sup_into : unit -> PS.TermIndex.t
  (** index for superposition into the set *)

  val idx_sup_from : unit -> PS.TermIndex.t
  (** index for superposition from the set *)

  val idx_back_demod : unit -> PS.TermIndex.t
  (** index for backward demodulation/simplifications *)

  val idx_fv : unit -> PS.SubsumptionIndex.t
  (** index for subsumption *)

  val idx_simpl : unit -> PS.UnitIndex.t
  (** index for forward simplifications *)

  (** {6 Inference Rules} *)

  val infer_active: Env.binary_inf_rule
  (** superposition where given clause is active *)

  val infer_passive: Env.binary_inf_rule
  (** superposition where given clause is passive *)

  val infer_equality_resolution: Env.unary_inf_rule

  val infer_equality_factoring: Env.unary_inf_rule

  (** {6 Simplifications rules} *)

  val is_tautology : C.t -> bool
  (** Check whether the clause is a (syntactic) tautology, ie whether
      it contains true or "A" and "not A" *)

  val is_semantic_tautology : C.t -> bool
  (** semantic tautology deletion, using a congruence closure algorithm
      to see if negative literals imply some positive Literal.t *)

  val handle_distinct_constants : Literal.t -> Literal.t
  (** Decide on "quoted" "symbols" (which are all distinct) *)

  val basic_simplify : Env.simplify_rule
  (** basic simplifications (remove duplicate literals, trivial literals,
      destructive equality resolution...) *)

  val demodulate : Env.simplify_rule
  (** rewrite clause using orientable unit equations *)

  val backward_demodulate : C.ClauseSet.t -> C.t -> C.ClauseSet.t
  (** backward version of demodulation: add to the set active clauses that
      can potentially be rewritten by the given clause *)

  val positive_simplify_reflect : Env.simplify_rule
  val negative_simplify_reflect : Env.simplify_rule

  val subsumes : Literal.t array -> Literal.t array -> bool
  (** subsumes c1 c2 iff c1 subsumes c2 *)

  val subsumes_with :
    Literals.t Scoped.t ->
    Literals.t Scoped.t ->
    Substs.FO.t option
  (** returns subsuming subst if the first clause subsumes the second one *)

  val eq_subsumes : Literal.t array -> Literal.t array -> bool
  (** equality subsumption *)

  val subsumed_by_active_set : C.t -> bool
  (** check whether the clause is subsumed by any clause in the set *)

  val subsumed_in_active_set : Env.backward_redundant_rule
  (** list of clauses in the active set that are subsumed by the clause *)

  val contextual_literal_cutting : Env.simplify_rule
  (** contexual Literal.t cutting of the given clause by the active set  *)

  val condensation : Env.simplify_rule
  (** condensation *)

  (** {6 Registration} *)

  val register : unit -> unit
  (** Register rules in the environment *)
end

val key : (module S) Flex_state.key
(** key to access the {!Env.flex_state}. After registration (after
    calling [register]), the Env's state contains
    a mapping from "superposition" to the packed module. *)

val register : sup:(module S) -> unit
(** Register the superposition module to its Environment's
    mixtbl. Done automatically by the {!extension}. *)

module Make(Env : Env.S) : S with module Env = Env

(** {2 As Extension}
    Extension named "superposition" *)

val extension : Extensions.t

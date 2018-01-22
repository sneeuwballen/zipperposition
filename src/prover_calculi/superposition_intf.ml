
(* This file is free software, part of Zipperposition. See file "license" for more details. *)

open Logtk
open Libzipperposition

module type S = sig
  module Env : Env.S
  module C : module type of Env.C with type t = Env.C.t
  module PS : module type of Env.ProofState with type C.t = Env.C.t

  (** {6 Term Indices} *)

  val idx_sup_into : unit -> PS.TermIndex.t
  (** index for superposition into the set *)

  val idx_sup_from : unit -> PS.TermIndex.t
  (** index for superposition from the set *)

  val idx_fv : unit -> PS.SubsumptionIndex.t
  (** index for subsumption *)

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

  val handle_distinct_constants : Env.lit_rewrite_rule
  (** Decide on "quoted" "symbols" (which are all distinct) *)

  val basic_simplify : Env.simplify_rule
  (** basic simplifications (remove duplicate literals, trivial literals,
      destructive equality resolution...) *)

  val subsumes : Literal.t array -> Literal.t array -> bool
  (** subsumes c1 c2 iff c1 subsumes c2 *)

  val subsumes_with :
    Literals.t Scoped.t ->
    Literals.t Scoped.t ->
    (Subst.FO.t * Proof.tag list) option
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

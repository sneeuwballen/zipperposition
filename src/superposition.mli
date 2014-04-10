
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

(** {2 Inference rules} *)

module type S = sig
  module Env : Env.S
  module C : module type of Env.C
  module PS : module type of Env.ProofState

  (** {6 Term Indices} *)

  val idx_sup_into : unit -> PS.TermIndex.t    (** index for superposition into the set *)
  val idx_sup_from : unit -> PS.TermIndex.t    (** index for superposition from the set *)
  val idx_back_demod : unit -> PS.TermIndex.t  (** index for backward demodulation/simplifications *)
  val idx_fv : unit -> PS.SubsumptionIndex.t   (** index for subsumption *)
  val idx_simpl : unit -> PS.UnitIndex.t       (** index for forward simplifications *)

  (** {6 Inference Rules} *)

  val infer_active: Env.binary_inf_rule
    (** superposition where given clause is active *)

  val infer_passive: Env.binary_inf_rule
    (** superposition where given clause is passive *)

  val infer_equality_resolution: Env.unary_inf_rule

  val infer_equality_factoring: Env.unary_inf_rule

  val infer_split : Env.unary_inf_rule
    (** hyper-splitting *)

  (* TODO branch rewriting? *)

  (** {6 Simplifications rules} *)

  val is_tautology : C.t -> bool
    (** Check whether the clause is a (syntactic) tautology, ie whether
        it contains true or "A" and "not A" *)

  val is_semantic_tautology : C.t -> bool
    (** semantic tautology deletion, using a congruence closure algorithm
        to see if negative literals imply some positive Literal.t *)

  val handle_distinct_constants : Literal.t -> Literal.t
    (** Decide on "quoted" "symbols" (which are all distinct) *)

  val basic_simplify : C.t -> C.t
    (** basic simplifications (remove duplicate literals, trivial literals,
        destructive equality resolution...) *)

  val demodulate : C.t -> C.t
    (** rewrite clause using orientable unit equations *)

  val backward_demodulate : C.CSet.t -> C.t -> C.CSet.t
    (** backward version of demodulation: add to the set active clauses that
        can potentially be rewritten by the given clause *)

  val positive_simplify_reflect : C.t -> C.t
  val negative_simplify_reflect : C.t -> C.t

  val subsumes : Literal.t array -> Literal.t array -> bool
    (** subsumes c1 c2 iff c1 subsumes c2 *)

  val subsumes_with : Literal.t array -> Substs.scope ->
                      Literal.t array -> Substs.scope ->
                      Substs.FO.t option
    (** returns subsuming subst if the first clause subsumes the second one *)

  val eq_subsumes : Literal.t array -> Literal.t array -> bool
    (** equality subsumption *)

  val subsumed_by_active_set : C.t -> bool
    (** check whether the clause is subsumed by any clause in the set *)

  val subsumed_in_active_set : C.t -> C.CSet.t
    (** list of clauses in the active set that are subsumed by the clause *)

  val contextual_literal_cutting : C.t -> C.t
    (** contexual Literal.t cutting of the given clause by the active set  *)

  val condensation : C.t -> C.t
    (** condensation *)

  (** {6 Registration} *)

  val register : unit -> unit
  (** Register rules in the environment *)
end

module Make(Env : Env.S) : S with module Env = Env

(** {2 As Extension}
Extension named "superposition" *)

val extension : Extensions.t

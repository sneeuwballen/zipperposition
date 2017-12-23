
(* This file is free software, part of Zipperposition. See file "license" for more details. *)

open Logtk
open Libzipperposition

module type S = sig
  module E : Env.S
  module Solver : Sat_solver.S
  module BLit = BBox.Lit

  val split : E.multi_simpl_rule
  (** Split a clause into components *)

  val check_empty : E.unary_inf_rule
  (** Forbid empty clauses with trails, i.e. adds the negation of their
      trails to the SAT-solver *)

  val before_check_sat : unit Signal.t
  val after_check_sat : unit Signal.t

  val filter_absurd_trails : (Trail.t -> bool) -> unit
  (** [filter_trails f] calls [f] on every trail associated with the empty
      clause. If [f] returns [false], the trail is ignored, otherwise
      it's negated and sent to the SAT solver *)

  val check_satisfiability : E.generate_rule
  (** Checks that the SAT context is still valid *)

  type cut_res = private {
    cut_form: Cut_form.t; (** the lemma itself *)
    cut_pos: E.C.t list; (** clauses true if lemma is true *)
    cut_lit: BLit.t; (** lit that is true if lemma is true *)
    cut_depth: int; (** if the lemma is used to prove another lemma *)
    cut_proof: Proof.Step.t; (** where does the lemma come from? *)
    cut_proof_parent: Proof.Parent.t; (** how to justify sth from the lemma *)
    cut_reason: unit CCFormat.printer option; (** Informal reason why the lemma was added *)
  }
  (** This represents a cut on a formula, where we obtain a list
      of clauses [cut_pos] representing the formula itself with the
      trail [lemma], and a boolean literal [cut_lit] that is true iff
      the trail is true.

      Other modules, when a cut is introduced, will try to disprove
      the lemma (e.g. by induction or theory reasoning).
  *)

  val cut_form : cut_res -> Cut_form.t
  val cut_pos : cut_res -> E.C.t list
  val cut_lit : cut_res -> BLit.t
  val cut_depth : cut_res -> int
  val cut_proof : cut_res -> Proof.Step.t
  val cut_proof_parent : cut_res -> Proof.Parent.t

  val pp_cut_res : cut_res CCFormat.printer
  val cut_res_clauses: cut_res -> E.C.t Sequence.t

  val print_lemmas : unit CCFormat.printer
  (** print the current list of lemmas, and their status *)

  val introduce_cut :
    ?reason:unit CCFormat.printer ->
    ?penalty:int ->
    ?depth:int ->
    Cut_form.t ->
    Proof.Step.t ->
    cut_res
  (** Introduce a cut on the given clause(s). Pure.
      @param reason some comment on why the lemma was added *)

  val add_prove_lemma : (cut_res -> E.C.t list E.conversion_result) -> unit
  (** Add a mean of proving lemmas *)

  val add_lemma : cut_res -> unit
  (** Add the given cut to the list of lemmas. Modifies the global list
      of lemmas.
      It will call the functions added by {!add_prove_lemma} to try and
      prove this one. *)

  val add_imply : cut_res list -> cut_res -> Proof.Step.t -> unit
  (** [add_imply l res] means that the conjunction of lemmas in [l]
      implies that the lemma [res] is proven *)

  val on_input_lemma : cut_res Signal.t
  (** Triggered every time a cut is introduced  for an input lemma
      (i.e. every time a statement of the form `lemma F` is translated) *)

  val on_lemma : cut_res Signal.t
  (** Triggered every time a cut is introduced, by any means. In
      particular it is triggered at least as often as {!on_input_lemma} *)

  val convert_lemma : E.clause_conversion_rule
  (** Intercepts input lemmas and converts them into clauses.
      Triggers {!on_input_lemma} with the resulting cut *)

  val register : split:bool -> unit -> unit
  (** Register inference rules to the environment
      @param split if true, the clause splitting rule is added. Otherwise
      Avatar is only used for other things such as induction. *)
end

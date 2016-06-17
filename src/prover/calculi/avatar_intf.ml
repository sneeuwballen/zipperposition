
(* This file is free software, part of Zipperposition. See file "license" for more details. *)

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
  (** Checks  that the SAT context is still valid *)

  type cut_res = {
    cut_pos: E.C.t list; (** clauses true if lemma is true *)
    cut_neg: E.C.t list; (** clauses true if lemma is false *)
    cut_lit: BLit.t; (** lit that is true if lemma is true *)
  }

  val pp_cut_res : cut_res CCFormat.printer
  val cut_res_clauses: cut_res -> E.C.t Sequence.t

  val introduce_cut :
    Literals.t list ->
    ProofStep.t ->
    cut_res
  (** Introduce a cut on the given clause(s). *)

  val on_input_lemma : cut_res Signal.t
  (** Triggered every time a cut is introduced  for an input lemma
      (i.e. every time a statement of the form `lemma F` is translated) *)

  val convert_lemma : E.clause_conversion_rule
  (** Intercepts input lemmas and converts them into clauses.
      Triggers {!on_input_lemma} with the resulting cut *)

  val register : unit -> unit
  (** Register inference rules to the environment *)
end

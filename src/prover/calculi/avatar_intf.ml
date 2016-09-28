
(* This file is free software, part of Zipperposition. See file "license" for more details. *)

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
  (** Checks  that the SAT context is still valid *)

  type cut_res = private {
    cut_src: Literals.t list ; (** the lemma itself *)
    cut_pos: E.C.t list; (** clauses true if lemma is true *)
    cut_neg: E.C.t list; (** clauses true if lemma is false *)
    cut_skolems: (ID.t * Type.t) list;
      (** skolems of universal variables in [cut_neg] *)
    cut_lit: BLit.t; (** lit that is true if lemma is true *)
  }
  (** This represents a cut on a formula, where we obtain a list
      of clauses [cut_pos] representing the formula itself with the
      trail [lemma],
      and a list of clauses [cut_neg] with the trail [not lemma] that
      must be refuted to prove the lemma. Those negative clauses correspond
      to the negation of [forall x1...xn. F]; they are ground and
      [cut_skolems] contains the list of Skolem constants corresponding
      to [x1,...,xn] *)

  val pp_cut_res : cut_res CCFormat.printer
  val cut_res_clauses: cut_res -> E.C.t Sequence.t

  val print_lemmas : unit CCFormat.printer
  (** print the current list of lemmas, and their status *)

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

  val register : split:bool -> unit -> unit
  (** Register inference rules to the environment
      @param split if true, the clause splitting rule is added. Otherwise
      Avatar is only used for other things such as induction. *)
end

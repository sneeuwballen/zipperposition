
(* This file is free software, part of Zipperposition. See file "license" for more details. *)

module type S = sig
  module E : Env.S
  module Solver : Sat_solver.S

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

  val save_clause : tag:int -> E.C.t -> unit
  (** Map the tag to the clause *)

  val get_clause : tag:int -> E.C.t option
  (** Recover clause from the tag, if any *)

  val introduce_cut :
    Literals.t ->
    (CompactClause.t -> Proof.t) ->
    E.C.t list * E.Ctx.BoolLit.t
  (** Introduce a cut on the given clause *)

  val register : unit -> unit
  (** Register inference rules to the environment *)
end

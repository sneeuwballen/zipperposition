
(* This file is free software, part of Zipperposition. See file "license" for more details. *)

(** {1 Basic Splitting à la Avatar}

    We don't implement all the stuff from Avatar, in particular all clauses are
    active whether or not their trail is satisfied in the current model.
    Trails are only used to make splits easier {b currently}.

    Future work may include locking clauses whose trails are unsatisfied.

    Depends on the "meta" extension.
*)

open Logtk

type 'a printer = Format.formatter -> 'a -> unit
type formula = TypedSTerm.t

(** {2 Avatar: splitting+sat} *)

module type S = sig
  module E : Env.S
  module Solver : BoolSolver.SAT

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

  val introduce_cut : formula -> (CompactClause.t -> Proof.t) ->
    E.C.t list * E.Ctx.BoolLit.t
  (** Introduce a cut on the given formula *)

  val register : unit -> unit
  (** Register inference rules to the environment *)
end

module Make(E : Env.S)(Sat : BoolSolver.SAT) : S
  with module E = E and module Solver = Sat

val extension : Extensions.t
(** Extension that enables Avatar splitting and create a new SAT-solver. *)


(* This file is free software, part of Zipperposition. See file "license" for more details. *)

open Logtk

type proof_step = Proof.Step.t
type proof = Proof.t

type result =
  | Sat
  | Unsat of proof

exception WrongState of string

module type S = sig
  module Lit = BBox.Lit

  exception UndecidedLit

  type clause = Lit.t list

  val add_clause : proof:proof_step -> Lit.t list -> unit
  (** [add_clause ~tag ~proof c] adds the constraint [c] to the SAT solver,
      annotated with [proof]. [tag] is a unique identifier for this constraint
      and must not have been already used. *)

  val add_clauses : proof:proof_step -> Lit.t list list -> unit

  val add_clause_seq : proof:proof_step -> Lit.t list Sequence.t -> unit

  val check : full:bool -> unit -> result
  (** Is the current problem satisfiable?
      @param full if true, check unconditionally *)

  val last_result : unit -> result
  (** Last computed result. This does not compute a new result *)

  val valuation : Lit.t -> bool
  (** Assuming the last call to {!check} returned {!Sat}, get the boolean
      valuation for this (positive) literal in the current model.
      @raise WrongState if the last result wasn't {!Sat} *)

  val valuation_level : Lit.t -> bool * int
  (** Gives the value of a literal in the model, as well as its
      decision level. If decision level is 0, the literal has been proved,
      rather than decided/propagated
      @raise WrongState if the last result wasn't {!Sat} *)

  val proved_at_0 : Lit.t -> bool option
  (** If the literal has been propagated at decision level 0,
      return its value (which does not depend on the model).
      Otherwise return [None] *)

  val all_proved: unit -> Lit.Set.t
  (** Set of (signed) proved literals *)

  val set_printer : Lit.t CCFormat.printer -> unit
  (** How to print literals? *)

  val get_proof : unit -> proof
  (** Return a proof of [false], assuming {!check} returned [Unsat].
      The leaves of the proof are input clauses' proofs, the internal
      nodes are clauses deduced by the SAT solver.
      @raise WrongState if the last result isn't [Unsat] *)

  val get_proof_opt : unit -> proof option
  (** Obtain the proof, if any *)

  val get_proof_of_lit : Lit.t -> proof
  (** [get_proof_of_lit lit] returns the proof of [lit], assuming it has been
      proved true at level 0 (see {!valuation_level})
      @raise Invalid_argument if the literal is not at level 0 *)

  val setup: unit -> unit
end

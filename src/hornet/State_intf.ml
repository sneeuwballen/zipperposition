
(* This file is free software, part of Zipperposition. See file "license" for more details. *)

open Libzipperposition

type term = FOTerm.t
type ty = Type.t
type statement = (Clause.t, term, ty) Statement.t
type proof = Hornet_types.proof
type event = Hornet_types.event

module type CTX_ARGS = sig
  val ord : Ordering.t
  val signature : Type.t ID.Map.t
  val conf : Flex_state.t
  val statements : statement CCVector.ro_vector
  val max_depth : int
  val dimacs_file : string option
end

module type CONTEXT = sig
  type bool_clause = Bool_lit.t list

  val bool_state : Bool_lit.state

  (** {6 SAT} *)

  val on_backtrack : (unit -> unit) -> unit
  (** Push the given callback on a stack. It will be
      called when the SAT solver backtracks. *)

  val add_clause : proof -> bool_clause -> unit
  val add_clause_l : proof -> bool_clause list -> unit

  module Form : sig
    type t
    val imply : t -> t -> t
    val atom : Bool_lit.t -> t
    val and_ : t list -> t
    val or_: t list -> t
    val not_ : t -> t
  end

  val add_form : proof -> Form.t -> unit
  (** Add boolean form to the SAT solver *)

  val send_event : event -> unit
  (** Send an event to notify other parts of the prover *)

  (** {6 FO} *)

  val renaming_cleared: unit -> Subst.Renaming.t
  (** A global renaming, reset every time this is called *)

  (** {6 Config} *)

  include CTX_ARGS
end

(** A reasoning engine. Each theory is informed when the SAT solver
    makes some decisions, and when it backtracks.
    In return, theories can exploit their domain-specific knowledge
    to propagate new (boolean) clauses to the SAT solver,
    and to detect unsatisfiability by adding a conflict clause.

    Theories can communicate via boolean literals.

    Initially a theory is a function that takes a context,
    and returns some callback that will be called when the solver
    makes decisions *)

module type THEORY = sig
  module Ctx : CONTEXT

  val name : string

  val on_assumption : Bool_lit.t -> unit
  (** Called every time the SAT solver picks a new boolean literal *)

  val on_event : event -> unit
  (** React to events *)

  val set_depth_limit : int -> unit
  (** Called when the depth limit is changed *)
end

module type THEORY_FUN = functor(C:CONTEXT) -> THEORY with module Ctx = C

type theory_fun = (module THEORY_FUN)

(** Parameters to create a Context *)
module type ARGS = sig
  include CTX_ARGS
  val theories : theory_fun list
end


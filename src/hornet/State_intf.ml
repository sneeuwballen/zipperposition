
(* This file is free software, part of Zipperposition. See file "license" for more details. *)

open Libzipperposition

type term = FOTerm.t
type ty = Type.t
type statement = (Clause.t, term, ty) Statement.t

module type PROOF = sig
  type t

  val of_clause_proof : Clause.Proof.t -> t
  include Interfaces.PRINT with type t := t
end

module type BOOL_LIT = Bool_lit_intf.S

module type CONTEXT = sig
  module Proof : PROOF
  module B_lit : BOOL_LIT with type proof := Proof.t

  type bool_clause = B_lit.t list

  (** {6 SAT} *)

  val raise_conflict : bool_clause -> Proof.t -> 'a

  val on_backtrack : (unit -> unit) -> unit
  (** Push the given callback on a stack. It will be
      called when the SAT solver backtracks. *)

  val add_clause : bool_clause -> unit
  val add_clause_l : bool_clause list -> unit

  module Form : sig
    type t
    val imply : t -> t -> t
    val atom : B_lit.t -> t
    val and_ : t list -> t
    val or_: t list -> t
    val not_ : t -> t
  end

  val add_form : Form.t -> unit

  (** {6 Config} *)

  val conf : Flex_state.t
  val ord : Ordering.t
  val signature: Type.t ID.Map.t
  val max_depth : int
  val statements : statement CCVector.ro_vector
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

  val on_assumption : Ctx.B_lit.t -> unit
  (** Called every time the SAT solver picks a new boolean literal *)

  val set_depth_limit : int -> unit
  (** Called when the depth limit is changed *)

  val on_exit : unit -> unit
  (** Called before exit *)
end

module type THEORY_FUN = functor(C:CONTEXT) -> THEORY with module Ctx = C

type theory_fun = (module THEORY_FUN)

(** Parameters to create a Context *)
module type ARGS = sig
  val theories : theory_fun list
  val ord : Ordering.t
  val signature : Type.t ID.Map.t
  val conf : Flex_state.t
  val statements : statement CCVector.ro_vector
  val max_depth : int
end


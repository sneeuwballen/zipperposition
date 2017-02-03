
(* This file is free software, part of Zipperposition. See file "license" for more details. *)

(** {1 State} *)

(** The global state of a proof attempt *)

open Libzipperposition

(** {2 Proofs} *)

module Proof : sig
  type t
end

(** {2 Boolean Literals} *)

module type BOOL_LIT = sig
  type view =
    | Fresh of int
    | Select_lit of Clause.General.t * Clause.General.idx
    | Depth_limit of int

  type t = private {
    view: view;
    sign: bool;
  }

  val fresh : unit -> t
  val select_lit : Clause.General.t -> Clause.General.idx -> t
  val depth_limit : int -> t

  include Msat.Formula_intf.S with type t := t and type proof = Proof.t
end

(** {2 Context for Theories} *)

(** Each theory is given this context, which serves to communicate
    with the SAT solver *)

module type CONTEXT = sig
  module B_lit : BOOL_LIT with type proof = Proof.t

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
  val statements : Statement.clause_t CCVector.ro_vector
end

type context = (module CONTEXT)

(** {2 Theory} *)

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
end

module type THEORY_FUN = functor(C:CONTEXT) -> THEORY with module Ctx = C

type theory_fun = (module THEORY_FUN)

(** {2 State} *)

type t

val create :
  conf:Flex_state.t ->
  ord:Ordering.t ->
  signature:Type.t ID.Map.t ->
  theories:theory_fun list ->
  statements: Statement.clause_t CCVector.ro_vector ->
  max_depth:int ->
  unit ->
  t

val context : t -> context

(** {2 Result} *)

type res =
  | Sat
  | Unsat
  | Unknown

val pp_res : res CCFormat.printer

val run : t -> res
(** Main loop. It calls the SAT solver which takes care of
    (trying to) find a model *)


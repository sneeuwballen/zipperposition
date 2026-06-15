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

  type t

  exception UndecidedLit

  type clause = Lit.t list

  type result =
    | Sat
    | Unsat of proof

  val add_clause : t -> proof:proof_step -> Lit.t list -> unit
  val add_clauses : t -> proof:proof_step -> Lit.t list list -> unit
  val add_clause_seq : t -> proof:proof_step -> Lit.t list Iter.t -> unit
  val check : t -> full:bool -> unit -> result
  val last_result : t -> result
  val valuation : t -> Lit.t -> bool
  val valuation_level : t -> Lit.t -> bool * int
  val proved_at_0 : t -> Lit.t -> bool option
  val all_proved : t -> Lit.Set.t
  val set_printer : t -> Lit.t CCFormat.printer -> unit
  val get_proof : t -> proof
  val get_proof_opt : t -> proof option
  val get_proof_of_lit : t -> Lit.t -> proof
  val setup : t -> unit
  val clear : t -> ?size:[ `Big | `Small | `Tiny ] -> unit -> unit
end

(** Static (backward-compatible) interface: no [type t], functions take unit for
    "self". Used temporarily by Avatar which wraps a solver in a module. *)
module type STATIC = sig
  module Lit = BBox.Lit

  exception UndecidedLit

  type clause = Lit.t list

  type result =
    | Sat
    | Unsat of proof

  val add_clause : proof:proof_step -> Lit.t list -> unit
  val add_clauses : proof:proof_step -> Lit.t list list -> unit
  val add_clause_seq : proof:proof_step -> Lit.t list Iter.t -> unit
  val check : full:bool -> unit -> result
  val last_result : unit -> result
  val valuation : Lit.t -> bool
  val valuation_level : Lit.t -> bool * int
  val proved_at_0 : Lit.t -> bool option
  val all_proved : unit -> Lit.Set.t
  val set_printer : Lit.t CCFormat.printer -> unit
  val get_proof : unit -> proof
  val get_proof_opt : unit -> proof option
  val get_proof_of_lit : Lit.t -> proof
  val setup : unit -> unit
  val clear : ?size:[ `Big | `Small | `Tiny ] -> unit -> unit
end

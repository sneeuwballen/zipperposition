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

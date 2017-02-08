
(* This file is free software, part of Zipperposition. See file "license" for more details. *)

type lit = Hornet_types.lit
type clause = Hornet_types.clause
type clause_idx = Hornet_types.clause_idx
type proof = Hornet_types.proof

type view =
  | Fresh of int
  | Box_clause of clause
  | Select_lit of clause * clause_idx
  | Ground_lit of lit (* must be ground *)

(** Encapsulate objects into boolean literals that can be handled by
    the SAT solver *)
module type S = sig
  type t
  type proof = Hornet_types.proof

  val view : t -> view
  val sign : t -> bool

  val fresh : unit -> t
  val select_lit : clause -> clause_idx -> t
  val box_clause : clause -> t
  val ground : lit -> t

  include Msat.Formula_intf.S with type t := t and type proof := proof

  val pp_clause : t list CCFormat.printer
end

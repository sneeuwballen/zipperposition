
(* This file is free software, part of Zipperposition. See file "license" for more details. *)

(** Encapsulate objects into boolean literals that can be handled by
    the SAT solver *)
module type S = sig
  type view =
    | Fresh of int
    | Box_clause of Clause.t
    | Select_lit of Clause.General.t * Clause.General.idx
    | Ground_lit of Lit.t (* must be ground *)
    | Depth_limit of int (* max number of "risky" inferences *)

  type t

  val view : t -> view
  val sign : t -> bool

  val fresh : unit -> t
  val select_lit : Clause.General.t -> Clause.General.idx -> t
  val box_clause : Clause.t -> t
  val ground : Lit.t -> t
  val depth_limit : int -> t

  type proof
  include Msat.Formula_intf.S with type t := t and type proof := proof

  val pp_clause : t list CCFormat.printer
end

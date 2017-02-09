
(* This file is free software, part of Zipperposition. See file "license" for more details. *)

(** {1 Bool Literals} *)

(** The goal is to encapsulate objects into boolean literals that can be
    handled by the SAT solver *)

type lit = Hornet_types.lit
type clause = Hornet_types.clause
type clause_idx = Hornet_types.clause_idx
type proof = Hornet_types.proof

(** {2 Basics} *)

type atom = Hornet_types.bool_atom
type t = Hornet_types.bool_lit

include Msat.Formula_intf.S with type t := t and type proof := proof

type view =
  | Fresh of int
  | Box_clause of clause
  | Select_lit of clause * clause_idx
  | Ground_lit of lit (* must be ground and positive *)

val atom : t -> atom
val view : t -> view
val sign : t -> bool

include Interfaces.PRINT with type t := t
include Interfaces.HASH with type t := t

(** {2 Constructors} *)

type state
(** A mutable state that is used to allocate fresh literals *)

val create_state: unit -> state

val of_atom : ?sign:bool -> atom -> t

val fresh : state -> t
val select_lit : state -> clause -> clause_idx -> t
val box_clause : state -> clause -> t
val ground : lit -> t

(** {2 Boolean Clauses} *)

type bool_clause = t list

val pp_clause : bool_clause CCFormat.printer

(** {2 Containers} *)

module Tbl : CCHashtbl.S with type key = t

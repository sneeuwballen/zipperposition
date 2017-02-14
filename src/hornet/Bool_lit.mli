
(* This file is free software, part of Zipperposition. See file "license" for more details. *)

(** {1 Bool Literals} *)

(** The goal is to encapsulate objects into boolean literals that can be
    handled by the SAT solver *)

open Hornet_types

(** {2 Basics} *)

type atom = Hornet_types.bool_atom
type t = Hornet_types.bool_lit
type proof = Hornet_types.proof
type view = Hornet_types.bool_atom

include Msat.Formula_intf.S with type t := t and type proof := proof

val view : t -> view
val atom : t -> atom
val sign : t -> bool

include Interfaces.PRINT with type t := t
include Interfaces.HASH with type t := t
include Interfaces.ORD with type t := t

(** {2 Constructors} *)

type state
(** A mutable state that is used to allocate fresh literals *)

val create_state: unit -> state

val of_atom : ?sign:bool -> atom -> t

val fresh : state -> t
val box_clause : state -> clause -> t
val ground : state -> lit -> t

(** {2 Boolean Clauses} *)

type bool_clause = t list

val equal_clause : bool_clause -> bool_clause -> bool
val hash_clause : bool_clause -> int
val pp_clause : bool_clause CCFormat.printer

(** {2 Boolean Trails} *)

type bool_trail = Hornet_types.bool_trail

val pp_trail : bool_trail CCFormat.printer

(** {2 Containers} *)

module Tbl : CCHashtbl.S with type key = t

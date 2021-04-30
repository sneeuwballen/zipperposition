
(* This file is free software, part of Zipperposition. See file "license" for more details. *)

(** {1 Boolean selection functions} *)

open Logtk

module PB = Position.Build
module T = Term
module Pos = Position

(** As described in FBoolSup paper, Boolean selection function
    selects positions in the clause that are non-interpreted 
    Boolean subterms. *)

type t = Literal.t array -> (Term.t * Position.t) list

type parametrized = strict:bool -> ord:Ordering.t -> t

(** {2 Selection Functions} *)

val all_selectable_subterms : ?forbidden:T.VarSet.t -> ord:Ordering.t -> pos_builder:PB.t -> T.t -> (T.t * Pos.t -> unit) -> unit
val all_eligible_subterms : ord:Ordering.t -> pos_builder:PB.t -> T.t -> (T.t * Pos.t -> unit) -> unit

val from_string : ord:Ordering.t -> string -> t
(** selection function from string (may fail) *)

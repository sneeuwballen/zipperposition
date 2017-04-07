
(* This file is free software, part of Logtk. See file "license" for more details. *)

(** {1 Very Simple AST}

    AST that holds Horn Clauses and type declarations, nothing more. *)

open Logtk

type term = STerm.t

type t =
  | Clause of term * term list
  | Type of string * term

type location = ParseLocation.t

include Interfaces.PRINT with type t := t

module Seq : sig
  val terms : t -> term Sequence.t
  val vars : t -> STerm.var Sequence.t
end

val app_infix : ?loc:location -> string -> term -> term -> term

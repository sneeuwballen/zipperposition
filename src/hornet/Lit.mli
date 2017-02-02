
(* This file is free software, part of Zipperposition. See file "license" for more details. *)

(** {1 Literal} *)

(** Literals occurring in clauses *)

open Libzipperposition

type term = FOTerm.t

type t = private
  | Bool of bool
  | Atom of term * bool

val true_ : t
val false_: t
val atom : ?sign:bool -> term -> t

val sign : t -> bool

include Interfaces.EQ with type t := t
include Interfaces.HASH with type t := t
include Interfaces.PRINT with type t := t


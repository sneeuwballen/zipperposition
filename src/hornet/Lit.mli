
(* This file is free software, part of Zipperposition. See file "license" for more details. *)

(** {1 Literal} *)

(** Literals occurring in clauses *)

open Libzipperposition

type ty = Type.t
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
include Interfaces.ORD with type t := t
include Interfaces.PRINT with type t := t

val vars_seq : t -> ty HVar.t Sequence.t
val vars_list : t -> ty HVar.t list
val vars_set : t -> ty HVar.t list (** unique *)

val weight : t -> int

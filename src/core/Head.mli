
(* This file is free software, part of Zipperposition. See file "license" for more details. *)

(** {1 Head} *)

type var = Type.t HVar.t

type t =
  | I of ID.t
  | B of Builtin.t
  | V of var

val term_to_head : Term.t -> t option
(** Return the head of a term if it can be expressed by Head.t *)

val term_to_args : Term.t -> Term.t list
(** Return the arguments of a term (removes type arguments) *)

include Interfaces.PRINT with type t := t

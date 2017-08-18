
(* This file is free software, part of Zipperposition. See file "license" for more details. *)

(** {1 Head} *)

type var = Type.t HVar.t

type t =
  | I of ID.t
  | B of Builtin.t
  | V of var

include Interfaces.PRINT with type t := t

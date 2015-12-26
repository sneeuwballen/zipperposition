
(* This file is free software, part of Zipperposition. See file "license" for more details. *)

(** {1 ID or Builtin} *)

type t =
  | I of ID.t
  | B of Builtin.t

include Interfaces.EQ with type t := t
include Interfaces.ORD with type t := t
include Interfaces.PRINT with type t := t


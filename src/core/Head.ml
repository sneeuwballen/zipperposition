
(* This file is free software, part of Zipperposition. See file "license" for more details. *)

(** {1 Head} *)

type var = Type.t HVar.t

type t =
  | I of ID.t
  | B of Builtin.t
  | V of var

let pp out = function
  | I id -> ID.pp out id
  | B b -> Builtin.pp out b
  | V x -> HVar.pp out x

let to_string = CCFormat.to_string pp


(* This file is free software, part of Zipperposition. See file "license" for more details. *)

(** {1 Boolean Trail} *)

open Hornet_types

type t = bool_trail

val is_empty : t -> bool

val make : bool_lit lazy_t list -> t

val merge : t -> t -> t

val is_absurd : t -> bool

include Interfaces.PRINT with type t := t
include Interfaces.EQ with type t := t

val pp_opt : t CCFormat.printer

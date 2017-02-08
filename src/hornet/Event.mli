
(* This file is free software, part of Zipperposition. See file "license" for more details. *)

type t = Hornet_types.event

include Interfaces.PRINT with type t := t


(* This file is free software, part of Zipperposition. See file "license" for more details. *)

open Hornet_types

type t = event

let pp = Hornet_types_util.pp_event
let to_string = CCFormat.to_string pp

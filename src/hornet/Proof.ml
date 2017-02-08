
(* This file is free software, part of Zipperposition. See file "license" for more details. *)

(** {1 CProofs} *)

open Hornet_types

module Fmt = CCFormat

type t = Hornet_types.proof
type clause = Hornet_types.clause
type bool_atom = Hornet_types.bool_atom

let from_stmt st = P_from_stmt st
let instance c subst = P_instance (c,subst)
let avatar_split c = P_avatar_split c
let split c = P_split c

let pp = Hornet_types_util.pp_proof
let to_string = Fmt.to_string pp

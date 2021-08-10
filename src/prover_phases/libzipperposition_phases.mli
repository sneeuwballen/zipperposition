
(* This file is free software, part of Zipperposition. See file "license" for more details. *)

module Phases = Phases
module Phases_impl = Phases_impl

val main_cli :
  ?setup_gc:bool ->
  unit ->
  Phases.errcode Phases.or_error

val main :
  ?setup_gc:bool ->
  ?params:Libzipperposition.Params.t ->
  string ->
  Phases.errcode Phases.or_error


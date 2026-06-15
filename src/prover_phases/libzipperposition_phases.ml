(* This file is free software, part of Zipperposition. See file "license" for more details. *)

module Phases = Phases
module Phases_impl = Phases_impl

let main_cli ?setup_gc () =
  match Phases.run (Phases_impl.main_cli ?setup_gc ()) with
  | Ok (_, n) -> Ok n
  | Error e -> Error e

let main ?setup_gc ?params file =
  match Phases.run (Phases_impl.main ?setup_gc ?params file) with
  | Ok (_, n) -> Ok n
  | Error e -> Error e

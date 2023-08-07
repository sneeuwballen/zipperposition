
(* This file is free software, part of Zipperposition. See file "license" for more details. *)

module Phases = Phases
module Phases_impl = Phases_impl

let main_cli ?setup_gc () =
  Phases.run (Phases_impl.main_cli ?setup_gc ())
  |> CCResult.map snd

let main ?setup_gc ?params file =
  Phases.run (Phases_impl.main ?setup_gc ?params file)
  |> CCResult.map snd


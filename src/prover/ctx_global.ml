(* This file is free software, part of Zipperposition. See file "license" for more details. *)

(** {1 Global conversion hooks for Ctx}

    These are global hooks shared across all contexts (i.e., all environments).
    They are stored separately from {!Ctx} to avoid circular dependencies. *)

open Logtk

let from_hooks : Literal.Conv.hook_from list Atomic.t = Atomic.make []
let to_hooks : Literal.Conv.hook_to list Atomic.t = Atomic.make []

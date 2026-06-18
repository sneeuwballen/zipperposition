(* This file is free software, part of Zipperposition. See file "license" for more details. *)

open Logtk
(** {1 Predicate Elimination} *)

open Libzipperposition

val k_enabled : bool Logtk.Flex_state.key
val setup : ?in_fp_mode:bool -> unit -> unit
val begin_fixpoint : unit -> unit
val fixpoint_step : unit -> bool
val end_fixpoint : unit -> unit
val extension : Extensions.t

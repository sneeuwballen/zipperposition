(* This file is free software, part of Zipperposition. See file "license" for more details. *)

open Logtk
(** {1 Predicate Elimination} *)

open Libzipperposition

val k_enabled : bool Logtk.Flex_state.key
val setup : ?in_fp_mode:bool -> Env.t -> unit
val begin_fixpoint : Env.t -> unit
val fixpoint_step : Env.t -> bool
val end_fixpoint : Env.t -> unit
val extension : Extensions.t

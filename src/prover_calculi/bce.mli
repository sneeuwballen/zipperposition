(* This file is free software, part of Zipperposition. See file "license" for more details. *)

open Logtk
(** {1 Blocked Clause Elimination} *)

open Libzipperposition

val k_enabled : bool Logtk.Flex_state.key

module type S = sig
  module Env : Env.S

  val setup : ?in_fp_mode:bool -> unit -> unit
  val begin_fixpoint : unit -> unit
  val fixpoint_step : unit -> bool
  val end_fixpoint : unit -> unit
end

module Make (E : Env.S) : S with module Env = E

val extension : Extensions.t

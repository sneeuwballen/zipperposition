
(* This file is free software, part of Zipperposition. See file "license" for more details. *)

(** {1 Blocked Clause Elimination} *)
open Logtk
open Libzipperposition

module type S = sig
  module Env : Env.S
  val setup : unit -> unit
  val begin_fixpoint : unit -> unit
  val fixpoint_step : unit -> bool
  val end_fixpoint : unit -> unit
end

module Make(E : Env.S) : S with module Env = E

val extension : Extensions.t

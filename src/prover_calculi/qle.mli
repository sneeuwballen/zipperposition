(* This file is free software, part of Zipperposition. See file "license" for more details. *)

open Logtk
(** {1 Quasipure Literal Elimination} *)

open Libzipperposition

module type S = sig
  module Env : Env.S

  val setup : unit -> unit
end

module Make (E : Env.S) : S with module Env = E

val extension : Extensions.t


(* This file is free software, part of Zipperposition. See file "license" for more details. *)

(** {1 Booleans} *)
open Libzipperposition

val enabled : bool ref

module type S = sig
  module Env : Env.S
  module C : module type of Env.C

  (** {6 Registration} *)

  val setup : unit -> unit
  (** Register rules in the environment *)


end

module Make(E : Env.S) : S with module Env = E

val extension : Extensions.t

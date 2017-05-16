
(* This file is free software, part of Zipperposition. See file "license" for more details. *)

(** See {!Purify} *)

module type S = sig
  module Env : Env.S

  val setup : unit -> unit
  (** Register rules in the environment *)
end

module Make(E : Env.S) : S with module Env = E

val extension : Extensions.t


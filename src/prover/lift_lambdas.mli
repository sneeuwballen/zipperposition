(* This file is free software, part of Zipperposition. See file "license" for more details. *)

module type S = sig
  module Env : Env.S
  module C : module type of Env.C

  (** {5 Registration} *)

  val setup : unit -> unit
  (** Register rules in the environment *)

  val lift_lambdas : Env.C.t -> Env.C.t list
end

module Make (E : Env.S) : S with module Env = E

val extension : Extensions.t

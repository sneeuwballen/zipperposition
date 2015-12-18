
(* This file is free software, part of Zipperposition. See file "license" for more details. *)

(** {1 Induction through Cut} *)

module type S = sig
  module Env : Env.S
  module Ctx : module type of Env.Ctx

  val register : unit -> unit
end

module Make(E: Env.S)(Solver : Sat_solver.S) :
  S with module Env = E
     and module Ctx = E.Ctx

val extension : Extensions.t

open Libzipperposition
open Logtk

module type S = sig
  module Env : Env.S
  module C : module type of Env.C

  (** {6 Registration} *)

  val setup : unit -> unit
  (** Register rules in the environment *)
end

module Make(E : Env.S) : S with module Env = E

val k_enable_combinators : bool Logtk.Flex_state.key
val extension : Extensions.t
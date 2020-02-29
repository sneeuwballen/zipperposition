open Extensions
open Logtk

module type S = sig
  module Env : Env.S
  module C : module type of Env.C

  (** {6 Registration} *)

  val setup : unit -> unit
  (* Converts lambdas to combinators 
     if combinator reasoning is enabled. *)
  val maybe_conv_lams : Env.C.t -> Env.C.t

  (** Register rules in the environment *)
end

val comb_normalize : Term.t -> Term.t option

module Make(E : Env.S) : S with module Env = E

val k_enable_combinators : bool Logtk.Flex_state.key
val extension : Extensions.t
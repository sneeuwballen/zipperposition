open Logtk

module type S = sig
  module Env : Env.S
  module C : module type of Env.C

  (** {5 Registration} *)

  val setup : unit -> unit

  (* Converts lambdas to combinators
     if combinator reasoning is enabled. *)
  val maybe_conv_lams : Env.C.t -> Env.C.t

  (* Converts lambdas in either case *)
  val force_conv_lams : Env.C.t -> Env.C.t

  (* Expands the term to be of the form
      \lambda (all type vars). body of prop type *)
  val expand : Term.t -> Term.t

  (** Register rules in the environment *)
end

module Make (E : Env.S) : S with module Env = E

val k_enable_combinators : bool Logtk.Flex_state.key
val extension : Extensions.t

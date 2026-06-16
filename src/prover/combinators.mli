open Logtk

module type S = sig
  module E : Env.S
  module C : module type of E.C

  (** {5 Registration} *)

  val setup : Env.t -> unit

  (* Converts lambdas to combinators 
     if combinator reasoning is enabled. *)
  val maybe_conv_lams : E.C.t -> E.C.t

  (* Converts lambdas in either case *)
  val force_conv_lams : E.C.t -> E.C.t

  (* Expands the term to be of the form 
      \lambda (all type vars). body of prop type *)
  val expand : Term.t -> Term.t

  (** Register rules in the environment *)
end

module Make (E : Env.S) : S with module E = E

val k_enable_combinators : bool Logtk.Flex_state.key
val extension : Extensions.t

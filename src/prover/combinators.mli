open Logtk

val setup : Env.t -> unit

(* Converts lambdas to combinators 
   if combinator reasoning is enabled. *)
val maybe_conv_lams : Clause.t -> Clause.t

(* Converts lambdas in either case *)
val force_conv_lams : Clause.t -> Clause.t

(* Expands the term to be of the form 
    \lambda (all type vars). body of prop type *)
val expand : Term.t -> Term.t

(** Register rules in the environment *)

val k_enable_combinators : bool Flex_state.key
val extension : Extensions.t

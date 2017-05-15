
(* This file is free software, part of Zipperposition. See file "license" for more details. *)

(** {1 Higher-Order Unification} *)

(** Use combinators for higher-order unification and reduction.
    We follow "Higher-order unification via combinators", Dougherty.
*)

type term = Term.t

type penalty = int
(** penalty on the search space *)

(** {2 Set of Combinators} *)

module Combinators : sig
  type rule = Rewrite.Term.rule * penalty
  (** A rule is a term rewrite rule, plus a penalty on the search space *)

  type rules = rule list

  type t
  (** Set of combinators *)

  val rules : t -> rules
  val name : t -> string
  val decls : t -> (ID.t * Type.t) list

  val ski : t
  (** basic set *)

  val default : t

  val by_name : string -> t

  val list_names : unit -> string list
end

val unif_step :
  Combinators.t Scoped.t ->
  (term * term) Scoped.t ->
  (Subst.t * penalty) list
(** [unif_step c (t,u)] returns a set of possible steps
    for unifying [t] and [u] using combinators [c] *)


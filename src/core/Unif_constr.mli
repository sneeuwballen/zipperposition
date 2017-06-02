
(* This file is free software, part of Zipperposition. See file "license" for more details. *)

(** {1 Unification Constraint} *)

type term = InnerTerm.t

(** A constraint delayed because unification for this pair of terms is
      not syntactic *)
type t = private {
  t1: term;
  sc1: Scoped.scope;
  t2: term;
  sc2: Scoped.scope;
}

val make : term Scoped.t -> term Scoped.t -> t

(** Apply a substitution to a delayed constraint *)
val apply_subst :
  renaming:Subst.Renaming.t ->
  Subst.t ->
  t ->
  term * term

(** Apply a substitution to delayed constraints *)
val apply_subst_l :
  renaming:Subst.Renaming.t ->
  Subst.t ->
  t list ->
  (term * term) list

include Interfaces.HASH with type t := t
include Interfaces.ORD with type t := t
include Interfaces.PRINT with type t := t


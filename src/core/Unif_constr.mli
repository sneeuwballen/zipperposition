
(* This file is free software, part of Zipperposition. See file "license" for more details. *)

(** {1 Unification Constraint} *)

(** A constraint is a pair of (scoped) terms that cannot be unified
    immediately (because they belong to some theory, for example).

    We keep them in a separate constraint that will become a negative
    literal [t ≠ u], on which the theory can then act.
*)

type term = InnerTerm.t

(** A constraint delayed because unification for this pair of terms is
      not syntactic *)
type t = private {
  t1: term;
  sc1: Scoped.scope;
  t2: term;
  sc2: Scoped.scope;
  tags: Proof.tag list;
}

val make : tags:Proof.tag list -> term Scoped.t -> term Scoped.t -> t

val tags : t -> Proof.tag list

(** Apply a substitution to a delayed constraint *)
val apply_subst :
  Subst.Renaming.t ->
  Subst.t ->
  t ->
  term * term

(** Apply a substitution to delayed constraints *)
val apply_subst_l :
  Subst.Renaming.t ->
  Subst.t ->
  t list ->
  (term * term) list

module FO : sig
  val make : tags:Proof.tag list -> Term.t Scoped.t -> Term.t Scoped.t -> t
end

include Interfaces.HASH with type t := t
include Interfaces.ORD with type t := t
include Interfaces.PRINT with type t := t


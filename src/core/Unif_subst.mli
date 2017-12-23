
(* This file is free software, part of Zipperposition. See file "license" for more details. *)

(** {1 Unification Substitution} *)

(** A tuple for full-unification, containing:

    - the substitution itself
    - delayed constraints (if any)
*)

type term = Subst.term
type var = Subst.var

type t

val empty : t
(** Empty *)

val is_empty : t -> bool
(** Both substitution and constraints are empty *)

val subst : t -> Subst.t
(** Substitution *)

val constr_l : t -> Unif_constr.t list
(** Constraints *)

val constr_l_subst : Subst.Renaming.t -> t -> (term*term) list
(** Apply the substitution to the constraint *)

val tags : t -> Proof.tag list

val has_constr : t -> bool
(** Is there any constraint? *)

val make : Subst.t -> Unif_constr.t list -> t

val of_subst : Subst.t -> t

val map_subst : f:(Subst.t -> Subst.t) -> t -> t

val add_constr : Unif_constr.t -> t -> t

val deref : t -> term Scoped.t -> term Scoped.t

val bind : t -> var Scoped.t -> term Scoped.t -> t

val update : t -> var Scoped.t -> term Scoped.t -> t

val mem : t -> var Scoped.t -> bool

module FO : sig
  val bind : t -> Type.t HVar.t Scoped.t -> Term.t Scoped.t -> t
  val mem : t -> Type.t HVar.t Scoped.t -> bool
  val deref : t -> Term.t Scoped.t -> Term.t Scoped.t
end

include Interfaces.HASH with type t := t
include Interfaces.ORD with type t := t
include Interfaces.PRINT with type t := t

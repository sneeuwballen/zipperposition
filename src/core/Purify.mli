
(* This file is free software, part of Zipperposition. See file "license" for more details. *)

(** {1 Purification} *)

(** A rule to replace subterms [t] that have a type for which equality is not
    syntactic, by a fresh variable [X], adding a constraint [X != t].
    This allows unification to proceed as usual and then the constraint [X != t]
    to act as a later constraint.
*)

type term = Term.t

val type_is_purifiable : Type.t -> bool
(** Should terms of this type be purified? *)

val is_value : term -> bool
(** Is this term a value of its (purifiable) type?
    If this is the case, we should keep the term as is.

    invariant: distinct terms that are values should be distinct
    in every model. *)

val purify : Literals.t -> Literals.t option
(** Purify clauses by replacing arithmetic expressions occurring
    under terms by variables, and adding constraints *)

val is_shielded : Type.t HVar.t -> Literals.t -> bool
(** Is this variable shielded in this clause? *)

val unshielded_vars : Literals.t -> Type.t HVar.t list
(** Set of variables occurring unshielded *)

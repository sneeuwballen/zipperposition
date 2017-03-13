
(* This file is free software, part of Zipperposition. See file "license" for more details. *)

(** {1 Universally Quantified Conjunction of Clauses} *)

open Logtk

type var = FOTerm.var
type term = FOTerm.t

(** A formula of the form [forall vars. \bigand_i C_i].
    The [C_i] are clauses with free variables in [vars] *)
type t = private {
  vars: FOTerm.VarSet.t;
  cs: Literals.t list;
}

val make : Literals.t list -> t
val trivial : t

include Interfaces.HASH with type t := t
include Interfaces.ORD with type t := t
include Interfaces.PRINT with type t := t
val pp_tstp : t CCFormat.printer

val vars : t -> FOTerm.VarSet.t
val cs : t -> Literals.t list

val ind_vars : t -> var list
(** subset of {!vars} that have an inductive type *)

val subst1 : var -> term -> t -> t
(** Substitution of one variable *)

val apply_subst : renaming:Subst.Renaming.t -> Subst.t -> t Scoped.t -> t

val are_variant : t -> t -> bool
(** Are these two cut formulas alpha-equivalent? *)


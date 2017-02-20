
(* This file is free software, part of Zipperposition. See file "license" for more details. *)

(** {1 Dismatching Constraint} *)

(** A constraint that specifies that a list of terms [t1, …, tn]
    must not match patterns [u1, …, un].
    Variables in the [u_i] live in a distinct scope than variables
    in the [t_i]. *)

open Libzipperposition

type term = FOTerm.t

type t

val empty : t
(** Trivial constraint *)

val is_empty : t -> bool
(** Is the constraint empty? *)

val make : (term * term) list -> t
(** [make [t_1,u_1; …; t_n,u_n]]
    makes a dismatching constraint that is satisfied for every
    ground substitution [sigma] such that at least one [t_i\sigma] does not
    match the pattern [u_i]. *)

val combine : t -> t -> t
(** Conjunction of the two constraints.
    {!empty} is neutral for this operation. *)

val apply_subst :
  renaming:Subst.Renaming.t ->
  Subst.t ->
  t Scoped.t ->
  t
(** Apply a substitution [sigma] to the constraints. The constraint
    might become trivial as a result. *)

val is_trivial : t -> bool
(** Is the constraint trivially satisfied? (i.e. always true).
    That happens, for instance, for constraints such as [f x /< g y] *)

val is_absurd : t -> bool
(** Is the constraint never satisfied? (i.e. necessarily false).
    That happens if all RHS match their LHS already
    (will still hold for every instance). *)

val is_absurd_with: Subst.t -> t Scoped.t -> bool
(** No solution compatible with this subst *)

include Interfaces.ORD with type t := t
include Interfaces.PRINT with type t := t

val vars_seq : t -> Type.t HVar.t Sequence.t

val vars_l : t -> Type.t HVar.t list

val variant :
  ?subst:Subst.t ->
  t Scoped.t ->
  t Scoped.t ->
  Subst.t Sequence.t
(** Find substitutions for which these two constraints are variant *)

val matching :
  ?subst:Subst.t ->
  t Scoped.t ->
  t Scoped.t ->
  Subst.t Sequence.t
(** Find substitutions for which the first constraint subsumes
    the second one.
    It means that every instance accepted by the second will be an
    instance accepted by the first. *)

val are_variant : t -> t -> bool

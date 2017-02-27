
(* This file is free software, part of Zipperposition. See file "license" for more details. *)

(** {1 Labelled Clause} *)

open Logtk
open Hornet_types

type t = labelled_clause

val make_empty : clause -> select_lit -> t
(** Initial empty label for this clause *)

include Interfaces.PRINT with type t := t
include Interfaces.HASH with type t := t
include Interfaces.ORD with type t := t

val same_clause : t -> t -> bool
(** Two labels for the same clause? *)

val apply_subst :
  renaming:Subst.Renaming.t ->
  Subst.t ->
  t Scoped.t ->
  t
(** Apply the substitution to each variable in the given scope *)

val hash_mod_alpha : t -> int

val is_empty : t -> bool
(** Is the substitution trivial? (i.e. a renaming) *)

val has_no_ground_instance : t -> bool
(** The constraints attached to the clause are not compatible
    with the current substitution, meaning that the labelled clause
    represents 0 ground clauses *)

val to_subst : t -> Subst.t
(** Build a substitution (all terms have scope 0) *)

val to_dismatch : t -> Dismatching_constr.t
(** Build a dismatching constraints that excludes precisely the
    current substitution *)

val variant :
  ?subst:Subst.t ->
  t Scoped.t ->
  t Scoped.t ->
  Subst.t Sequence.t

val matching :
  ?subst:Subst.t ->
  t Scoped.t ->
  t Scoped.t ->
  Subst.t Sequence.t

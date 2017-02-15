
(* This file is free software, part of Zipperposition. See file "license" for more details. *)

(** {1 Labelled Clause} *)

open Libzipperposition
open Hornet_types

type t = labelled_clause

val make_empty : clause -> select_lit -> t
(** Initial empty label for this clause *)

include Interfaces.PRINT with type t := t
include Interfaces.HASH with type t := t
include Interfaces.ORD with type t := t

val apply_subst :
  renaming:Subst.Renaming.t ->
  Subst.t ->
  t Scoped.t ->
  t
(** Apply the substitution to each variable in the given scope *)

val is_empty : t -> bool
(** Is the substitution trivial? *)

val to_subst : t -> Subst.t
(** Build a substitution (all terms have scope 0) *)

val to_dismatch : t -> Dismatching_constr.t
(** Build a dismatching constraints that excludes precisely the
    current substitution *)


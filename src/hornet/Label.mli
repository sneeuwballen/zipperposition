
(* This file is free software, part of Zipperposition. See file "license" for more details. *)

(** {1 Label for Horn Clauses} *)

open Logtk
open Hornet_types

type t = Hornet_types.label
(** Set of labelled clauses. Invariant: sorted *)

include Interfaces.PRINT with type t := t
include Interfaces.HASH with type t := t

val hash_mod_alpha : t -> int

val return : labelled_clause -> t

val make : labelled_clause list -> t

val is_empty : t -> bool
(** Empty set of labels *)

val all_empty : t -> bool
(** All labelled clauses have empty labels. See {!Labelled_clause.is_empty} *)

val has_no_ground_instance : t -> bool
(** Some labelled clause has unsatisfiable constraints *)

val apply_subst :
  renaming:Subst.Renaming.t ->
  Subst.t ->
  t Scoped.t ->
  t

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
(** Substitution that make these the first label imply the second *)

val subsumes :
  ?subst:Subst.t ->
  t Scoped.t ->
  t Scoped.t ->
  Subst.t Sequence.t

val subsumes_pred :
  ?subst:Subst.t ->
  t Scoped.t ->
  t Scoped.t ->
  bool

val merge : t -> t -> t

val to_list : t -> labelled_clause list
val to_seq: t -> labelled_clause Sequence.t


(* This file is free software, part of Zipperposition. See file "license" for more details. *)

(** {1 Label for Horn Clauses} *)

open Libzipperposition
open Hornet_types

type t = labelled_clause list
(** Set of labelled clauses. Invariant: sorted *)

val return : labelled_clause -> t

val make : labelled_clause list -> t

val apply_subst :
  renaming:Subst.Renaming.t ->
  Subst.t ->
  t Scoped.t ->
  t

val merge : t -> t -> t

include Interfaces.PRINT with type t := t

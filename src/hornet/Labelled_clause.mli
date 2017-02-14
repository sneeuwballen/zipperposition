
(* This file is free software, part of Zipperposition. See file "license" for more details. *)

(** {1 Labelled Clause} *)

open Libzipperposition
open Hornet_types

type t = labelled_clause

val make_empty : clause -> t
(** Initial empty label for this clause *)

include Interfaces.PRINT with type t := t
include Interfaces.HASH with type t := t
include Interfaces.ORD with type t := t

val apply_subst :
  renaming:Subst.Renaming.t ->
  Subst.t ->
  t Scoped.t ->
  t

val to_subst : t -> Subst.t
(** Build a substitution (all terms have scope 0) *)



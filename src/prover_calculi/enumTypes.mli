(* This file is free software, part of Zipperposition. See file "license" for more details. *)

(** {1 Inference and simplification rules for "Enum Types"} *)

open Logtk
open Libzipperposition

type term = Term.t

exception Error of string

type decl

val pp_decl : decl CCFormat.printer

type declare_result =
  | New of decl
  | AlreadyDeclared of decl

val declare_ty :
  proof:Proof.t ->
  ty_id:Name.t ->
  ty_vars:Type.t HVar.t list ->
  var:Type.t HVar.t ->
  term list ->
  declare_result

val instantiate_vars : Env.multi_simpl_rule
val setup : Env.t -> unit
val extension : Extensions.t

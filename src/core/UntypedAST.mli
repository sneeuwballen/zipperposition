
(* This file is free software, part of Zipperposition. See file "license" for more details. *)

(** {1 Main AST before Typing}

    This AST should be output by parsers. *)

module Loc = ParseLocation
module T = STerm

type term = T.t
type ty = T.t
type form = T.t

(** Basic definition of inductive types *)
type data = {
  data_name: string;
  data_vars: string list;
  data_cstors: (string * ty list) list;
}

(** Attributes for assertions *)
type attrs = {
  name: string option;
}

(** Statement *)
type statement_view =
  | Decl of string * ty
  | Def of string * ty * term
  | Data of data list
  | Assert of form
  | Goal of form

type statement = {
  stmt: statement_view;
  attrs: attrs;
  loc: Loc.t option;
}

val default_attrs : attrs

val decl : ?loc:Loc.t -> string -> ty -> statement
val def : ?loc:Loc.t -> string -> ty -> term -> statement
val data : ?loc:Loc.t -> data list -> statement
val assert_ : ?loc:Loc.t -> ?attrs:attrs -> term -> statement
val goal : ?loc:Loc.t -> ?attrs:attrs -> term -> statement

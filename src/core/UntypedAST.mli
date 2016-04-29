
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

(** Attributes *)
type attr =
  | A_name of string
  | A_AC

type attrs = attr list

(** Statement *)
type statement_view =
  | Decl of string * ty
  | Def of string * ty * term
  | Rewrite of term
  | Data of data list
  | Assert of form
  | Goal of form

type statement = {
  stmt: statement_view;
  attrs: attrs;
  loc: Loc.t option;
}

val default_attrs : attrs

val decl : ?loc:Loc.t -> ?attrs:attrs -> string -> ty -> statement
val def : ?loc:Loc.t -> ?attrs:attrs -> string -> ty -> term -> statement
val data : ?loc:Loc.t -> ?attrs:attrs -> data list -> statement
val rewrite : ?loc:Loc.t -> ?attrs:attrs -> term -> statement
val assert_ : ?loc:Loc.t -> ?attrs:attrs -> term -> statement
val goal : ?loc:Loc.t -> ?attrs:attrs -> term -> statement

val name_of_attrs : attrs -> string option
val name : statement -> string option

val pp_attr : attr CCFormat.printer
val pp_attrs : attrs CCFormat.printer

val pp_statement : statement CCFormat.printer

(** {2 Errors} *)

exception Parse_error of Loc.t * string

val error : Loc.t -> string -> 'a
val errorf : Loc.t -> ('a, Format.formatter, unit, 'b) format4 -> 'a

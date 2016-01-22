
(* This file is free software, part of Zipperposition. See file "license" for more details. *)

(** {1 Main AST before Typing} *)

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

let default_attrs = {name=None; }

let make_ ?loc ?(attrs=default_attrs) stmt = {loc; stmt; attrs; }

let decl ?loc n ty = make_ ?loc (Decl (n,ty))
let def ?loc n ty t = make_ ?loc (Def (n,ty,t))
let data ?loc l = make_ ?loc (Data l)
let assert_ ?loc ?attrs t = make_ ?attrs ?loc (Assert t)
let goal ?loc ?attrs t = make_ ?attrs ?loc (Goal t)




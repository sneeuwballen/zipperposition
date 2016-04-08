
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
  | Rewrite of term
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
let rewrite ?loc ?attrs t = make_ ?loc ?attrs (Rewrite t)
let data ?loc l = make_ ?loc (Data l)
let assert_ ?loc ?attrs t = make_ ?attrs ?loc (Assert t)
let goal ?loc ?attrs t = make_ ?attrs ?loc (Goal t)

let pp_statement out st =
  let fpf = Format.fprintf in
  match st.stmt with
  | Decl (id,ty) ->
      fpf out "@[<2>val %s :@ @[%a@]@]." id T.pp ty
  | Def (id,ty,t) ->
      fpf out "@[<2>def %s :@ @[%a@]@ := @[%a@]@]." id T.pp ty T.pp t
  | Rewrite t ->
      fpf out "@[<2>rewrite @[%a@]." T.pp t
  | Data l ->
      let pp_cstor out (id,args) =
        fpf out "@[<2>| @[%s@ %a@]@]" id (Util.pp_list ~sep:" " T.pp) args in
      let pp_data out d =
        fpf out "@[%s %a@] :=@ @[<v>%a@]"
          d.data_name
          (Util.pp_list ~sep:" " CCFormat.string) d.data_vars
          (Util.pp_list ~sep:"" pp_cstor) d.data_cstors
      in
      fpf out "@[<v>data %a@]" (Util.pp_list ~sep:" and " pp_data) l
  | Assert f ->
      fpf out "@[<2>assert@ @[%a@]@]." T.pp f
  | Goal f ->
      fpf out "@[<2>goal@ @[%a@]@]." T.pp f

(** {2 Errors} *)

exception Parse_error of Loc.t * string

let () = Printexc.register_printer
  (function
    | Parse_error (loc, msg) ->
        Some
          (CCFormat.sprintf "@[<2>parse error:@ @[%s@]@ %a@]" msg Loc.pp loc)
    | _ -> None)

let error loc msg = raise (Parse_error (loc,msg))
let errorf loc msg = CCFormat.ksprintf msg ~f:(error loc)

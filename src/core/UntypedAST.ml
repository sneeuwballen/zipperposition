
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

(** Attributes *)
type attr =
  | A_name of string
  | A_AC

type attrs = attr list

(** Statement *)
type statement_view =
  | Include of string
  | Decl of string * ty
  | Def of string * ty * term
  | Rewrite of term
  | Data of data list
  | Assert of form
  | Lemma of form
  | Goal of form

type statement = {
  stmt: statement_view;
  attrs: attrs;
  loc: Loc.t option;
}

let default_attrs = []

let make_ ?loc ?(attrs=default_attrs) stmt = {loc; stmt; attrs; }

let include_ ?loc ?attrs s = make_ ?loc ?attrs (Include s)
let decl ?loc ?attrs n ty = make_ ?loc ?attrs (Decl (n,ty))
let def ?loc ?attrs n ty t = make_ ?loc ?attrs (Def (n,ty,t))
let rewrite ?loc ?attrs t = make_ ?loc ?attrs (Rewrite t)
let data ?loc ?attrs l = make_ ?loc ?attrs (Data l)
let assert_ ?loc ?attrs t = make_ ?attrs ?loc (Assert t)
let lemma ?loc ?attrs t = make_ ?attrs ?loc (Lemma t)
let goal ?loc ?attrs t = make_ ?attrs ?loc (Goal t)

let pp_attr out = function
  | A_name n -> Format.fprintf out "name:%s" n
  | A_AC -> CCFormat.string out "AC"

let pp_attrs out = function
  | [] -> ()
  | l -> Format.fprintf out "@ [@[%a@]]" (Util.pp_list ~sep:", " pp_attr) l

let name_of_attrs =
  CCList.find_map
    (function A_name n -> Some n | _ -> None)
let name st = name_of_attrs st.attrs

let pp_statement out st =
  let attrs = st.attrs in
  let fpf = Format.fprintf in
  match st.stmt with
  | Include s ->
      fpf out "@[<2>include \"%s\"@]@." (String.escaped s)
  | Decl (id,ty) ->
      fpf out "@[<2>val%a %s :@ @[%a@]@]." pp_attrs attrs id T.pp ty
  | Def (id,ty,t) ->
      fpf out "@[<2>def%a %s :@ @[%a@]@ := @[%a@]@]." pp_attrs attrs id T.pp ty T.pp t
  | Rewrite t ->
      fpf out "@[<2>rewrite%a @[%a@]@]." pp_attrs attrs T.pp t
  | Data l ->
      let pp_cstor out (id,args) =
        fpf out "@[<2>| @[%s@ %a@]@]" id (Util.pp_list ~sep:" " T.pp) args in
      let pp_data out d =
        fpf out "@[%s %a@] :=@ @[<v>%a@]"
          d.data_name
          (Util.pp_list ~sep:" " CCFormat.string) d.data_vars
          (Util.pp_list ~sep:"" pp_cstor) d.data_cstors
      in
      fpf out "@[<v>data%a@ %a@]." pp_attrs attrs (Util.pp_list ~sep:" and " pp_data) l
  | Assert f ->
      fpf out "@[<2>assert%a@ @[%a@]@]." pp_attrs attrs T.pp f
  | Lemma f ->
      fpf out "@[<2>lemma%a@ @[%a@]@]." pp_attrs attrs T.pp f
  | Goal f ->
      fpf out "@[<2>goal%a@ @[%a@]@]." pp_attrs attrs T.pp f

(** {2 Errors} *)

exception Parse_error of Loc.t * string

let () = Printexc.register_printer
  (function
    | Parse_error (loc, msg) ->
        Some
          (CCFormat.sprintf "@[<4>parse error:@ @[%s@]@ at %a@]" msg Loc.pp loc)
    | _ -> None)

let error loc msg = raise (Parse_error (loc,msg))
let errorf loc msg = CCFormat.ksprintf msg ~f:(error loc)

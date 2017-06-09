
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
  data_cstors: (string * (string option * ty) list) list;
  (* list of constructor. Each constructor is paired with a list of
     arguments, that is, an optional projector + the type *)
}

(** Attributes (general terms) *)
type attr =
  | A_app of string * attr list
  | A_quoted of string
  | A_list of attr list

type attrs = attr list

type def = {
  def_id: string;
  def_ty: ty;
  def_rules: term list;
}

(** Statement *)
type statement_view =
  | Include of string
  | Decl of string * ty
  | Def of def list
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

module A = struct
  type t = attr
  let str s = A_app (s,[])
  let app s l = A_app (s,l)
  let quoted s = A_quoted s
  let list l = A_list l
end

let name_of_attrs =
  CCList.find_map
    (function A_app ("name", [A_quoted n]) -> Some n | _ -> None)

let attr_name n = A.app "name" [A.str n]
let attr_ac = A.str "ac"
let attr_prefix s = A.app "prefix" [A.quoted s]
let attr_infix s = A.app "infix" [A.quoted s]

let make_ ?loc ?(attrs=default_attrs) stmt = {loc; stmt; attrs; }

let mk_def def_id def_ty def_rules = {def_id; def_ty; def_rules}

let include_ ?loc ?attrs s = make_ ?loc ?attrs (Include s)
let decl ?loc ?attrs f ty = make_ ?loc ?attrs (Decl (f,ty))
let def ?loc ?attrs l = make_ ?loc ?attrs (Def l)
let rewrite ?loc ?attrs t = make_ ?loc ?attrs (Rewrite t)
let data ?loc ?attrs l = make_ ?loc ?attrs (Data l)
let assert_ ?loc ?attrs t = make_ ?attrs ?loc (Assert t)
let lemma ?loc ?attrs t = make_ ?attrs ?loc (Lemma t)
let goal ?loc ?attrs t = make_ ?attrs ?loc (Goal t)

let pp_attr =
  let rec pp_attr_gen ~inner out = function
    | A_app (s,[]) -> CCFormat.string out s
    | A_app (s,l) when not inner ->
      Format.fprintf out "@[%s@ %a@]" s (Util.pp_list ~sep:" " pp_attr) l
    | A_app (s,l) ->
      Format.fprintf out "(@[%s@ %a@])" s (Util.pp_list ~sep:" " pp_attr) l
    | A_quoted s -> Format.fprintf out "\"%s\"" s
    | A_list l ->
      Format.fprintf out "[@[<hv>%a@]]" (Util.pp_list ~sep:"," pp_attr) l
  and pp_attr out = pp_attr_gen ~inner:true out in
  pp_attr_gen ~inner:false

let pp_attrs out = function
  | [] -> ()
  | l -> Format.fprintf out "@ [@[%a@]]" (Util.pp_list ~sep:", " pp_attr) l

let pp_attr_zf = pp_attr
let pp_attrs_zf = pp_attrs

let rec pp_attr_tstp out = function
  | A_app (s,[]) -> Format.fprintf out "%s()" s
  | A_app (s,l) ->
    Format.fprintf out "%s(@[%a@])" s (Util.pp_list ~sep:"," pp_attr_tstp) l
  | A_quoted s -> Format.fprintf out "'%s'" s
  | A_list l ->
    Format.fprintf out "[@[<hv>%a@]]" (Util.pp_list ~sep:"," pp_attr_tstp) l

let name st = name_of_attrs st.attrs

let pp_statement out st =
  let attrs = st.attrs in
  let fpf = Format.fprintf in
  match st.stmt with
    | Include s ->
      fpf out "@[<2>include \"%s\"@]@." (String.escaped s)
    | Decl (id,ty) ->
      fpf out "@[<2>val%a %s :@ @[%a@]@]." pp_attrs attrs id T.pp ty
    | Def l ->
      let pp_def out {def_id=id;def_ty;def_rules} =
        fpf out "@[<2>@[%s :@ %a@]@ := @[%a@]"
          id T.pp def_ty (Util.pp_list ~sep:" and " T.pp) def_rules
      in
      fpf out "@[<2>def%a %a@]."
        pp_attrs attrs (Util.pp_list ~sep:"" pp_def) l
    | Rewrite t ->
      fpf out "@[<2>rewrite%a @[%a@]@]." pp_attrs attrs T.pp t
    | Data l ->
      let pp_arg out (_,ty) = T.pp out ty in
      let pp_cstor out (id,args) =
        fpf out "@[<2>| @[%s@ %a@]@]" id (Util.pp_list ~sep:" " pp_arg) args in
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

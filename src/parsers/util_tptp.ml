
(* This file is free software, part of Libzipperposition. See file "license" for more details. *)

(** {1 Utils related to TPTP parsing} *)

open Libzipperposition

module T = TypedSTerm
module PT = STerm
module F = T.Form
module A = Ast_tptp
module Loc = ParseLocation
module Err = CCError

type 'a or_error = [`Error of string | `Ok of 'a]

type typed = T.t
type untyped = PT.t

exception Error of string

let error msg = raise (Error msg)
let errorf msg = CCFormat.ksprintf msg ~f:error

(** {2 Printing/Parsing} *)

type parse_cache = (string, unit) Hashtbl.t

let create_parse_cache () = Hashtbl.create 16

let find_file name dir =
  (* check if the file exists *)
  let file_exists name =
    try ignore (Unix.stat name); true
    with Unix.Unix_error (e, _, _) when e = Unix.ENOENT -> false
  in
  (* search in [dir], and its parents recursively *)
  let rec search dir =
    let cur_name = Filename.concat dir name in
    Util.debugf 2 "search `%s`@ as `%s`" (fun k->k name cur_name);
    if file_exists cur_name
    then Some cur_name
    else
      let dir' = Filename.dirname dir in
      if dir = dir'
      then None
      else search dir'
  in
  let search_env () =
    try
      let dir' = Sys.getenv "TPTP" in
      search dir'
    with Not_found -> None
  in
  if Filename.is_relative name
  (* search by relative path, in parent dirs *)
  then match search dir with
    | None -> search_env ()
    | Some _ as res -> res
  else if file_exists name
  then Some name  (* found *)
  else None

let parse_lexbuf ?names buf =
  try
    (* parse declarations from file *)
    let decls = Parse_tptp.parse_declarations Lex_tptp.token buf in
    let q = Queue.create () in
    List.iter
      (fun decl -> match decl, names with
         | (A.CNF _ | A.FOF _ | A.TFF _ | A.THF _ | A.TypeDecl _ | A.NewType _), None ->
             Queue.push decl q
         | (A.CNF _ | A.FOF _ | A.TFF _ | A.THF _ | A.TypeDecl _ | A.NewType _), Some names ->
             if List.mem (A.get_name decl) names
             then Queue.push decl q
             else ()   (* not included *)
         | (A.Include _ | A.IncludeOnly _), _ ->
             Queue.push decl q)
      decls;
    Err.return (Sequence.of_queue q)
  with
  | Error msg | Sys_error msg ->
      Err.fail msg
  | e ->
      Err.fail (Printexc.to_string e)

(* find file *)
let _find_and_open filename dir =
  match filename with
  | "stdin" -> stdin
  | _ ->
      match find_file filename dir with
      | Some filename ->
          begin try open_in filename
            with Sys_error msg ->
              errorf "error when opening file `%s`: %s" filename msg
          end
      | None -> errorf "could not find file `%s`" filename

let parse_file ?cache ~recursive f =
  let parse_cache =
    lazy (if recursive then CCOpt.get_lazy create_parse_cache cache
      else assert false)
  in
  let dir = Filename.dirname f in
  let result_decls = Queue.create () in
  (* function that parses one file *)
  let rec parse_this_file ?names filename =
    (* find and open file *)
    let input = _find_and_open filename dir in
    try
      let buf = Lexing.from_channel input in
      Loc.set_file buf filename;
      (* parse declarations from file *)
      let decls = Parse_tptp.parse_declarations Lex_tptp.token buf in
      List.iter
        (fun decl -> match decl, names with
           | (A.CNF _ | A.FOF _ | A.TFF _ | A.THF _ | A.TypeDecl _ | A.NewType _), None ->
               Queue.push decl result_decls
           | (A.CNF _ | A.FOF _ | A.TFF _ | A.THF _ | A.TypeDecl _ | A.NewType _), Some names ->
               if List.mem (A.get_name decl) names
               then Queue.push decl result_decls
               else ()   (* not included *)
           | A.Include f, _ when recursive ->
               if Hashtbl.mem (Lazy.force parse_cache) f
               then Util.debugf 2 "@[<2>ignore include of `%s`, already parsed@]" (fun k->k f)
               else (
                 (* be sure not to include the file twice *)
                 Hashtbl.add (Lazy.force parse_cache) f ();
                 parse_this_file ?names:None f
               )
           | A.IncludeOnly (f, names'), _ when recursive ->
               if Hashtbl.mem (Lazy.force parse_cache) f
               then Util.debugf 2 "@[<2>ignore include of `%s`, already parsed@]" (fun k->k f)
               else (
                 Util.debugf 2
                   "@[<2>caution: partial include of `%s` will not be cached"
                   (fun k->k f);
                 parse_this_file ~names:names' f
               )
           | (A.Include _ | A.IncludeOnly _), _ ->
               Queue.push decl result_decls)
        decls
    with e ->
      close_in input;
      raise e
  in
  try
    parse_this_file ?names:None f;
    Err.return (Sequence.of_queue result_decls)
  with
  | Error msg | Sys_error msg -> Err.fail msg
  | e -> Err.fail (Printexc.to_string e)

let fpf = Format.fprintf

let print_into ppt oc decls =
  fpf oc "@[<v>%a@]@?"
    (CCFormat.seq ~start:"" ~stop:"" ~sep:"" (A.pp ppt))
    decls

let print_into_file ppt file decls =
  CCIO.with_out file
    (fun oc ->
       let out = Format.formatter_of_out_channel oc in
       print_into ppt out decls;
       Format.pp_print_flush out ())

let has_includes decls =
  Sequence.exists
    (function
      | A.Include _
      | A.IncludeOnly _ -> true
      | A.FOF _
      | A.CNF _
      | A.TFF _
      | A.THF _
      | A.NewType _
      | A.TypeDecl _ -> false)
    decls

(** {2 Bridge to UntypedAST} *)

module UA = UntypedAST

let is_conjecture_ = function
  | A.R_conjecture -> true
  | _ -> false

let to_ast st =
  let conv_form name role f =
    let name = A.string_of_name name in
    let attrs = {UA.name=Some name; } in
    if is_conjecture_ role
    then UA.goal ~attrs f
    else UA.assert_ ~attrs f
  in
  match st with
  | A.Include _
  | A.IncludeOnly _ -> error "cannot convert `Include` to UntypedAST"
  | A.TypeDecl (_,s,ty,_)
  | A.NewType (_,s,ty,_) ->
      UA.decl s ty
  | A.TFF (name,role,f,_)
  | A.THF (name,role,f,_)
  | A.FOF (name,role,f,_) ->
      conv_form name role f
  | A.CNF (name,role,f,_) ->
      let f = PT.or_ f in
      conv_form name role f

(* default function for giving a name to the declaration of a symbol *)
let name_sym_ sy =
  let str = CCFormat.sprintf "'ty_decl_%s'" sy in
  A.NameString str

let of_ast st =
  let name = match st.UA.attrs.UA.name with
    | None -> A.NameString "no_name"
    | Some s -> A.NameString s
  in
  match st.UA.stmt with
  | UA.Decl (s,ty) ->
      let name = name_sym_ s in
      (* XXX we should look if [ty] returns tType or not *)
      A.TypeDecl (name, s, ty, [])
  | UA.Def (_,_,_)
  | UA.DefWhere _ -> error "cannot convert `def` statement into TPTP"
  | UA.Data _ -> error "cannot convert `data` statement into TPTP"
  | UA.Goal f -> A.TFF (name, A.R_conjecture, f, [])
  | UA.Assert f -> A.TFF (name, A.R_axiom, f, [])


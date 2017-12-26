
(* This file is free software, part of Logtk. See file "license" for more details. *)

(** {1 Utils related to TPTP parsing} *)

open Logtk

module T = TypedSTerm
module PT = STerm
module F = T.Form
module A = Ast_tptp
module Loc = ParseLocation
module Err = CCResult

type 'a or_error = ('a, string) CCResult.t

type typed = T.t
type untyped = PT.t

exception Error of string

let error msg = raise (Error msg)
let errorf msg = CCFormat.ksprintf msg ~f:error
let section = Util.Section.base

let stat_def_as_rw = Util.mk_stat "tptp.def_as_rewrite"

(** {2 Printing/Parsing} *)

type parse_cache = (string, unit) Hashtbl.t

let create_parse_cache () = Hashtbl.create 16

let find_file name dir =
  (* search in [dir], and its parents recursively *)
  let rec search dir =
    let cur_name = Filename.concat dir name in
    Util.debugf 2 ~section "search `%s`@ as `%s`" (fun k->k name cur_name);
    if Sys.file_exists cur_name
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
  else if Sys.file_exists name
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
        decls;
      close_in input
    with e ->
      close_in input;
      raise e
  in
  try
    parse_this_file ?names:None f;
    Err.return (Sequence.of_queue result_decls)
  with
    | Error msg | Sys_error msg ->
      Err.fail (Util.err_spf "in parse_tptp: %s" msg)
    | e -> Err.fail (Printexc.to_string e)

let fpf = Format.fprintf

let print_into ppt oc decls =
  fpf oc "@[<v>%a@]@?"
    (Util.pp_seq ~sep:"" (A.pp ppt))
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

(* do we turn "definition" statements into rewrite rules? *)
let enable_def_as_rewrite = ref false

let attr_of_info = function
  | A.GString "ac"
  | A.GString "AC"
  | A.GNode ("ac", [])
  | A.GNode ("AC", []) -> [UA.attr_ac]
  | A.GNode ("infix", [A.GString n]) -> [UA.attr_infix n]
  | A.GNode ("prefix", [A.GString n]) -> [UA.attr_prefix n]
  | g ->
    Util.warnf "unknown attribute `%a`, ignore" A.pp_general g;
    []

(* does this formula look like a proper definition? *)
let rec looks_like_def f = match PT.view f with
  | PT.Bind (Binder.Forall, _, f') -> looks_like_def f'
  | PT.AppBuiltin ((Builtin.Equiv | Builtin.Eq), ([_;lhs;rhs] | [lhs;rhs])) ->
    (* LHS is an atom/literal? *)
    let as_atom t = match PT.view t with
      | PT.Const id | PT.App ({PT.term=PT.Const id; _}, _) -> Some id
      | _ -> None
    in
    (* check that [lhs] is atomic and its head does not occur in [rhs] *)
    begin match as_atom lhs with
      | None -> false
      | Some id ->
        PT.Seq.symbols rhs |> Sequence.for_all (fun id' -> id<>id')
    end
  | _ -> false

let to_ast st =
  let conv_form name role f =
    let name = A.string_of_name name in
    let attrs = [UA.attr_name name] in
    match role with
      | A.R_question
      | A.R_conjecture -> UA.goal ~attrs f
      | A.R_negated_conjecture -> UA.goal ~attrs (PT.not_ f)
      | A.R_lemma -> UA.lemma ~attrs f
      | A.R_definition
        when !enable_def_as_rewrite &&
             looks_like_def f ->
        (* conversion into def *)
        Util.debugf ~section 3 "(@[tptp.def_as_rewrite@ %a@])"
          (fun k->k PT.pp f);
        Util.incr_stat stat_def_as_rw;
        UA.rewrite ~attrs f
      | A.R_definition
      | A.R_axiom
      | A.R_hypothesis
      | A.R_assumption
      | A.R_theorem
      | A.R_plain
      | A.R_finite _
      | A.R_type
      | A.R_unknown  -> UA.assert_ ~attrs f
  and conv_decl name s ty info =
    let name = A.string_of_name name in
    let attr_name = UA.attr_name name in
    let attrs' =
      CCList.flat_map
        (function
          | A.GNode ("attrs", l) -> CCList.flat_map attr_of_info l
          | _ -> [])
        info
    in
    UA.decl s ty ~attrs:(attr_name :: attrs')
  in
  begin match st with
    | A.Include _
    | A.IncludeOnly _ -> error "cannot convert `Include` to UntypedAST"
    | A.TypeDecl (name,s,ty,info)
    | A.NewType (name,s,ty,info) ->
      conv_decl name s ty info
    | A.TFF (name,role,f,_)
    | A.THF (name,role,f,_)
    | A.FOF (name,role,f,_) ->
      conv_form name role f
    | A.CNF (name,role,f,_) ->
      let f = PT.or_ f in
      conv_form name role f
  end

(* default function for giving a name to the declaration of a symbol *)
let name_sym_ sy =
  let str = CCFormat.sprintf "'ty_decl_%s'" sy in
  A.NameString str

let of_ast st =
  let name = match UA.name st with
    | None -> A.NameString "no_name"
    | Some s -> A.NameString s
  in
  match st.UA.stmt with
    | UA.Include s -> A.Include s
    | UA.Decl (s,ty) ->
      let name = name_sym_ s in
      (* XXX we should look if [ty] returns tType or not *)
      A.TypeDecl (name, s, ty, [])
    | UA.Def _ -> error "cannot convert `def` statement into TPTP"
    | UA.Rewrite _ -> error "cannot convert `rewrite` statement into TPTP"
    | UA.Data _ -> error "cannot convert `data` statement into TPTP"
    | UA.Goal f -> A.TFF (name, A.R_conjecture, f, [])
    | UA.Assert f -> A.TFF (name, A.R_axiom, f, [])
    | UA.Lemma f -> A.TFF (name, A.R_lemma, f, [])

let () =
  Options.add_opts
    [ "--tptp-def-as-rewrite", Arg.Set enable_def_as_rewrite, " in TPTP, definitions as rewrite rules";
      "--no-tptp-def-as-rewrite", Arg.Clear enable_def_as_rewrite, " disable definition->rewrite in TPTP";
    ]


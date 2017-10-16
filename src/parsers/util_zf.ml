
(* This file is free software, part of Zipperposition. See file "license" for more details. *)

(** {1 Utils for ZF} *)

open Logtk

module A = UntypedAST

type parse_cache = (string,unit) Hashtbl.t

let create_parse_cache () = Hashtbl.create 8

type parser_res = (UntypedAST.statement Sequence.t, string) CCResult.t
type 'a parser_ = 'a -> parser_res

let find_file name ~dir : string option =
  Util.debugf 2 "search `%s` in `%s`" (fun k->k name dir);
  let abs_path = Filename.concat dir name in
  if Sys.file_exists abs_path
  then Some abs_path
  else None

let rec parse_lexbuf_ ?cache ?(recursive=true) ~dir lex =
  let l =
    Parse_zf.parse_statement_list Lex_zf.token lex
  in
  if recursive
  then (
    let cache = CCOpt.get_lazy create_parse_cache cache in
    CCList.flat_map
      (fun st -> match st.A.stmt with
         | A.Include s ->
           begin match find_file s ~dir with
             | None ->
               Util.errorf ~where:"utils_zf"
                 "could not find included file `%s`" s
             | Some s' when Hashtbl.mem cache s' ->
               [] (* already included *)
             | Some s' ->
               (* put in cache *)
               Hashtbl.add cache s' ();
               parse_file_ ~cache ~recursive s'
           end
         | _ ->
           [st])
      l
  )
  else l

and parse_file_ ?cache ?recursive file =
  CCIO.with_in file
    (fun ic ->
       let lexbuf = Lexing.from_channel ic in
       ParseLocation.set_file lexbuf file;
       let dir = Filename.dirname file in
       parse_lexbuf_ ?cache ?recursive ~dir lexbuf)

let parse_lexbuf ?cache ?recursive file : parser_res =
  try parse_lexbuf_ ?cache ?recursive ~dir:"." file
      |> Sequence.of_list |> CCResult.return
  with e -> CCResult.of_exn e

let parse_stdin () : parser_res =
  let lexbuf = Lexing.from_channel stdin in
  ParseLocation.set_file lexbuf "stdin";
  parse_lexbuf ~recursive:false lexbuf

let parse_file ?cache ?recursive file : parser_res =
  if file="stdin"
  then parse_stdin()
  else try
      parse_file_ ?cache ?recursive file
      |> Sequence.of_list
      |> CCResult.return
    with
      | Sys_error e ->
        CCResult.fail (Util.err_spf "sys_error when parsing `%s`:@ %s" file e)
      | e -> CCResult.of_exn e

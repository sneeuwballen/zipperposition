
(* This file is free software, part of Zipperposition. See file "license" for more details. *)

(** {1 Utils for Dedkuti} *)

open Logtk

module E = CCResult
module A = UntypedAST
module T = STerm

type parser_res = (UntypedAST.statement Sequence.t, string) CCResult.t
type 'a parser_ = 'a -> parser_res

let parse_lexbuf_ lex =
  Parse_dk.file Lex_dk.token lex |> Sequence.of_list

let parse_lexbuf file : parser_res =
  try parse_lexbuf_ file |> E.return
  with e -> E.of_exn e

let parse_stdin () : parser_res =
  let lexbuf = Lexing.from_channel stdin in
  ParseLocation.set_file lexbuf "stdin";
  parse_lexbuf lexbuf

let parse_file file : parser_res =
  if file="stdin"
  then parse_stdin()
  else
    try
      CCIO.with_in file
        (fun ic ->
           let lexbuf = Lexing.from_channel ic in
           ParseLocation.set_file lexbuf file;
           parse_lexbuf_ lexbuf)
      |> E.return
    with
      | Sys_error e ->
        E.fail (Util.err_spf "sys_error when parsing `%s`:@ %s" file e)
      | e -> E.of_exn e

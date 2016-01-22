
(* This file is free software, part of Zipperposition. See file "license" for more details. *)

(** {1 Utils for ZF} *)

open Libzipperposition

let parse_lexbuf lex =
  Parse_zf.parse_statement_list Lex_zf.token lex
  |> Sequence.of_list (* hide *)
  |> CCError.return

let parse_stdin () =
  let lexbuf = Lexing.from_channel stdin in
  ParseLocation.set_file lexbuf "stdin";
  parse_lexbuf lexbuf

let parse_file file =
  if file="stdin"
  then parse_stdin()
  else try
    CCIO.with_in file
      (fun ic ->
        let lexbuf = Lexing.from_channel ic in
        ParseLocation.set_file lexbuf file;
        parse_lexbuf lexbuf)
  with e -> CCError.of_exn e

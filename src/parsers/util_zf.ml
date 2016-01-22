
(* This file is free software, part of Zipperposition. See file "license" for more details. *)

(** {1 Utils for ZF} *)

let parse_file file =
  try
    CCIO.with_in file
      (fun ic ->
        let lexbuf = Lexing.from_channel ic in
        Parse_zf.parse_statement_list Lex_zf.token lexbuf)
    |> Sequence.of_list (* hide *)
    |> CCError.return
  with e -> CCError.of_exn e


(* This file is free software, part of Zipperposition. See file "license" for more details. *)

(** {1 Utils for Dedkuti} *)

open Logtk

module A = UntypedAST

type parser_res = (UntypedAST.statement Sequence.t, string) CCResult.t
type 'a parser_ = 'a -> parser_res

val parse_lexbuf : Lexing.lexbuf parser_
(** parse lexbuf.
    @param recursive if true, parse includes too, relative to current dir. Default true.
    @param cache parse cache to use if [recursive = true] *)

val parse_stdin : unit parser_
(** parse stdin. Does not expand includes. *)

val parse_file : string parser_
(** [parse_file ~recursive file] parses [file]
    @param recursive if true, recursively parse includes. Default true.
    @param cache parse cache to use if [recursive = true] *)


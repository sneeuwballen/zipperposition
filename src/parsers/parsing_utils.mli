
(* This file is free software, part of Zipperposition. See file "license" for more details. *)

(** {1 Utils for Parsing in various Formats (High-Level Interface)} *)

(** This module provides functions to parse a file and guess its syntax
    (from its extension) in a convenient way.

    To parse a file [file_name] into a sequence of statements, the simplest way is:

    {[
      let fmt = guess_input file_name in
      parse_file fmt file_name
    ]}
*)

open Logtk

type 'a or_error = ('a, string) CCResult.t

val parse_tptp : string -> UntypedAST.statement Sequence.t or_error
(** Parse the given file as TPTP *)

val parse_tip : string -> UntypedAST.statement Sequence.t or_error
(** Parse the given file as TIP *)

val guess_input : string -> Input_format.t
(** Guess input from file extension *)

val input_of_file : string -> Input_format.t
(** Choose input for this file based on {!Options.input} and
    {!guess_input}.
    This is the recommended way of picking the input format for a given file. *)

val parse_file : Input_format.t -> string -> UntypedAST.statement Sequence.t or_error
(** [parse_file fmt file] parses the file using the parser for the
    given input format. *)


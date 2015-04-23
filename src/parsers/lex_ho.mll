
(*
Copyright (c) 2013, Simon Cruanes
All rights reserved.

Redistribution and use in source and binary forms, with or without
modification, are permitted provided that the following conditions are met:

Redistributions of source code must retain the above copyright notice, this
list of conditions and the following disclaimer.  Redistributions in binary
form must reproduce the above copyright notice, this list of conditions and the
following disclaimer in the documentation and/or other materials provided with
the distribution.

THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS "AS IS" AND
ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED
WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE ARE
DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT HOLDER OR CONTRIBUTORS BE LIABLE
FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL
DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR
SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER
CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY,
OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE
OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
*)

(** {1 Parser for HO} *)

{
  open Logtk
  open Parse_ho

  exception Error of string
}

let printable_char = [^ '\n']
let comment_line = ['%' '#'] printable_char*

let numeric = ['0' - '9']
let lower_alpha = ['a' - 'z']
let upper_alpha = ['A' - 'Z']
let operator = ['-' '|' '>' '<' '&' '_' '$']+
let alpha_numeric = lower_alpha | upper_alpha | numeric | '_'

let upper_word = upper_alpha alpha_numeric*
let lower_word = lower_alpha alpha_numeric*
let dollar_word = '$' lower_alpha alpha_numeric*
let dollar_dollar_word = '$' '$' lower_alpha alpha_numeric*
let interrogation_word = '?' alpha_numeric*

rule token = parse
  | eof { EOI }
  | '\n' { Lexing.new_line lexbuf; token lexbuf }
  | [' ' '\t' '\r'] { token lexbuf }
  | comment_line { token lexbuf }
  | '(' { LEFT_PAREN }
  | ')' { RIGHT_PAREN }
  | '[' { LEFT_BRACKET }
  | ']' { RIGHT_BRACKET }
  | '{' { LEFT_BRACE }
  | '}' { RIGHT_BRACE }
  | '|' { PIPE }
  | ',' { COMMA }
  | '.' { DOT }
  | '_' { WILDCARD }
  | "->" { RIGHT_ARROW }
  | "<-" { LEFT_ARROW }
  | "~" { NOT }
  | "<=>" { EQUIV }
  | "=>" { IMPLY }
  | "<~>" { XOR }
  | "&&" { AND }
  | "||" { OR }
  | "=" { EQ }
  | "@" { AT }
  | "/\\" { FORALL_TY }
  | "!=" { NEQ }
  | ":" { COLUMN }
  | "!" { FORALL }
  | "?" { EXISTS }
  | "^" { LAMBDA }
  | "val" { VAL }
  | lower_word { LOWER_WORD(Lexing.lexeme lexbuf) }
  | dollar_word { DOLLAR_WORD(Lexing.lexeme lexbuf) }
  | dollar_dollar_word { DOLLAR_DOLLAR_WORD(Lexing.lexeme lexbuf) }
  | upper_word { UPPER_WORD(Lexing.lexeme lexbuf) }
  | interrogation_word { INTERROGATION_WORD(Lexing.lexeme lexbuf) }
  | operator { OPERATOR(Lexing.lexeme lexbuf) }
  | _ as c { raise (Error (Printf.sprintf "lexer fails on char '%c'" c)) }


{
  module E = CCError
  type 'a or_error = [`Ok of 'a | `Error of string ]

  let decl_of_string s : Ast_ho.t or_error =
    try
      E.return (Parse_ho.parse_decl token (Lexing.from_string s))
    with
    | Parse_ho.Error -> E.fail "parse error"
    | Error msg -> E.fail msg

  let decls_of_string s : Ast_ho.t list or_error =
    try
      E.return (Parse_ho.parse_decls token (Lexing.from_string s))
    with
    | Parse_ho.Error -> E.fail "parse error"
    | Error msg -> E.fail msg

  let term_of_string s : PrologTerm.t option =
    try
      Some (Parse_ho.parse_term token (Lexing.from_string s))
    with Parse_ho.Error | Error _ -> None

  let pterm (s:string): HOTerm.t option =
    CCOpt.(
      term_of_string s >>= fun t ->
      let ctx = TypeInference.Ctx.create Signature.TPTP.base in
      CCError.to_opt (TypeInference.HO.convert ~generalize:true ~ctx t)
    )
}

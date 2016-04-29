
(* This file is free software, part of Zipperposition. See file "license" for more details. *)

(** {1 Lexer for Zipperposition Formulas} *)

{
  open Libzipperposition
  open Parse_zf
}

let printable_char = [^ '\n']
let comment_line = '#' printable_char*

let numeric = ['0' - '9']
let lower_alpha = ['a' - 'z']
let upper_alpha = ['A' - 'Z']
let alpha_numeric = lower_alpha | upper_alpha | numeric | '_'

let upper_word = upper_alpha alpha_numeric*
let lower_word = lower_alpha alpha_numeric*

let filepath = '"' ([^ '"'] | '\\' '"')* '"'

let zero_numeric = '0'
let non_zero_numeric = ['1' - '9']
let numeric = ['0' - '9']
let sign = ['+' '-']

let dot_decimal = '.' numeric +
let positive_decimal = non_zero_numeric numeric*
let decimal = zero_numeric | positive_decimal
let unsigned_integer = decimal
let signed_integer = sign unsigned_integer
let integer = signed_integer | unsigned_integer

rule token = parse
  | eof { EOI }
  | '\n' { Lexing.new_line lexbuf; token lexbuf }
  | [' ' '\t' '\r'] { token lexbuf }
  | comment_line { token lexbuf }
  | '(' { LEFT_PAREN }
  | ')' { RIGHT_PAREN }
  | '[' { LEFT_BRACKET }
  | ']' { RIGHT_BRACKET }
  | '.' { DOT }
  | ',' { COMMA }
  | '_' { WILDCARD }
  | ':' { COLON }
  | "=" { LOGIC_EQ }
  | "!=" { LOGIC_NEQ }
  | ":=" { EQDEF }
  | "->" { ARROW }
  | "val" { VAL }
  | "def" { DEF }
  | "type" { TYPE }
  | "prop" { PROP }
  | "assert" { ASSERT }
  | "goal" { GOAL }
  | "and" { AND }
  | "rewrite" { REWRITE }
  | "true" { LOGIC_TRUE }
  | "false" { LOGIC_FALSE }
  | "pi" { PI }
  | "data" { DATA }
  | "&&" { LOGIC_AND }
  | "||" { LOGIC_OR }
  | "|" { VERTICAL_BAR }
  | "~" { LOGIC_NOT }
  | "forall" { LOGIC_FORALL }
  | "exists" { LOGIC_EXISTS }
  | "=>" { LOGIC_IMPLY }
  | "<=>" { LOGIC_EQUIV }
  | "AC" { AC }
  | "name" { NAME }
  | lower_word { LOWER_WORD(Lexing.lexeme lexbuf) }
  | upper_word { UPPER_WORD(Lexing.lexeme lexbuf) }
  | _ as c
    {
      let loc = UntypedAST.Loc.of_lexbuf lexbuf in
      UntypedAST.errorf loc "unexpected char '%c'" c
    }

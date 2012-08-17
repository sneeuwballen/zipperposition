(*
This file is part of the first order theorem prover Darwin
Copyright (C) 2006  The University of Iowa

This program is free software; you can redistribute it and/or
modify it under the terms of the GNU General Public License
as published by the Free Software Foundation; either version 2
of the License, or (at your option) any later version.

This program is distributed in the hope that it will be useful,
but WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
GNU General Public License for more details.

You should have received a copy of the GNU General Public License
along with this program; if not, write to the Free Software
Foundation, Inc., 59 Temple Place - Suite 330, Boston, MA  02111-1307, USA.
*)



(* A lexer for a subset of the protein tme format for first order clauses *)

{ 

open Parser_tptp

let prev_column_index =
  ref 1

let current_column_index =
  ref 1

let prev_line_index =
  ref 1

let current_line_index =
  ref 1

let current_token =
  ref ""

let update_column_index (value: int) =
  prev_column_index := !current_column_index;
  current_column_index := value


let update_line_index () =
  prev_line_index := !current_line_index;
  current_line_index := !current_line_index + 1;
  update_column_index 1


let count_new_lines (string: string) : unit =
  String.iter
    (fun char ->
       if char = '\n' then
         update_line_index ()
       else
         update_column_index (!current_column_index + 1)
    )
    string

let update_token (token: string) =
  current_token := token;
  update_column_index (!current_column_index + String.length token)


let lexing_error (error: string) (token: string) =
  print_endline (error
            ^ " at line " ^ string_of_int !current_line_index
            ^ " column " ^ string_of_int !current_column_index
            ^ ":\n" ^ token);
  raise Const.PARSE_ERROR

let parse_error () =
  print_endline ("Parse error"
            ^ " at line " ^ string_of_int !current_line_index
            ^ " column " ^ string_of_int !current_column_index
            ^ ":\n" ^ !current_token);
  raise Const.PARSE_ERROR

}




(* regular expressions *)

let numeric = ['0' - '9']
let non_zero_numeric = ['1' - '9']
let lower_alpha = ['a' - 'z']
let upper_alpha = ['A' - 'Z']
let alpha_numeric = ( lower_alpha | upper_alpha | numeric | '_' )
let dollar_dollar = "$$"
(*let printable_char = _*)
let vline = '|'

let sign = ['+' '-']
let unsigned_decimal = ('0' | non_zero_numeric numeric* )
let signed_decimal = sign unsigned_decimal
let unsigned_integer = unsigned_decimal
let signed_integer = sign unsigned_integer
let fraction_decimal = '.' numeric numeric*
let real = ( signed_decimal | unsigned_decimal ) fraction_decimal

let upper_word = upper_alpha alpha_numeric*
let lower_word = lower_alpha alpha_numeric*
let dollar_dollar_word = dollar_dollar lower_word

(*let single_quoted = "'" ([^ '\\' '''] | "\'" | "\\")* "'"*)
let single_quoted = "'" ([^ '''] | "\\'")+ "'"
let single_quoted_lower_word = "'" lower_word "'"
(*let distinct_object = '"' ([^ '\\' '"'] | '\\' '"' | "\\")* '"'*)
let distinct_object = '"' ([^ '"'] | "\\\"")+ '"'
(*
let comment_line = "%" printable_char*
let not_star_slash = ( [^'*']* ['*']['*']* [^ '/' '*'])* [^'*']*
let comment_block = "/*" not_star_slash '*' '*'* '/'
let comment = comment_line | comment_block
*)
let one_line_comment =
  '%' [^ '\n' '\r']* ('\n' | "\r\n")

let multi_line_comment =
  "/*" ( [^ '*'] | ('*' [^ '/']) )* "*/"

let multi_line_comment_unclosed =
  "/*" ( [^ '*'] | ('*' [^ '/']) )* eof




let variable =
  [ 'A'-'Z' '_' ] [ 'a'-'z' 'A'-'Z' '0'-'9' '_']*
  
let symbol =
  (*[ 'a'-'z' '0'-'9' ':' '*' '+' '-' '/'] [ 'a'-'z' 'A'-'Z' '0'-'9' '_']* *)
  [ 'a'-'z' '0'-'9' '_'] [ 'a'-'z' 'A'-'Z' '0'-'9' '_']*

let implication =
  ":-" | "<-"

let negation =
  "~"





rule token =
    parse
      | [' ' '\t' '\r']              { update_token (Lexing.lexeme lexbuf);
                                       token lexbuf } (* skip blanks *)
      | ['\n']                       { update_line_index ();
                                       current_token := Lexing.lexeme lexbuf;
                                       token lexbuf } (* skip new lines *)
      | one_line_comment             { update_line_index ();
                                       current_token := Lexing.lexeme lexbuf;
                                       token lexbuf } (* skip comment *)
      | multi_line_comment           { count_new_lines (Lexing.lexeme lexbuf);
                                       current_token := Lexing.lexeme lexbuf;
                                       token lexbuf } (* skip comment *)
      | multi_line_comment_unclosed  { prev_column_index := !current_column_index;
                                       prev_line_index := !current_line_index;
                                       lexing_error "Unclosed Comment" (Lexing.lexeme lexbuf) }
          (* end of input - for channels, strings, ... *)
      | eof                          { update_token (Lexing.lexeme lexbuf); EOI }
      | "fof"                        { update_token (Lexing.lexeme lexbuf); FOF }
      | "cnf"                        { update_token (Lexing.lexeme lexbuf); CNF }
      | "thf"                        { update_token (Lexing.lexeme lexbuf);
                                       failwith "Parser_tptp: tfh syntax not supported." }
      | "include"                    { update_token (Lexing.lexeme lexbuf); INCLUDE }
      | lower_word                   { update_token (Lexing.lexeme lexbuf); LOWER_WORD(Lexing.lexeme lexbuf) }
      | upper_word                   { update_token (Lexing.lexeme lexbuf); UPPER_WORD(Lexing.lexeme lexbuf) }
      | single_quoted_lower_word     { update_token (Lexing.lexeme lexbuf); 
                                       let s = Lexing.lexeme lexbuf in
                                         SINGLE_QUOTED(String.sub s 1 (String.length s - 2)) }
      | single_quoted                { update_token (Lexing.lexeme lexbuf); SINGLE_QUOTED(Lexing.lexeme lexbuf) }
      | distinct_object              { update_token (Lexing.lexeme lexbuf); DISTINCT_OBJECT(Lexing.lexeme lexbuf) }
      | dollar_dollar_word           { update_token (Lexing.lexeme lexbuf); DOLLAR_DOLLAR_WORD(Lexing.lexeme lexbuf) }
      | unsigned_integer             { update_token (Lexing.lexeme lexbuf); UNSIGNED_INTEGER(Lexing.lexeme lexbuf) }
      | signed_integer               { update_token (Lexing.lexeme lexbuf); SIGNED_INTEGER(Lexing.lexeme lexbuf) }
      | real                         { update_token (Lexing.lexeme lexbuf); REAL(Lexing.lexeme lexbuf) }
      | '('                          { update_token (Lexing.lexeme lexbuf); LEFT_PARENTHESIS }
      | ')'                          { update_token (Lexing.lexeme lexbuf); RIGHT_PARENTHESIS }
      | '['                          { update_token (Lexing.lexeme lexbuf); LEFT_BRACKET }
      | ']'                          { update_token (Lexing.lexeme lexbuf); RIGHT_BRACKET }
      | '.'                          { update_token (Lexing.lexeme lexbuf); DOT }
      | ':'                          { update_token (Lexing.lexeme lexbuf); COLON }
      | ","                          { update_token (Lexing.lexeme lexbuf); COMMA }
      | '='                          { update_token (Lexing.lexeme lexbuf); EQUALITY }
      | "!="                         { update_token (Lexing.lexeme lexbuf); DISEQUALITY }
      | "<=>"                        { update_token (Lexing.lexeme lexbuf); BIJECTION }
      | "=>"                         { update_token (Lexing.lexeme lexbuf); LEFT_IMPLICATION }
      | "<="                         { update_token (Lexing.lexeme lexbuf); RIGHT_IMPLICATION }
      | "<~>"                        { update_token (Lexing.lexeme lexbuf); UNKNOWN }
      | negation                     { update_token (Lexing.lexeme lexbuf); NEGATION }
      | "$true"                      { update_token (Lexing.lexeme lexbuf); DOLLAR_TRUE }
      | "$false"                      { update_token (Lexing.lexeme lexbuf); DOLLAR_FALSE }
      | '$'                          { update_token (Lexing.lexeme lexbuf); DOLLAR }
      | '&'                          { update_token (Lexing.lexeme lexbuf); AND }
      | '|'                          { update_token (Lexing.lexeme lexbuf); OR }
      | '!'                          { update_token (Lexing.lexeme lexbuf); FORALL }
      | '?'                          { update_token (Lexing.lexeme lexbuf); EXISTS }
      | _                            { prev_column_index := !current_column_index;
                                       prev_line_index := !current_line_index;
                                       lexing_error "Invalid Input" (Lexing.lexeme lexbuf) }


{ 

(* footer *)

}

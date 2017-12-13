(* Copyright 2005 INRIA *)
(* Copyright 2014 Ali Assaf *)
(* Copyright 2014 Raphaël Cauderlier *)
{

open Logtk
open Parse_dk
let pos = ParseLocation.of_lexbuf

}
let id = (['_' '\'' '0'-'9' 'a'-'z' 'A'-'Z'])+
let qid = id '.' id
let space = [' ' '\t']
let number = (['0'-'9'])+

rule token = parse
| space { token lexbuf } (* skip blanks *)
| '\n' { Lexing.new_line lexbuf; token lexbuf }
| "(;" { comment (pos lexbuf) [] lexbuf }
| "def" { DEFKW }
| "cc.uT" { TYPE }
| "Type" { TYPE }
| "cc.eT" { TERM }
| "cc.eP" { PROOF }
| "cc.Arrow" { CCARR }
| "dk_logic.true" { TRUE }
| "dk_logic.false" { FALSE }
| "dk_logic.not" { NOT }
| "dk_logic.and" { AND }
| "dk_logic.or" { OR }
| "dk_logic.imp" { IMP }
| "dk_logic.eqv" { EQV }
| "dk_logic.equal" { EQUAL }
| "dk_logic.forall" { ALL }
| "dk_logic.exists" { EX }
| "dk_logic.forall_type" { ALL_TYPE }
| "dk_logic.exists_type" { EX_TYPE }
| "dk_logic.eP" { PROOF }
| "dk_logic.Prop" { PROP }
| "dk_builtins.prop" { PROP }
| number as n { NUMBER(n) }
| id as id { ID(id) }
| qid as qid { QID(qid) }
| ":" { COLON }
| "." { DOT }
| "->" { ARROW }
| "=>" { DOUBLE_ARROW }
| ":=" { DEF }
| "(" { LPAREN }
| ")" { RPAREN }
| "[" { LBRACK }
| "]" { RBRACK }
| "," { COMMA }
| "-->" { REW }

| "%%begin-auto-proof"                      { BEGINPROOF }
| "%%type:"                                 { BEGIN_TY }
| "%%begin-type-alias:"                     { BEGIN_TYPEALIAS }
| "%%begin-variable:"                       { BEGIN_VAR }
| "%%begin-hypothesis:"                     { BEGIN_HYP }
| "%%end-type-alias"                        { END_TYPEALIAS }
| "%%end-variable"                          { END_VAR }
| "%%end-hypothesis"                        { END_HYP }
| "%%name:" space* (id as name) space*      { BEGINNAME name }
| "%%" id ":"                               { BEGINHEADER }
| "%%end-auto-proof"                        { ENDPROOF }

| eof { EOF }
| _ as c
  {
    let loc = ParseLocation.of_lexbuf lexbuf in
    UntypedAST.errorf loc "lexer fails on char %c\n" c
  }
and comment current stack = parse
| "(;" { comment (pos lexbuf) (current :: stack) lexbuf }
| ";)" { match stack with [] -> token lexbuf | h :: t -> comment h t lexbuf }
| '\n' { Lexing.new_line lexbuf; comment current stack lexbuf }
| eof {
  let loc = ParseLocation.of_lexbuf lexbuf in
  UntypedAST.errorf loc "This comment is not closed"  }
| _ { comment current stack lexbuf }

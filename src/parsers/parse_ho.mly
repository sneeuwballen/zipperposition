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

(** {1 Higher-Order parser}

A parser with a nice curried syntax. *)

%{
  open Logtk

  module L = ParseLocation
  module A = Ast_ho
  module Term = STerm
%}

%token EOI

%token LEFT_PAREN
%token RIGHT_PAREN
%token LEFT_BRACKET
%token RIGHT_BRACKET
%token LEFT_BRACE
%token RIGHT_BRACE

%token PIPE
%token LEFT_ARROW
%token RIGHT_ARROW
%token COMMA
%token WILDCARD
%token DOT
%token VAL
%token COLUMN
%token FORALL
%token EXISTS
%token LAMBDA
%token FORALL_TY

%token EQ
%token NEQ
%token EQUIV
%token XOR
%token IMPLY
%token AND
%token OR
%token NOT

%token <string> LOWER_WORD
%token <string> DOLLAR_WORD
%token <string> DOLLAR_DOLLAR_WORD
%token <string> UPPER_WORD
%token <string> INTERROGATION_WORD
%token <string> OPERATOR
%token <string> INTEGER

%nonassoc EQUIV
%nonassoc XOR
%nonassoc EQ
%nonassoc NEQ
%nonassoc AND
%nonassoc OR
%nonassoc NOT
%right IMPLY

%start <Ast_ho.t> parse_decl
%start <Ast_ho.t list> parse_decls
%start <Logtk.STerm.t> parse_term

%%

parse_decls:
  | l=declaration* EOI { l }

parse_decl:
  | d=declaration EOI { d }

parse_term:
  | t=term EOI { t }

declaration:
  | VAL w=LOWER_WORD COLUMN ty=type_ DOT
    { A.Type (w, ty) }
  | t=term DOT
    { A.Clause (t, []) }
  | t=term LEFT_ARROW l=separated_nonempty_list(COMMA, term) DOT
    { A.Clause (t,l) }

/* records */

%public record_rest(TERM):
  | { None }
  | PIPE t=TERM { Some t }

%public record_field(BIND,TERM):
  | n=LOWER_WORD BIND t=TERM { n,t }

%public record_body(BIND, TERM):
  | l=separated_list(COMMA, record_field(BIND, TERM)) { l }

type_:
  | l=app_type RIGHT_ARROW r=type_
    {
      let loc = L.mk_pos $startpos $endpos in
      Term.fun_ty ~loc [l] r
    }
  | FORALL_TY v=typed_var ty=type_
    {
      let loc = L.mk_pos $startpos $endpos in
      Term.forall_ty ~loc [v] ty
    }
  | t=app_type {t}

app_type:
  | w=LOWER_WORD args=unary_type+
    {
      let loc = L.mk_pos $startpos $endpos in
      let f = Term.const ~loc w in
      Term.app ~loc f args
    }
  | t=unary_type { t }

unary_type:
  | w=LOWER_WORD
    {
      let loc = L.mk_pos $startpos $endpos in
      Term.const ~loc w
    }
  | LEFT_PAREN t=type_ RIGHT_PAREN { t }
  | w=INTERROGATION_WORD
  | w=UPPER_WORD
    {
      let loc = L.mk_pos $startpos $endpos in
      Term.var ~loc w
    }
  | w=DOLLAR_WORD
  | w=DOLLAR_DOLLAR_WORD
    {
      let loc = L.mk_pos $startpos $endpos in
      Term.const ~loc w
    }
  | WILDCARD
    { Term.wildcard }
  | LEFT_BRACKET l=separated_nonempty_list(COMMA, type_) RIGHT_BRACKET
    {
      let loc = L.mk_pos $startpos $endpos in
      Term.list_ ~loc l
    }
  | LEFT_BRACE l=record_body(COLUMN, type_) rest=record_rest(UPPER_WORD) RIGHT_BRACE
    {
      let loc = L.mk_pos $startpos $endpos in
      Term.record ~loc l ~rest
    }

unary_term:
  | LEFT_PAREN t=term RIGHT_PAREN { t }
  | w=INTERROGATION_WORD
    {
      let loc = L.mk_pos $startpos $endpos in
      Term.var ~loc w
    }
  | v=var { v }
  | WILDCARD
    {
      let loc = L.mk_pos $startpos $endpos in
      Term.wildcard
    }
  | w=LOWER_WORD
  | w=DOLLAR_WORD
  | w=DOLLAR_DOLLAR_WORD
    {
      let loc = L.mk_pos $startpos $endpos in
      Term.const ~loc w
    }
  | w=INTEGER { Term.int_ (Z.of_string w) }
  | LEFT_BRACKET l=separated_list(COMMA, term) RIGHT_BRACKET
    {
      let loc = L.mk_pos $startpos $endpos in
      Term.list_ ~loc l
    }
  | LEFT_BRACE l=record_body(EQ,term) rest=record_rest(UPPER_WORD) RIGHT_BRACE
    {
      let loc = L.mk_pos $startpos $endpos in
      Term.record ~loc l ~rest
    }

app_term:
  | t=unary_term { t }
  | t=unary_term l=unary_term+
    {
      let loc = L.mk_pos $startpos $endpos in
      Term.app ~loc t l
    }

formula_term:
  | t=app_term { t }
  | l=formula_term AND r=formula_term
    {
      let loc = L.mk_pos $startpos $endpos in
      Term.and_ ~loc [l;r]
    }
  | l=formula_term OR r=formula_term
    {
      let loc = L.mk_pos $startpos $endpos in
      Term.or_ ~loc [l;r]
    }
  | l=formula_term EQUIV r=formula_term
    {
      let loc = L.mk_pos $startpos $endpos in
      Term.equiv ~loc l r
    }
  | l=formula_term XOR r=formula_term
    {
      let loc = L.mk_pos $startpos $endpos in
      Term.xor ~loc l r
    }
  | l=formula_term IMPLY r=formula_term
    {
      let loc = L.mk_pos $startpos $endpos in
      Term.imply ~loc l r
    }
  | l=formula_term EQ r=formula_term
    {
      let loc = L.mk_pos $startpos $endpos in
      Term.eq ~loc l r
    }
  | l=formula_term NEQ r=formula_term
    {
      let loc = L.mk_pos $startpos $endpos in
      Term.neq ~loc l r
    }
  | NOT t=formula_term
    {
      let loc = L.mk_pos $startpos $endpos in
      Term.not_ ~loc  t
    }
  | l=app_term o=OPERATOR r=app_term
    {
      let loc = L.mk_pos $startpos $endpos in
      A.app_infix ~loc o l r
    }

term:
  | t=formula_term { t }
  | LAMBDA LEFT_BRACKET v=typed_vars RIGHT_BRACKET COLUMN t=term
    {
      let loc = L.mk_pos $startpos $endpos in
      Term.lambda ~loc v t
    }
  | FORALL LEFT_BRACKET v=typed_vars RIGHT_BRACKET COLUMN t=term
    {
      let loc = L.mk_pos $startpos $endpos in
      Term.forall ~loc v t
    }
  | EXISTS LEFT_BRACKET v=typed_vars RIGHT_BRACKET COLUMN t=term
    {
      let loc = L.mk_pos $startpos $endpos in
      Term.exists ~loc v t
    }

var:
  | w=UPPER_WORD
    {
      let loc = L.mk_pos $startpos $endpos in
      Term.var ~loc w
    }

typed_var:
  | v=UPPER_WORD { v, None }
  | v=UPPER_WORD COLUMN ty=app_type { v, Some ty }

typed_vars:
  | v=separated_nonempty_list(COMMA,typed_var) { v }

%%


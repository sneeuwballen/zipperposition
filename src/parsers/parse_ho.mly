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

  module Sym = Symbol
  module L = ParseLocation
  module A = Ast_ho
  module Term = Ast_ho.Term
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

%token EQ
%token NEQ
%token EQUIV
%token XOR
%token IMPLY
%token AND
%token OR
%token NOT

%token <string> LOWER_WORD
%token <string> UPPER_WORD
%token <string> INTERROGATION_WORD

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
%start <Logtk.PrologTerm.t> parse_term

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
  | w=LOWER_WORD l=unary_type +
    {
      let loc = L.mk_pos $startpos $endpos in
      Term.app ~loc (Term.const ~loc (Sym.of_string w)) l
    }
  | l=unary_type RIGHT_ARROW r=type_
    {
      let loc = L.mk_pos $startpos $endpos in
      Term.mk_fun_ty ~loc [l] r
    }
  | t=unary_type { t }

unary_type:
  | LEFT_PAREN t=type_ RIGHT_PAREN { t }
  | w=INTERROGATION_WORD
    {
      let loc = L.mk_pos $startpos $endpos in
      Term.var ~loc w
    }
  | w=UPPER_WORD
    {
      let loc = L.mk_pos $startpos $endpos in
      Term.var ~loc w
    }
  | w=LOWER_WORD
    {
      let loc = L.mk_pos $startpos $endpos in
      Term.const ~loc (Sym.of_string w)
    }
  | WILDCARD
    { Term.wildcard }
  | LEFT_BRACKET l=separated_nonempty_list(COMMA, type_) RIGHT_BRACKET
    {
      let loc = L.mk_pos $startpos $endpos in
      Term.list_ ~loc l
    }
  | LEFT_BRACE l=record_body(COLUMN, type_) rest=record_rest(type_) RIGHT_BRACE
    {
      let loc = L.mk_pos $startpos $endpos in
      Term.record ~loc l ~rest
    }

term:
  | t=unary_term { t }
  | t=formula_term { t }

formula_term:
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
  | t=app_term{ t }

app_term:
  | t=unary_term { t }
  | t=unary_term l=unary_term +
    {
      let loc = L.mk_pos $startpos $endpos in
      Term.app ~loc t l
    }

unary_term:
  | LEFT_PAREN t=term RIGHT_PAREN { t }
  | w=INTERROGATION_WORD
    {
      let loc = L.mk_pos $startpos $endpos in
      Term.var ~loc w
    }
  | w=UPPER_WORD
    {
      let loc = L.mk_pos $startpos $endpos in
      Term.var ~loc w
    }
  | WILDCARD
    {
      let loc = L.mk_pos $startpos $endpos in
      Term.const ~loc Sym.Base.wildcard
    }
  | w=LOWER_WORD
    {
      let loc = L.mk_pos $startpos $endpos in
      Term.const ~loc (Sym.of_string w)
    }
  | LEFT_BRACKET l=separated_list(COMMA, term) RIGHT_BRACKET
    {
      let loc = L.mk_pos $startpos $endpos in
      Term.list_ ~loc l
    }
  | LEFT_BRACE l=record_body(EQ,term) rest=record_rest(term) RIGHT_BRACE
    {
      let loc = L.mk_pos $startpos $endpos in
      Term.record ~loc l ~rest
    }


%%


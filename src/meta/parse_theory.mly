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

(** {1 Theory file Parser} *)

%{
  open Logtk

  module T = FOTerm
  module F = FOFormula

  let remove_quotes s =
    assert (s.[0] = '\'' && s.[String.length s - 1] = '\'');
    String.sub s 1 (String.length s - 2)

  let __table = Hashtbl.create 5
  let __count = ref 0

  let clear_table () =
    Hashtbl.clear __table;
    __count := 0

  (** Get variable associated with this name *)
  let get_var ~ty name =
    try Hashtbl.find __table name
    with Not_found ->
      let v = T.mk_var ~ty !__count in
      incr __count;
      Hashtbl.add __table name v;
      v
%}

%token EOI

%token DOT
%token COMMA
%token LEFT_PAREN
%token RIGHT_PAREN
%token LEFT_BRACKET
%token RIGHT_BRACKET

%token IS
%token IF
%token AND_ALSO
%token AXIOM
%token THEORY
%token LEMMA
%token INCLUDE
%token RAW

%token NOT

%token COLUMN
%token STAR
%token ARROW

%token AND
%token NOTAND
%token VLINE
%token NOTVLINE
%token IMPLY
%token LEFT_IMPLY
%token EQUIV
%token XOR
%token GENTZEN_ARROW
%token EQUAL
%token NOT_EQUAL

%token FORALL
%token EXISTS
/* %token LAMBDA */

%token <string> LOWER_WORD
%token <string> UPPER_WORD
%token <string> SINGLE_QUOTED
%token <string> DISTINCT_OBJECT
%token <string> DOLLAR_WORD
%token <string> DOLLAR_DOLLAR_WORD
%token <string> REAL
%token <string> RATIONAL
%token <string> INTEGER

%left VLINE
%left AND
%nonassoc EQUIV
%nonassoc XOR
%nonassoc IMPLY
%nonassoc LEFT_IMPLY
%nonassoc NOTVLINE
%nonassoc NOTAND

%start <Ast_theory.statement> parse_statement
%start <Ast_theory.statement list> parse_statements

%%

/* top-level */

parse_statement: d=statement EOI { d }
parse_statements: l=statements EOI { l }

/* theory grammar */

statements:
  | EOI { [] }
  | s=statement l=statements { s :: l }
  | error statements { [Ast_theory.Error ("syntax error", $startpos($1), $endpos($1))] }

statement:
  | t=theory { t }
  | a=axiom { a }
  | l=lemma { l }
  | c=clause { c }
  | i=include_ { i }

theory:
  | THEORY h=datalog_atom IS l=premises DOT
    { let name, args = h in
      Ast_theory.Theory (name, args, l)
    }

axiom:
  | AXIOM h=datalog_atom IS f=fof_formula DOT
    { let name, args = h in
      Ast_theory.Axiom (name, args, f)
    }

lemma:
  | LEMMA f=fof_formula IF l=premises DOT
    { Ast_theory.LemmaInline (f, l) }
  | LEMMA AXIOM h=datalog_atom IF l=premises DOT
    { let name, args = h in
      Ast_theory.Lemma (name, args, l)
    }

clause:
  | RAW h=datalog_lit DOT
    { Ast_theory.Clause (h, []) }
  | RAW h=datalog_lit IF l=separated_nonempty_list(COMMA, datalog_lit) DOT
    { Ast_theory.Clause (h, l) }

datalog_lit:
  | s=atomic_word { s, [] }
  | s=atomic_word LEFT_PAREN l=separated_nonempty_list(COMMA, datalog_term) RIGHT_PAREN
    { s, l }

datalog_term:
  | s=atomic_word { s }
  | s=UPPER_WORD { s }

include_:
  | INCLUDE w=atomic_word DOT
    { Ast_theory.Include w }

premises: separated_nonempty_list(AND_ALSO, premise) { $1 }

premise:
  | f=fof_formula { Ast_theory.IfPattern f }
  | AXIOM h=datalog_atom { let name, args = h in Ast_theory.IfAxiom (name, args) }
  | THEORY h=datalog_atom { let name, args = h in Ast_theory.IfTheory (name, args) }

datalog_atom:
  | w=LOWER_WORD { w, [] }
  | w=LOWER_WORD LEFT_PAREN args=separated_nonempty_list(COMMA, atomic_word) RIGHT_PAREN
    { w, args }

/* TPTP formulas */

fof_formula:
  | fof_logic_formula { $1 }
  | fof_sequent { $1 } 

fof_sequent:
  | l=fof_tuple GENTZEN_ARROW r=fof_tuple
    { F.mk_imply (F.mk_and l) (F.mk_or r) }
  | LEFT_PAREN seq=fof_sequent RIGHT_PAREN { seq }

fof_tuple:
  LEFT_BRACKET l=separated_list(COMMA, fof_logic_formula) RIGHT_BRACKET { l } 

fof_logic_formula:
  | f=fof_unitary_formula { f }
  | l=fof_logic_formula o=binary_connective r=fof_logic_formula
    { o l r }

fof_unitary_formula:
  | fof_quantified_formula { $1 }
  | fof_unary_formula { $1 } 
  | atomic_formula { $1 } 
  | LEFT_PAREN f=fof_logic_formula RIGHT_PAREN { f }

fof_quantified_formula:
  | q=fol_quantifier LEFT_BRACKET vars=variables RIGHT_BRACKET COLUMN f=fof_unitary_formula
    { q vars f }

fof_unary_formula:
  | o=unary_connective f=fof_unitary_formula { o f }
  | f=fol_infix_unary { f }

fol_infix_unary:
  | l=term o=infix_inequality r=term
    { o l r }
  
%inline binary_connective:
  | EQUIV { F.mk_equiv }
  | IMPLY { F.mk_imply }
  | LEFT_IMPLY { F.mk_imply }
  | XOR { F.mk_xor }
  | NOTVLINE { fun x y -> F.mk_not (F.mk_or [x; y]) }
  | NOTAND { fun x y -> F.mk_not (F.mk_and [x;x]) }
  | AND { fun x y -> F.mk_and [x;y] }
  | VLINE { fun x y -> F.mk_or [x;y] }
%inline fol_quantifier:
  | FORALL { F.mk_forall_list }
  | EXISTS { F.mk_exists_list }
%inline unary_connective:
  | NOT { F.mk_not }
%inline infix_inequality:
  | NOT_EQUAL { F.mk_neq }

atomic_formula:
  | plain_atomic_formula { $1 }
  | defined_atomic_formula { $1 }
  | system_atomic_formula { $1 }

plain_atomic_formula: plain_term { F.mk_atom $1 }

defined_atomic_formula:
  | defined_plain_formula { $1 }
  | defined_infix_formula { $1 }

defined_infix_formula:
  | l=term o=defined_infix_pred r=term  { o l r }

%inline defined_infix_pred:
  | EQUAL { F.mk_eq }

defined_plain_formula:
  | p=defined_prop
    { F.mk_atom (T.mk_const p) }
  | p=defined_pred LEFT_PAREN args=arguments RIGHT_PAREN
    { F.mk_atom (T.mk_node p args) }

/* includes $true and $false */
defined_prop: atomic_defined_word { $1 } 
defined_pred: atomic_defined_word { $1 }

system_atomic_formula: system_term { F.mk_atom $1 }
  
/* Terms */

term:
  | function_term { $1 }
  | variable { $1 }
  /* | conditional_term { $1 }  for TFF */
  /* | let_term { $1 } */

function_term:
  | plain_term { $1 }
  | defined_term { $1 }
  | system_term { $1 }

plain_term:
  | s=constant { T.mk_const s }
  | f=functor_ LEFT_PAREN args=arguments RIGHT_PAREN { T.mk_node f args }

constant:
| s=atomic_word { Symbol.mk_const s }
| s=atomic_defined_word { s }
functor_: f=atomic_word { Symbol.mk_const f }

defined_term:
  | defined_atom { T.mk_const $1 }
  | defined_atomic_term { $1 }

defined_atom:
  | n=INTEGER { Symbol.mk_bigint (Big_int.big_int_of_string n) }
  | n=RATIONAL { Symbol.mk_ratio (Ratio.ratio_of_string n) }
  | n=REAL { Symbol.mk_real (float_of_string n) }
  | s=DISTINCT_OBJECT { Symbol.mk_distinct s }

defined_atomic_term:
  | defined_plain_term { $1 }
  /* | defined_infix_term { $1 } */

defined_plain_term:
  | s=defined_constant { T.mk_const s }
  | f=defined_functor LEFT_PAREN args=arguments RIGHT_PAREN { T.mk_node f args }

defined_constant: defined_functor { $1 }
defined_functor: s=atomic_defined_word { s }

system_term:
  | c=system_constant { T.mk_const c }
  | f=system_functor LEFT_PAREN args=arguments RIGHT_PAREN { T.mk_node f args }

system_constant: system_functor { $1 }
system_functor: s=atomic_system_word { s }

tff_type:
  | ty=tff_atom_type { ty }
  | l=tff_atom_type ARROW r=tff_atom_type
    { Type.mk_fun r [l] }
  | LEFT_PAREN args=tff_ty_args RIGHT_PAREN ARROW r=tff_atom_type
    { Type.mk_fun r args }

tff_atom_type:
  | w=UPPER_WORD { Type.var w }
  | w=type_const { Type.const w }
  | LEFT_PAREN ty=tff_type RIGHT_PAREN { ty }

tff_ty_args:
  | ty=tff_atom_type { [ty] }
  | hd=tff_atom_type STAR tl=tff_ty_args { hd :: tl }
  
type_const:
  | w=LOWER_WORD { w }
  | w=DOLLAR_WORD { w }

arguments: separated_nonempty_list(COMMA, term) { $1 }

variables:
  | l=separated_nonempty_list(COMMA, variable) { l }

/* TODO: typed variables */
variable:
  | x=UPPER_WORD { get_var ~ty:Type.i x }
  | x=UPPER_WORD COLUMN ty=tff_type { get_var ~ty x }

atomic_word:
  | s=SINGLE_QUOTED { remove_quotes s }
  | s=LOWER_WORD { s }

atomic_defined_word:
  | w=DOLLAR_WORD { Symbol.mk_const w }

atomic_system_word:
  | w=DOLLAR_DOLLAR_WORD { Symbol.mk_const w }

%%


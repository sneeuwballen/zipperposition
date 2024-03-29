

(* This file is free software, part of tip-parser. See file "license" for more details. *)

(** {1 Parser for TIP} *)

(* vim:SyntasticToggleMode:
   vim:set ft=yacc: *)

%{
  open Logtk

  open struct
    module A = Tip_ast
    module Loc = ParseLocation
  end

%}

%token EOI

%token LEFT_PAREN
%token RIGHT_PAREN

%token BOOL
%token PAR
%token ARROW

%token TRUE
%token FALSE
%token OR
%token AND
%token DISTINCT
%token NOT
%token EQ
%token IF
%token MATCH
%token UNDERSCORE
%token FUN
%token LET
%token AS
%token AT

%token EXCL
%token NAMED
%token PATTERN

%token DATA
%token DATAS
%token ASSERT
%token LEMMA
%token PROVE
%token FORALL
%token EXISTS
%token DECLARE_SORT
%token DECLARE_CONST
%token DECLARE_FUN
%token DEFINE_FUN
%token DEFINE_FUN_REC
%token DEFINE_FUNS_REC
%token CHECK_SAT
%token EXIT

%token <string>IDENT
%token <string>QUOTED

%start <Tip_ast.term> parse_term
%start <Tip_ast.ty> parse_ty
%start <Tip_ast.statement> parse
%start <Tip_ast.statement list> parse_list

%%

parse_list: l=stmt* EOI {l}
parse: t=stmt EOI { t }
parse_term: t=term EOI { t }
parse_ty: t=ty EOI { t }

cstor_arg:
  | LEFT_PAREN name=IDENT ty=ty RIGHT_PAREN { name, ty }

cstor:
  | LEFT_PAREN c=IDENT RIGHT_PAREN { A.mk_cstor c [] }
  | LEFT_PAREN c=IDENT l=cstor_arg+ RIGHT_PAREN
    { A.mk_cstor c l }

dataselem:
  | LEFT_PAREN s=IDENT l=cstor+ RIGHT_PAREN { s,l }

fun_decl_mono:
  | LEFT_PAREN args=ty* RIGHT_PAREN
    ret=ty
    { args, ret }

fun_decl:
  | f=IDENT tup=fun_decl_mono { let args, ret = tup in [], f, args, ret }
  | f=IDENT
    LEFT_PAREN
      PAR
      LEFT_PAREN tyvars=tyvar* RIGHT_PAREN
      LEFT_PAREN tup=fun_decl_mono RIGHT_PAREN
    RIGHT_PAREN
    { let args, ret = tup in tyvars, f, args, ret }

const_ty:
  | ty=ty { [], ty }
  | LEFT_PAREN
      PAR
      LEFT_PAREN tyvars=tyvar* RIGHT_PAREN
      ty=ty
    RIGHT_PAREN
    { tyvars, ty }

fun_def_ty:
  | LEFT_PAREN args=typed_var* RIGHT_PAREN
    ret=ty
    { args, ret }

fun_def:
  | f=IDENT
    ty=fun_def_ty
    { let args, ret = ty in [], f, args, ret }
  | f=IDENT
    LEFT_PAREN
      PAR
      LEFT_PAREN l=tyvar* RIGHT_PAREN
      LEFT_PAREN ty=fun_def_ty RIGHT_PAREN
    RIGHT_PAREN
    { let args, ret = ty in l, f, args, ret }

fun_rec:
  | tup=fun_def
    body=term
    {
      let l, f, args, ret = tup in
      A.mk_fun_rec ~ty_vars:l f args ret body
    }

funs_rec:
  | LEFT_PAREN tup=fun_def RIGHT_PAREN
    {
      let l, f, args, ret = tup in
      A.mk_fun_decl ~ty_vars:l f args ret
    }

prove:
  | LEFT_PAREN
      PAR LEFT_PAREN tyvars=tyvar+ RIGHT_PAREN t=term
    RIGHT_PAREN
  { tyvars, t }
  | t=term
  { [], t }

stmt:
  | LEFT_PAREN ASSERT t=term RIGHT_PAREN
    {
      let loc = Loc.mk_pos $startpos $endpos in
      A.assert_ ~loc t
    }
  | LEFT_PAREN LEMMA t=term RIGHT_PAREN
    {
      let loc = Loc.mk_pos $startpos $endpos in
      A.lemma ~loc t
    }
  | LEFT_PAREN DECLARE_SORT s=IDENT n=IDENT RIGHT_PAREN
    {
      let loc = Loc.mk_pos $startpos $endpos in
      try
        let n = int_of_string n in
        A.decl_sort ~loc s ~arity:n
      with Failure _ ->
        A.parse_errorf ~loc "expected arity to be an integer, not `%s`" n
    }
  | LEFT_PAREN DATA name=IDENT
      LEFT_PAREN
        PAR LEFT_PAREN tyvars=tyvar* RIGHT_PAREN
        LEFT_PAREN cstors=cstor+ RIGHT_PAREN
      RIGHT_PAREN
    RIGHT_PAREN
    {
      let loc = Loc.mk_pos $startpos $endpos in
      A.data ~loc tyvars name cstors
    }
  | LEFT_PAREN DATA name=IDENT
      LEFT_PAREN cstors=cstor+ RIGHT_PAREN
    RIGHT_PAREN
    {
      let loc = Loc.mk_pos $startpos $endpos in
      A.data ~loc [] name cstors
    }
  | LEFT_PAREN DATAS
      LEFT_PAREN vars=tyvar* RIGHT_PAREN
      LEFT_PAREN l=dataselem+ RIGHT_PAREN
    RIGHT_PAREN
    {
      let loc = Loc.mk_pos $startpos $endpos in
      A.datas ~loc vars l
    }
  | LEFT_PAREN DECLARE_FUN tup=fun_decl RIGHT_PAREN
    {
      let loc = Loc.mk_pos $startpos $endpos in
      let tyvars, f, args, ret = tup in
      A.decl_fun ~loc ~tyvars f args ret
    }
  | LEFT_PAREN DECLARE_CONST f=IDENT const_ty=const_ty RIGHT_PAREN
    {
      let loc = Loc.mk_pos $startpos $endpos in
      let tyvars, ty = const_ty in
      A.decl_fun ~loc ~tyvars f [] ty
    }
  | LEFT_PAREN DEFINE_FUN f=fun_rec RIGHT_PAREN
    {
      let loc = Loc.mk_pos $startpos $endpos in
      A.fun_rec ~loc f
    }
  | LEFT_PAREN
    DEFINE_FUN_REC
    f=fun_rec
    RIGHT_PAREN
    {
      let loc = Loc.mk_pos $startpos $endpos in
      A.fun_rec ~loc f
    }
  | LEFT_PAREN
    DEFINE_FUNS_REC
      LEFT_PAREN decls=funs_rec+ RIGHT_PAREN
      LEFT_PAREN bodies=term+ RIGHT_PAREN
    RIGHT_PAREN
    {
      let loc = Loc.mk_pos $startpos $endpos in
      A.funs_rec ~loc decls bodies
    }
  | LEFT_PAREN
    PROVE
    tup=prove
    RIGHT_PAREN
    {
      let loc = Loc.mk_pos $startpos $endpos in
      let ty_vars, f = tup in
      A.prove ~loc ~ty_vars f
    }
  | LEFT_PAREN CHECK_SAT RIGHT_PAREN
    {
      let loc = Loc.mk_pos $startpos $endpos in
      A.check_sat ~loc ()
    }
  | LEFT_PAREN EXIT RIGHT_PAREN
    {
      let loc = Loc.mk_pos $startpos $endpos in
      A.assert_ ~loc (A.true_)
    }
  | error
    {
      let loc = Loc.mk_pos $startpos $endpos in
      A.parse_errorf ~loc "expected statement"
    }

var:
  | s=IDENT { s }
tyvar:
  | s=IDENT { s }

ty:
  | BOOL { A.ty_bool }
  | s=IDENT { A.ty_const s }
  | LEFT_PAREN s=IDENT args=ty+ RIGHT_PAREN
    { A.ty_app s args }
  | LEFT_PAREN ARROW tup=ty_arrow_args RIGHT_PAREN
    {
      let args, ret = tup in
      A.ty_arrow_l args ret }

ty_arrow_args:
  | a=ty ret=ty { [a], ret }
  | a=ty tup=ty_arrow_args { a :: fst tup, snd tup }

typed_var:
  | LEFT_PAREN s=IDENT ty=ty RIGHT_PAREN { s, ty }

case:
  | LEFT_PAREN
      c=IDENT
      rhs=term
    RIGHT_PAREN
    { A.Match_case (c, [], rhs) }
  | LEFT_PAREN
      LEFT_PAREN c=IDENT vars=var+ RIGHT_PAREN
      rhs=term
    RIGHT_PAREN
    { A.Match_case (c, vars, rhs) }
  | LEFT_PAREN
     UNDERSCORE rhs=term
    RIGHT_PAREN
    { A.Match_default rhs }

binding:
  | LEFT_PAREN v=var t=term RIGHT_PAREN { v, t }

term:
  | TRUE { A.true_ }
  | FALSE { A.false_ }
  | s=QUOTED { A.const s }
  | s=IDENT { A.const s }
  | t=composite_term { t }
  | t=term_w_attributes { t }
  | error
    {
      let loc = Loc.mk_pos $startpos $endpos in
      A.parse_errorf ~loc "expected term"
    }

attr:
  | NAMED IDENT { () }
  | PATTERN term { () }

term_w_attributes:
  | LEFT_PAREN EXCL t=term attr+ RIGHT_PAREN { t }

composite_term:
  | LEFT_PAREN t=term RIGHT_PAREN { t }
  | LEFT_PAREN IF a=term b=term c=term RIGHT_PAREN { A.if_ a b c }
  | LEFT_PAREN OR l=term+ RIGHT_PAREN { A.or_ l }
  | LEFT_PAREN AND l=term+ RIGHT_PAREN { A.and_ l }
  | LEFT_PAREN NOT t=term RIGHT_PAREN { A.not_ t }
  | LEFT_PAREN DISTINCT l=term+ RIGHT_PAREN { A.distinct l }
  | LEFT_PAREN EQ a=term b=term RIGHT_PAREN { A.eq a b }
  | LEFT_PAREN ARROW a=term b=term RIGHT_PAREN { A.imply a b }
  | LEFT_PAREN f=IDENT args=term+ RIGHT_PAREN { A.app f args }
  | LEFT_PAREN UNDERSCORE f=IDENT args=term+ RIGHT_PAREN { A.app f args }
  | LEFT_PAREN UNDERSCORE f=IDENT ty_args=ty* args=term* RIGHT_PAREN { A.app_poly f ty_args args }
  | LEFT_PAREN f=composite_term args=term+ RIGHT_PAREN { A.ho_app_l f args }
  | LEFT_PAREN AT f=term arg=term RIGHT_PAREN { A.ho_app f arg }
  | LEFT_PAREN
      MATCH
      lhs=term
      LEFT_PAREN
        l=case+
      RIGHT_PAREN
    RIGHT_PAREN
    { A.match_ lhs l }
  | LEFT_PAREN
      FUN
      LEFT_PAREN vars=typed_var+ RIGHT_PAREN
      body=term
    RIGHT_PAREN
    { A.fun_l vars body }
  | LEFT_PAREN
      LET
      LEFT_PAREN l=binding+ RIGHT_PAREN
      r=term
    RIGHT_PAREN
    { A.let_ l r }
  | LEFT_PAREN AS t=term ty=ty RIGHT_PAREN
    { A.cast t ~ty }
  | LEFT_PAREN FORALL LEFT_PAREN vars=typed_var+ RIGHT_PAREN
    f=term
    RIGHT_PAREN
    { A.forall vars f }
  | LEFT_PAREN EXISTS LEFT_PAREN vars=typed_var+ RIGHT_PAREN
    f=term
    RIGHT_PAREN
    { A.exists vars f }

%%

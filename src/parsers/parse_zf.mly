
(* This file is free software, part of Zipperposition. See file "license" for more details. *)

(** {1 Parser for Zipperposition Formulas} *)

%{
  open Libzipperposition

  module L = ParseLocation
  module A = UntypedAST
  module T = A.T

  let unquote s =
    assert (CCString.prefix ~pre:"\"" s);
    assert (CCString.suffix ~suf:"\"" s);
    let s = String.sub s 1 (String.length s-2) in
    CCString.flat_map
      (function
        | '\\' -> ""
        | c -> String.make 1 c)
      s
%}


%token EOI

%token LEFT_PAREN
%token RIGHT_PAREN
%token LEFT_BRACKET
%token RIGHT_BRACKET

%token WILDCARD
%token COMMA
%token DOT
%token COLON
%token EQDEF
%token AND

%token LOGIC_TRUE
%token LOGIC_FALSE
%token LOGIC_AND
%token LOGIC_OR
%token LOGIC_NOT
%token LOGIC_IMPLY
%token LOGIC_FORALL
%token LOGIC_EXISTS
%token LOGIC_EQ
%token LOGIC_NEQ
%token LOGIC_EQUIV

%token PROP
%token TYPE

%token ASSERT
%token DATA
%token DEF
%token VAL
%token GOAL
%token REWRITE
%token INCLUDE

%token ARROW
%token PI
%token VERTICAL_BAR

%token AC
%token NAME

%token <string> LOWER_WORD
%token <string> UPPER_WORD
%token <string> QUOTED

%start <Libzipperposition.UntypedAST.statement> parse_statement
%start <Libzipperposition.UntypedAST.statement list> parse_statement_list
%start <Libzipperposition.UntypedAST.term> parse_term
%start <Libzipperposition.UntypedAST.ty> parse_ty


%%

parse_statement: s=statement EOI {s}
parse_term: t=term EOI {t}
parse_ty: t=term EOI {t}
parse_statement_list: l=list(statement) EOI { l }

/* variable without a location */
raw_var:
  | w=LOWER_WORD { w }
  | w=UPPER_WORD { w }

typed_var:
  | v=raw_var { T.V v, None }
  | WILDCARD { T.Wildcard, None }
  | LEFT_PAREN v=raw_var COLON t=term RIGHT_PAREN { T.V v, Some t }

typed_ty_var:
  | v=raw_var { T.V v, None }
  | v=raw_var COLON TYPE { T.V v, Some T.tType  }
  | LEFT_PAREN v=raw_var COLON TYPE RIGHT_PAREN { T.V v, Some T.tType }

var:
  | WILDCARD { T.wildcard }
  | v=raw_var
    {
      let loc = L.mk_pos $startpos $endpos in
      T.var ~loc v
    }

const:
  | TYPE { T.tType }
  | PROP { T.prop }
  | LOGIC_TRUE { T.true_ }
  | LOGIC_FALSE { T.false_ }

atomic_term:
  | v=var { v }
  | t=const { t }
  | LEFT_PAREN t=term RIGHT_PAREN { t }

apply_term:
  | t=atomic_term { t }
  | t=atomic_term u=atomic_term+
    {
      let loc = L.mk_pos $startpos $endpos in
      T.app ~loc t u
    }
  | LOGIC_NOT t=apply_term
    {
      let loc = L.mk_pos $startpos $endpos in
      T.not_ ~loc t
    }

eq_term:
  | t=apply_term { t }
  | t=apply_term LOGIC_EQ u=apply_term
    {
      let loc = L.mk_pos $startpos $endpos in
      T.eq ~loc t u
    }
  | t=apply_term LOGIC_NEQ u=apply_term
    {
      let loc = L.mk_pos $startpos $endpos in
      T.neq ~loc t u
    }

and_term:
  | t=eq_term { t }
  | t=eq_term LOGIC_AND u=and_term
    {
      let loc = L.mk_pos $startpos $endpos in
      T.and_ ~loc [t; u]
    }

or_term:
  | t=and_term { t }
  | t=and_term LOGIC_OR u=or_term
    {
      let loc = L.mk_pos $startpos $endpos in
      T.or_ ~loc [t; u]
    }
  | t=and_term LOGIC_IMPLY u=or_term
    {
      let loc = L.mk_pos $startpos $endpos in
      T.imply ~loc t u
    }
  | t=and_term LOGIC_EQUIV u=or_term
    {
      let loc = L.mk_pos $startpos $endpos in
      T.equiv ~loc t u
    }

term:
  | t=or_term { t }
  | LOGIC_FORALL vars=typed_var+ DOT t=term
    {
      let loc = L.mk_pos $startpos $endpos in
      T.forall ~loc vars t
    }
  | LOGIC_EXISTS vars=typed_var+ DOT t=term
    {
      let loc = L.mk_pos $startpos $endpos in
      T.exists ~loc vars t
    }
  | t=apply_term ARROW u=term
    {
      let loc = L.mk_pos $startpos $endpos in
      T.fun_ty ~loc [t] u
    }
  | PI vars=typed_ty_var+ DOT t=term
    {
      let loc = L.mk_pos $startpos $endpos in
      T.forall_ty ~loc vars t
    }
  | error
    {
      let loc = L.mk_pos $startpos $endpos in
      UntypedAST.error loc "expected term"
    }

constructor:
  | v=raw_var l=atomic_term* { v, l }

constructors:
  | VERTICAL_BAR? l=separated_nonempty_list(VERTICAL_BAR, constructor) { l }

type_def:
  | t=raw_var vars=raw_var* EQDEF l=constructors
    {
      {A. data_name=t; data_vars=vars; data_cstors=l; }
    }

mutual_types:
  | l=separated_nonempty_list(AND, type_def) { l }

attr:
  | AC { A.A_AC }
  | NAME COLON n=raw_var { A.A_name n }

attrs:
  | LEFT_BRACKET l=separated_nonempty_list(COMMA, attr) RIGHT_BRACKET
    { l }
  | { [] }

statement:
  | INCLUDE s=QUOTED DOT
    {
      let loc = L.mk_pos $startpos $endpos in
      let s = unquote s in
      A.include_ ~attrs:[] ~loc s
    }
  | VAL a=attrs v=raw_var COLON t=term DOT
    {
      let loc = L.mk_pos $startpos $endpos in
      A.decl ~attrs:a ~loc v t
    }
  | DEF a=attrs v=raw_var COLON t=term EQDEF u=term DOT
    {
      let loc = L.mk_pos $startpos $endpos in
      A.def ~attrs:a ~loc v t u
    }
  | REWRITE a=attrs t=term DOT
    {
      let loc = L.mk_pos $startpos $endpos in
      A.rewrite ~attrs:a ~loc t
    }
  | ASSERT a=attrs t=term DOT
    {
      let loc = L.mk_pos $startpos $endpos in
      A.assert_ ~attrs:a ~loc t
    }
  | GOAL a=attrs t=term DOT
    {
      let loc = L.mk_pos $startpos $endpos in
      A.goal ~attrs:a ~loc t
    }
  | DATA a=attrs l=mutual_types DOT
    {
      let loc = L.mk_pos $startpos $endpos in
      A.data ~attrs:a ~loc l
    }
  | error
    {
      let loc = L.mk_pos $startpos $endpos in
      UntypedAST.error loc "expected statement"
    }

%%


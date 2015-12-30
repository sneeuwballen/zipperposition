
(* This file is free software, part of Zipperposition. See file "license" for more details. *)

(** {1 TPTP Parser} *)

%{
  open Libzipperposition

  module L = ParseLocation
  module PT = STerm
  module A = Ast_tptp

  let remove_quotes s =
    assert (s.[0] = '\'' && s.[String.length s - 1] = '\'');
    String.sub s 1 (String.length s - 2)
%}

%token EOI

%token DOT
/* %token SEMICOLUMN */
%token COMMA
%token LEFT_PAREN
%token RIGHT_PAREN
%token LEFT_BRACKET
%token RIGHT_BRACKET

%token FOF
%token CNF
%token TFF
%token INCLUDE

%token NOT

%token COLUMN
%token STAR
%token ARROW
%token FORALL_TY  /* quantification on types */
%token TYPE_TY  /* tType */
%token WILDCARD  /* $_ */

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

%token TRUE
%token FALSE

%token FORALL
%token EXISTS

%token UNDERSCORE

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

%start <Libzipperposition.STerm.t> parse_term
%start <Libzipperposition.STerm.t> parse_formula
%start <Libzipperposition.STerm.t Ast_tptp.declaration> parse_declaration
%start <Libzipperposition.STerm.t Ast_tptp.declaration list> parse_declarations
%start <Libzipperposition.STerm.t list list> parse_answer_tuple

%%

/* top-level */

parse_term: t=term EOI { t }
parse_formula: f=fof_formula EOI { f }
parse_declaration: d=declaration EOI { d }
parse_declarations: l=declarations EOI { l }
parse_answer_tuple: t=answer_tuples EOI { t }

/* TPTP grammar */

declarations:
  | l=declaration* { l }

declaration:
  | FOF LEFT_PAREN name=name COMMA role=role COMMA f=fof_formula info=annotations RIGHT_PAREN DOT
    { A.FOF (name, role, f, info) }
  | TFF LEFT_PAREN name=name COMMA role=role COMMA f=fof_formula info=annotations RIGHT_PAREN DOT
    { A.TFF (name, role, f, info) }
  | TFF LEFT_PAREN name=name COMMA role COMMA tydecl=type_decl info=annotations RIGHT_PAREN DOT
    { let s, ty = tydecl in
      match ty.PT.term with
      | PT.AppBuiltin (Builtin.TType, [])
      | PT.AppBuiltin
          (Builtin.Arrow,
           {PT.term=PT.AppBuiltin (Builtin.TType,[]);_} :: _) ->
             (* declare a new type symbol *)
             A.NewType (name, ID.make s, ty, info)
      | _ -> A.TypeDecl (name, ID.make s, ty, info)
    }
  | CNF LEFT_PAREN name=name COMMA role=role COMMA c=cnf_formula info=annotations RIGHT_PAREN DOT
    { A.CNF (name, role, c, info) }
  | INCLUDE LEFT_PAREN x=SINGLE_QUOTED RIGHT_PAREN DOT
    { A.Include (remove_quotes x) }
  | INCLUDE LEFT_PAREN x=SINGLE_QUOTED COMMA names=name_list RIGHT_PAREN DOT
    { A.IncludeOnly (x, names) }
  | error
    {
      let loc = L.mk_pos $startpos $endpos in
      raise (Ast_tptp.ParseError loc)
    }

role: w=LOWER_WORD { Ast_tptp.role_of_string w }

answer_tuples:
  | LEFT_BRACKET l=separated_nonempty_list(VLINE,answer_tuple) RIGHT_BRACKET
    { List.fold_left  (* remove underscores *)
        (fun acc opt -> match opt with | None -> acc | Some tup -> tup :: acc)
        [] l  }

answer_tuple:
  | LEFT_BRACKET l=separated_nonempty_list(COMMA,term) RIGHT_BRACKET { Some l }
  | UNDERSCORE { None }

type_decl:
  | LEFT_PAREN tydecl=type_decl RIGHT_PAREN { tydecl }
  | s=atomic_word COLUMN ty=tff_quantified_type { s, ty }

cnf_formula:
  | LEFT_PAREN c=disjunction RIGHT_PAREN { c }
  | c=disjunction { c }

disjunction:
  | l=separated_nonempty_list(VLINE, literal) { l }

literal:
  | f=atomic_formula { f }
  | NOT f=atomic_formula
    {
      let loc = L.mk_pos $startpos $endpos in
      PT.not_ ~loc f
    }

fof_formula:
  | fof_logic_formula { $1 }
  | fof_sequent { $1 }

fof_sequent:
  | l=fof_tuple GENTZEN_ARROW r=fof_tuple
    { (* TODO accurate locs for subterms *)
      let loc = L.mk_pos $startpos $endpos in
      PT.imply ~loc (PT.and_ ~loc l) (PT.or_ ~loc r)
    }
  | LEFT_PAREN seq=fof_sequent RIGHT_PAREN { seq }

fof_tuple:
  LEFT_BRACKET l=separated_list(COMMA, fof_logic_formula) RIGHT_BRACKET { l }

fof_logic_formula:
  | f=fof_unitary_formula { f }
  | l=fof_logic_formula o=binary_connective r=fof_logic_formula
    {
      let loc = L.mk_pos $startpos $endpos in
      o ?loc:(Some loc) l r
    }

fof_unitary_formula:
  | fof_quantified_formula { $1 }
  | fof_unary_formula { $1 }
  | atomic_formula { $1 }
  | LEFT_PAREN f=fof_logic_formula RIGHT_PAREN { f }

fof_quantified_formula:
  | q=fol_quantifier LEFT_BRACKET vars=typed_vars RIGHT_BRACKET COLUMN f=fof_unitary_formula
    {
      let loc = L.mk_pos $startpos $endpos in
      q ~loc vars f
    }

fof_unary_formula:
  | o=unary_connective f=fof_unitary_formula
    {
     let loc = L.mk_pos $startpos $endpos in
     o ~loc f
    }

%inline binary_connective:
  | EQUIV { PT.equiv }
  | IMPLY { PT.imply }
  | LEFT_IMPLY { fun ?loc l r -> PT.imply ?loc r l }
  | XOR { PT.xor }
  | NOTVLINE { fun ?loc x y -> PT.not_ ?loc (PT.or_ ?loc [x; y]) }
  | NOTAND { fun ?loc x y -> PT.not_ ?loc (PT.and_ ?loc [x; y]) }
  | AND { fun ?loc x y -> PT.and_ ?loc [x;y] }
  | VLINE { fun ?loc x y -> PT.or_ ?loc [x;y] }
%inline fol_quantifier:
  | FORALL { PT.forall }
  | EXISTS { PT.exists }
%inline unary_connective:
  | NOT { PT.not_ }

atomic_formula:
  | TRUE { PT.true_ }
  | FALSE { PT.false_ }
  | l=term o=infix_connective r=term { o l r }
  | t=function_term
    {
      let loc = L.mk_pos $startpos $endpos in
      PT.at_loc ~loc t
    }

%inline infix_connective:
  | EQUAL { PT.eq }
  | NOT_EQUAL { PT.neq }

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
  | s=constant { s }
  | f=functor_ LEFT_PAREN args=arguments RIGHT_PAREN
    {
      let loc = L.mk_pos $startpos $endpos in
      PT.app ~loc f args
    }

constant:
  | s=atomic_word { PT.const s }
  | s=atomic_defined_word { s }

functor_:
  | f=atomic_word { PT.const f }

defined_term:
  | t=defined_atom { t }
  | t=defined_atomic_term { t }

defined_atom:
  | n=INTEGER { PT.int_ (Z.of_string n) }
  | n=RATIONAL { PT.rat (Q.of_string n) }
  | REAL {
      let loc = L.mk_pos $startpos $endpos in
      raise (Ast_tptp.ParseError loc)
    }
  | s=DISTINCT_OBJECT
    {
      let loc = L.mk_pos $startpos $endpos in
      PT.const ~loc s
    }

defined_atomic_term:
  | t=defined_plain_term { t }
  /* | defined_infix_term { $1 } */

defined_plain_term:
  | s=defined_constant { s }
  | s=DOLLAR_WORD
    {
      let loc = L.mk_pos $startpos $endpos in
      match Builtin.TPTP.of_string s with
      | None -> raise (Ast_tptp.ParseError loc)
      | Some b -> PT.builtin ~loc b
    }
  | f=defined_functor LEFT_PAREN args=arguments RIGHT_PAREN
    {
      let loc = L.mk_pos $startpos $endpos in
      PT.app ~loc f args
    }
  | s=DOLLAR_WORD LEFT_PAREN args=arguments RIGHT_PAREN
    {
      let loc = L.mk_pos $startpos $endpos in
      match Builtin.TPTP.of_string s with
      | None -> raise (Ast_tptp.ParseError loc)
      | Some b -> PT.app_builtin ~loc b args
    }

defined_constant: t=defined_functor { t }
defined_functor: s=atomic_defined_word { s }

system_term:
  | c=system_constant { c }
  | f=system_functor LEFT_PAREN args=arguments RIGHT_PAREN
    {
      let loc = L.mk_pos $startpos $endpos in
      PT.app ~loc f args
    }

system_constant: t=system_functor { t }
system_functor: s=atomic_system_word { s }

typed_var:
  | v=UPPER_WORD COLUMN ty=tff_atom_type { v, Some ty }
  | v=UPPER_WORD { v, None }

typed_vars:
  | l=separated_nonempty_list(COMMA, typed_var) { l }

/* prenex quantified type */
tff_quantified_type:
  | ty=tff_type { ty }
  | FORALL_TY LEFT_BRACKET vars=tff_ty_vars RIGHT_BRACKET COLUMN ty=tff_quantified_type
    { PT.forall_ty vars ty }

/* general type, without quantifiers */
tff_type:
  | ty=tff_atom_type { ty }
  | l=tff_atom_type ARROW r=tff_atom_type
    { PT.fun_ty [l] r }
  | LEFT_PAREN args=tff_ty_args RIGHT_PAREN ARROW r=tff_atom_type
    { PT.fun_ty args r }

tff_atom_type:
  | v=variable { v }
  | w=type_const { w }
  | w=type_const LEFT_PAREN l=separated_nonempty_list(COMMA, tff_type) RIGHT_PAREN
    { PT.app w l }
  | TYPE_TY { PT.tType }
  | LEFT_PAREN ty=tff_type RIGHT_PAREN { ty }

tff_ty_args:
  | ty=tff_atom_type { [ty] }
  | hd=tff_atom_type STAR tl=tff_ty_args { hd :: tl }

tff_ty_vars:
  | l=separated_nonempty_list(COMMA, tff_ty_var) { l }

tff_ty_var:
  | v=UPPER_WORD COLUMN TYPE_TY { v, Some PT.tType }

type_const:
  | WILDCARD { PT.wildcard }
  | w=LOWER_WORD { PT.const w }
  | t=defined_ty { t }

arguments: separated_nonempty_list(COMMA, term) { $1 }

variable:
  | x=UPPER_WORD
    {
      let loc = L.mk_pos $startpos $endpos in
      PT.var ~loc x
    }

atomic_word:
  | s=SINGLE_QUOTED { remove_quotes s }
  | s=LOWER_WORD { s }

atomic_defined_word:
  | WILDCARD { PT.wildcard }
  | t=defined_ty { t }

defined_ty:
  | w=DOLLAR_WORD
    { match w with
      | "$i" -> PT.term
      | "$o" -> PT.prop
      | "$tType" -> PT.tType
      | "$int" -> PT.ty_int
      | "$rat" -> PT.ty_rat
      | _ ->
          let loc = L.mk_pos $startpos $endpos in
          raise (Ast_tptp.ParseError loc)
    }

atomic_system_word:
  | w=DOLLAR_DOLLAR_WORD { PT.const w }

name_list:
  l=separated_list(COMMA, name) { l }

name:
  | w=atomic_word { Ast_tptp.NameString w }
  | i=INTEGER { Ast_tptp.NameInt (int_of_string i) }

annotations:
  | { [] }
  | COMMA l=separated_list(COMMA, general_term) { l }

general_term:
  | general_data { $1 }
  | l=general_data COLUMN r=general_term { Ast_tptp.GColumn (l, r) }
  | general_list { $1 }

general_data:
  | w=atomic_word { Ast_tptp.GString w }
  | general_function { $1 }
  | i=INTEGER { Ast_tptp.GInt (int_of_string i) }
  | v=UPPER_WORD { Ast_tptp.GVar v }
  | w=DISTINCT_OBJECT { Ast_tptp.GString w }

general_function:
  | f=atomic_word LEFT_PAREN l=separated_nonempty_list(COMMA, general_term) RIGHT_PAREN
    { Ast_tptp.GNode (f, l) }

general_list:
  | LEFT_BRACKET l=separated_list(COMMA, general_term) RIGHT_BRACKET
    { Ast_tptp.GList l }

%%

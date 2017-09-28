/*  Copyright 2005 INRIA  */
/*  Copyright 2014 Ali Assaf */
/*  Copyright 2014 Raphaël Cauderlier */
%{
open Logtk
module T = Ast_dk
module L = ParseLocation
%}
%token <string> ID QID NUMBER
%token COLON DOT DOUBLE_ARROW DEF ARROW
%token TYPE TERM PROOF CCARR PROP
%token LPAREN RPAREN EOF
%token LBRACK COMMA RBRACK REW DEFKW
%token TRUE FALSE NOT AND OR IMP EQV ALL EX ALL_TYPE EX_TYPE ISTRUE EQUAL

%token MUSTUSE
%token BEGINPROOF
%token BEGIN_TYPEALIAS
%token BEGIN_TY
%token BEGIN_VAR
%token BEGIN_HYP
%token END_TYPEALIAS
%token END_VAR
%token END_HYP
%token <string> BEGINNAME
%token BEGINHEADER
%token ENDPROOF

%start file
%type <Ast_dk.statement list> file
%type <Ast_dk.ty> typ
%type <Ast_dk.term> term
%type <Ast_dk.ty> term_simple

%%

file:
| b=body EOF { b }
| BEGINPROOF h=proofheader* b=body ENDPROOF EOF
{ let h = CCList.filter_map CCFun.id h in
  CCList.append h b }
| error {
  let loc = L.mk_pos $startpos $endpos in
  UntypedAST.errorf loc "expected problem"
}

body:
| l=hyp_def* g=goal { CCList.append l [g] }

goal:
| name=ID COLON PROOF t=term DOT { T.mk_goal ~name t }

proofheader:
  | BEGINNAME ID { None }
  | BEGINHEADER { None }
  | BEGIN_TY id=ID { Some (T.mk_ty_decl id T.tType) }
  | BEGIN_TYPEALIAS f=ID DEF ty=type_simple END_TYPEALIAS
      { (* Type aliases are substituted in the parser *)
        (* This does not work because it does not substitute
           the alias in already-parsed input.
           TODO: give it to the typer. *)
        T.add_alias f ty;
        None
      }
  | BEGIN_VAR ID COLON typ END_VAR { None }
  | BEGIN_HYP ID COLON PROOF term_simple END_HYP { None }
  | error {
  let loc = L.mk_pos $startpos $endpos in
  UntypedAST.errorf loc "expected proofheader"
}

qid:
| x=QID { T.find_alias x ~or_else:(T.const x) }
| x=ID { T.find_alias x ~or_else:(T.const x) }

term_simple:
| x=qid { x }
| x=NUMBER { T.mk_int x } (* TODO: use nat *)
| TRUE { T.true_ }
| FALSE { T.false_ }
| NOT t=term_simple { T.not_ t }
| AND t=term_simple u=term_simple { T.and_ [t;u] }
| OR  t=term_simple u=term_simple { T.or_ [t;u] }
| IMP t=term_simple u=term_simple { T.imply t u }
| EQV t=term_simple u=term_simple { T.equiv t u }
| ALL ty=type_simple LPAREN x=ID COLON complex_type DOUBLE_ARROW body=term_simple RPAREN
  { T.forall [ (T.mk_var x, Some ty) ] body }
| EX ty=type_simple LPAREN x=ID COLON complex_type DOUBLE_ARROW body=term_simple RPAREN
  { T.exists [ (T.mk_var x, Some ty) ] body }
| ALL_TYPE LPAREN x=ID COLON TYPE DOUBLE_ARROW body=term_simple RPAREN
  { T.forall [ (T.mk_var x, Some T.tType) ] body }
| EX_TYPE LPAREN x=ID COLON TYPE DOUBLE_ARROW body=term_simple RPAREN
  { T.exists [ (T.mk_var x, Some T.tType) ] body }
| ISTRUE t=term_simple
  { T.mk_app
      (T.mk_const "dk_logic.ebP"
        (T.mk_arrow (T.const "basics.bool__t") T.ty_prop))
      [t]
    }
| EQUAL ty=type_simple t=term_simple u=term_simple { T.eq (T.cast t ty) u }
| LPAREN t=term RPAREN { t }
| x=ID COLON ty=typ DOUBLE_ARROW body=term_simple
  { T.mk_fun [ (T.mk_var x, Some ty) ] body }
| x=ID DEF t=term DOUBLE_ARROW u=term_simple
  { T.let_ [ (T.mk_var x, t) ] u }
| CCARR a=type_simple b=type_simple
  { T.mk_arrow a b }

term:
| t=term_simple { t }
| f=term_simple l=term_simple+ {
      (* The "applicatives" rules contains a list of
         arguments in reverse order.
         This list is not empty. *)
      T.mk_app f l
    }
| error {
  let loc = L.mk_pos $startpos $endpos in
  UntypedAST.errorf loc "expected term"
}

type_qid:
| x=ID { T.find_alias x ~or_else:(T.mk_const_t x) }
| x=QID { T.find_alias x ~or_else:(T.mk_const_t x) }

type_simple:
| ty=type_qid { ty }
| LPAREN ty=pre_typ RPAREN { ty }

pre_typ:
| f=type_simple l=type_simple* { T.mk_app f l }
| CCARR a=type_simple b=type_simple { T.mk_arrow a b }

typ:
| TERM ty=type_simple { ty }
| PROP { T.ty_prop }

complex_type :
| ty=typ { ty }
| LPAREN ty=arrow_type RPAREN { ty }

arrow_type :
| ty=typ { ty }
| a=complex_type ARROW b=arrow_type { T.mk_arrow a b }
| x=ID COLON TYPE ARROW body=arrow_type
  { T.forall_ty [T.V x, None] body }

kind :
| TYPE { T.tType }
| ID COLON TYPE ARROW ty=kind { T.mk_arrow T.tType ty }

declared_or_defined_id:
| x=ID { x }
| x=QID { x }

hyp_def:
| id=ID COLON ty=kind DOT { T.mk_ty_decl id ty }
| id=QID COLON ty=kind DOT { T.mk_ty_decl id ty }
| name=ID COLON PROOF t=term DOT { T.mk_assert ~name t }
| name=QID COLON PROOF t=term DOT { T.mk_assert ~name t }
| id=ID COLON ty=arrow_type DOT { T.mk_ty_decl id ty }
| id=QID COLON ty=arrow_type DOT { T.mk_ty_decl id ty }
| DEFKW id=ID COLON ty=arrow_type DOT { T.mk_ty_decl id ty }
| DEFKW id=QID COLON ty=arrow_type DOT { T.mk_ty_decl id ty }
| DEFKW id=ID COLON ty=typ DEF body=term DOT
  { T.mk_def id ty body }
| DEFKW id=QID COLON ty=typ DEF body=term DOT
  { T.mk_def id ty body }
| DEFKW id=declared_or_defined_id args=compact_arg+ COLON ty_ret=typ DEF body=term DOT
  {
    let ty_args = List.map snd args in
    let ty = T.mk_arrow_l ty_args ty_ret in
    let l = List.map (fun (id,ty) -> T.V id, Some ty) args in
    T.mk_def id ty (T.mk_fun l body)
  }
| env=env lhs=term REW rhs=term DOT
  {
    let t = T.mk_forall env (T.eq lhs rhs) in
    T.mk_rewrite t
  }

compact_arg:
| LPAREN id=ID COLON ty=arrow_type RPAREN { id, ty }

env_decl:
| id=ID COLON ty=arrow_type { T.mk_var id, Some ty }
| id=ID COLON TYPE { T.mk_var id, Some T.tType }
| id=ID { T.mk_var id, None }

env:
| LBRACK l=separated_list(COMMA, env_decl) RBRACK { l }


%%

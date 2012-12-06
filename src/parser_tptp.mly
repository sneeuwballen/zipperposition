/*
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
*/


%{

  (** TSTP parser *)

  open Const

  (** int counter for variables *)
  module Counter = struct
    type t = int ref
    let create_with i = ref i
    let set counter i = counter := i
    let inc counter = counter := !counter + 1
    let value counter = !counter
  end

  open Types
  open Symbols

  module T = Terms
  module C = Clauses
  module O = Orderings
  module Utils = FoUtils

  type term = Types.term
  type variable = Types.term
  type literal = Types.literal
  type clause = Types.clause

  (* includes from input *)
  let include_files: string list ref =
    ref []

  (* these are only valid for the current clause
     and have to be invalidated with init_clause for every new clause *)

  (* the variable id counter for the currently read term/clause *)
  let var_id_counter = 
    Counter.create_with 0
      
  (* mapping of the variable names (e.g. "X") of the currently read term/clause
     to variables. *)
  let (var_map: (string * sort * variable) list ref) =
    ref []

  (* the literals of the currently read clause *)
  let (literals: literal list ref) =
    ref []

  (** is the current clause a conjecture? *)
  let conjecture = ref false

  (* reset everything in order to parse a new term/clause *)
  let init_clause () =
    Counter.set var_id_counter 0;
    var_map := [];
    literals := [];
    conjecture := false
        
  (* gets the variables associated with a string from the variable mapping
     creates a new mapping for a new variable with the given sort *)
  let get_var ?(sort=univ_sort) (var_name: string) =
    try 
      (* way faster than List.assoc *)
      match (
        List.find
          (fun (var_name', var_sort, _) ->
             var_name = var_name' && var_sort == sort
          )
          !var_map
      ) with (_, _, t) -> t
    with
      | Not_found ->
          let new_var = 
            T.mk_var (Counter.value var_id_counter) sort
          in
            Counter.inc var_id_counter;
            var_map := (var_name, sort, new_var) :: !var_map;
            new_var

  let sort_table = Hashtbl.create 23
  (* get the infered sort for the given constant *)
  let get_sort constant =
    try
      Hashtbl.find sort_table constant
    with Not_found -> univ_sort

  let set_sort constant sort = Hashtbl.replace sort_table constant sort

  let mk_forall v t =
    (* only add the quantifier if v is a free var in t *)
    if T.var_occurs v t
      then T.mk_node forall_symbol bool_sort
        [T.mk_node lambda_symbol bool_sort [T.db_from_var t v]]
      else t

  let mk_exists v t =
    (* only add the quantifier if v is a free var in t *)
    if T.var_occurs v t
    then T.mk_node exists_symbol bool_sort
        [T.mk_node lambda_symbol bool_sort [T.db_from_var t v]]
    else t
    
%}
  
%token LEFT_PARENTHESIS
%token RIGHT_PARENTHESIS
%token LEFT_BRACKET
%token RIGHT_BRACKET
%token DOT
%token NEGATION
%token COLON
%token COMMA
%token EQUALITY
%token DISEQUALITY
%token EOI
%token FOF
%token CNF
%token THF
%token INCLUDE
%token <string> SINGLE_QUOTED
%token <string> DOLLAR_WORD
%token <string> DOLLAR_DOLLAR_WORD
%token <string> DISTINCT_OBJECT
%token <string> LOWER_WORD
%token <string> UPPER_WORD
%token <string> UNSIGNED_INTEGER
%token <string> SIGNED_INTEGER
%token <string> REAL
%token DOLLAR_TRUE
%token DOLLAR_FALSE
%token DOLLAR
%token AND
%token OR
%token FORALL
%token EXISTS
%token BIJECTION
%token XOR
%token LEFT_IMPLICATION
%token RIGHT_IMPLICATION
%token UNKNOWN

%start parse_file
%type <Types.hclause list * string list> parse_file

%start term
%type <Types.term> term

%start cnf_formula
%type <Types.literal list> cnf_formula

%%

/* start rules */

parse_file:
  | file EOI 
      {
        let clauses = $1 in
        let includes = !include_files in

        (* reset for next parser run *)
        include_files := [];
        
        clauses, includes
      }

  | EOI
      { print_endline "empty problem specification";
        raise Const.PARSE_ERROR }


/* parse rules */



file:
  | tptp_input
      { match $1 with
        | Some clause -> [clause]
        | None        -> []
      }

  | tptp_input file
      { match $1 with
        | Some clause -> clause :: $2
        | None        -> $2
      }

tptp_input:
  | annotated_formula
      { Some $1 }

  | include_
      { None }



annotated_formula:
  | fof_annotated
      { $1 }

  | cnf_annotated
      { $1 }

  | thf_annotated
      { $1 }

thf_annotated:
  | THF LEFT_PARENTHESIS name COMMA formula_role COMMA
    fof_formula annotations RIGHT_PARENTHESIS DOT
    { failwith "Parser_tptp: tfh syntax not supported." }

fof_annotated:
  | FOF LEFT_PARENTHESIS name COMMA formula_role COMMA
    fof_formula annotations RIGHT_PARENTHESIS DOT
    { 
      let clause = 
        let filename = !Const.cur_filename in  (* ugly *)
        let ord = Orderings.default_ordering () in
        let sign = not !conjecture in (* if conjecture, negate *)
        let lit = C.mk_lit ~ord $7 T.true_term sign in
        C.mk_hclause ~ord [lit] (lazy (Axiom (filename, $3))) []
      in
        init_clause ();  (* reset global state *)
        clause
    }

fof_formula:
  | binary_formula
    { $1 }
  | unitary_formula
    { $1 }


binary_formula:
  | nonassoc_binary
    { $1 }

  | assoc_binary
    { $1 }

nonassoc_binary:
  | unitary_formula binary_connective unitary_formula
    { $2 $1 $3 }

binary_connective:
  | BIJECTION
    { T.mk_equiv }
  | LEFT_IMPLICATION
    { T.mk_imply }
  | RIGHT_IMPLICATION
    { fun x y -> T.mk_imply y x }
  | XOR
    { fun x y -> T.mk_not (T.mk_equiv x y) }
  | NEGATION OR
    { fun x y -> T.mk_not (T.mk_or x y) }
  | NEGATION AND
    { fun x y -> T.mk_not (T.mk_and x y) }

assoc_binary:
  | or_formula
    { $1 }
  | and_formula
    { $1 }

or_formula:
  | unitary_formula OR more_or_formula
    { T.mk_or $1 $3 }

more_or_formula:
  | unitary_formula
    { $1 }
  | unitary_formula OR more_or_formula
    { T.mk_or $1 $3 }

and_formula:
  | unitary_formula AND more_and_formula
    { T.mk_and $1 $3 }

more_and_formula:
  | unitary_formula
    { $1 }
  | unitary_formula AND more_and_formula
    { T.mk_and $1 $3 }

unitary_formula:
  | quantified_formula
    { $1 }
  | unary_formula
    { $1 }
  | LEFT_PARENTHESIS fof_formula RIGHT_PARENTHESIS
    { $2 }
  | atomic_formula
    { $1 }

quantified_formula:
  | quantifier LEFT_BRACKET variable_list RIGHT_BRACKET
    COLON unitary_formula
    { 
      List.fold_left (fun form v -> $1 v form) $6 $3
    }

quantifier:
  | FORALL
    { mk_forall }
  | EXISTS
    { mk_exists }

variable_list:
  | variable
    { [$1] }
  | variable COMMA variable_list
    { $1 :: $3 }

unary_formula:
  | unary_connective unitary_formula
    { $1 $2 }

unary_connective:
  | NEGATION
    { T.mk_not }


cnf_annotated:
  | CNF LEFT_PARENTHESIS name COMMA formula_role COMMA
  cnf_formula annotations RIGHT_PARENTHESIS DOT
      // ignore everything except for the formula
      {
        let clause = 
          let ord = Orderings.default_ordering () in
          let filename = !Const.cur_filename in  (* ugly *)
          let c = C.mk_hclause ~ord $7 (lazy (Axiom (filename, $3))) [] in
          C.clause_of_fof ~ord c
        in
          init_clause ();
          clause
      }

formula_role:
  | LOWER_WORD
    { let role = $1 in
      (if role = "conjecture" then
        conjecture := true);
      $1
    }

annotations:
  | null
      { "" }

  | COMMA source optional_info
      { "" }



cnf_formula:
  | LEFT_PARENTHESIS disjunction RIGHT_PARENTHESIS
      { $2 }

  | disjunction
      { $1 }

disjunction:
  | literal
      { [$1] }

  | literal OR disjunction
      { $1 :: $3 }


literal:
  | atomic_formula
      { C.mk_eq ~ord:(O.default_ordering ()) $1 T.true_term }

  | NEGATION atomic_formula
      { C.mk_neq ~ord:(O.default_ordering ()) $2 T.true_term }

atomic_formula:
  | plain_atom
      { $1 }

  | defined_atom
      { $1 }

  | system_atom
      { $1 }

plain_atom:
  | plain_term_top
      { let t = T.cast $1 bool_sort in (* cast term to bool *)
        (match t.term with
        | Node (s, _) -> set_sort s bool_sort
        | Var _ -> assert false);
        t
      }

arguments:
  | term
      { [ $1 ] }

  | term COMMA arguments
      { $1 :: $3 }

defined_atom:
  | DOLLAR_TRUE
      { T.true_term }

  | DOLLAR_FALSE
      { T.false_term }

  | term EQUALITY term
      { T.mk_eq $1 $3 }
  | term DISEQUALITY term
      { T.mk_not (T.mk_eq $1 $3) }

system_atom:
  | system_term_top
      { let t = T.cast $1 bool_sort in
        (match t.term with
        | Node (s, _) -> set_sort s bool_sort
        | Var _ -> assert false);
        t
      }

term:
  | function_term
      { $1 }

  | variable
      { $1 }

function_term:
  | plain_term
      { $1 }

  | defined_term
      { $1 }

  | system_term
      { $1 }

plain_term_top:
  | constant
      { let sort = get_sort $1 in
        T.mk_const $1 sort }

  | functor_ LEFT_PARENTHESIS arguments RIGHT_PARENTHESIS
      { let sort = get_sort $1 in
        T.mk_node $1 sort $3
      }

plain_term:
  | constant
      { let sort = get_sort $1 in
        T.mk_const $1 sort }

  | functor_ LEFT_PARENTHESIS arguments RIGHT_PARENTHESIS
      { let sort = get_sort $1 in
        T.mk_node $1 sort $3
      }

constant:
  | atomic_word
      { mk_symbol $1 }

functor_:
  | atomic_word
      { mk_symbol $1 }

defined_term:
  | number
      { print_endline ("Parser_tptp: <defined_term: number> not supported: "
                      ^ $1);
        raise Const.PARSE_ERROR }

  | DISTINCT_OBJECT
      { print_endline ("Parser_tptp: <defined_term: distinct_object> not supported: " ^ $1);
        raise Const.PARSE_ERROR }

system_term_top:
  | system_constant
      { let sort = get_sort $1 in
        T.mk_const $1 sort }

  | system_functor LEFT_PARENTHESIS arguments RIGHT_PARENTHESIS
      { 
        T.mk_node $1 univ_sort $3
      }

system_term:
  | system_constant
      { let sort = get_sort $1 in
        T.mk_const $1 sort }

  | system_functor LEFT_PARENTHESIS arguments RIGHT_PARENTHESIS
      { let sort = get_sort $1 in
        T.mk_node $1 sort $3
      }

system_functor:
  | atomic_system_word
      { mk_symbol $1 }
      
system_constant:
  | atomic_system_word
      { mk_symbol $1 }



variable:
  | UPPER_WORD
      { get_var $1 }


source:
  | general_term
      { "" }

optional_info:
  | COMMA useful_info
      { "" }

  | null
      { "" }

useful_info:
  | general_term_list
      { "" }
      

include_:
  | INCLUDE LEFT_PARENTHESIS file_name formula_selection RIGHT_PARENTHESIS DOT
      { include_files := $3 :: !include_files }

formula_selection:
  | COMMA '[' name_list ']'
      { $3 }

  | null
      { [] }

name_list:
  | name
      { [$1] }

  | name COMMA name_list
      { $1 :: $3 }



general_term:
  | general_data
      { "" }

  | general_data COLON general_term
      { "" }

  | general_list
      { "" }

general_data:
  | atomic_word
      { "" }

  | atomic_word LEFT_PARENTHESIS general_arguments RIGHT_PARENTHESIS
      { "" }

  | number
      { "" }

  | DISTINCT_OBJECT
      { "" }

general_arguments:
  | general_term
      { [$1] }

  | general_term COMMA general_arguments
      { $1 :: $3 }

general_list:
  | '[' ']'
      { [] }

  | '[' general_term_list ']'
      { $2 }

general_term_list:
  | general_term
      { [$1] }

  | general_term COMMA general_term_list
      { $1 :: $3 }


name:
  | atomic_word
      { $1 }

  | UNSIGNED_INTEGER
      { $1 }

atomic_word:
  | LOWER_WORD
      { $1 }

  | SINGLE_QUOTED
      { $1 }

atomic_system_word:
  | DOLLAR_DOLLAR_WORD
      { print_endline ("Parser_tptp: <$$word> not supported: " ^ $1);
        raise Const.PARSE_ERROR  }

number:
  | REAL
      { $1 }

  | SIGNED_INTEGER
      { $1 }

  | UNSIGNED_INTEGER
      { $1 }

file_name:
  | SINGLE_QUOTED
      { let quoted = $1 in
        String.sub quoted 1 (String.length quoted - 2)
      }

null:
      { }

%%





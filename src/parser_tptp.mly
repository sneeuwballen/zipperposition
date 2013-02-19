/*
Zipperposition: a functional superposition prover for prototyping
Copyright (C) 2012 Simon Cruanes

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
  (** TSTP parser. It parses into terms *)

  open Const
  open Symbols
  open Types

  module Utils = FoUtils

  type term = Types.term
  type variable = Types.term
  type literal = Types.term
  type clause = Types.term

  (* includes from input *)
  let include_files: string list ref =
    ref []

  (* these are only valid for the current clause
     and have to be invalidated with init for every new clause *)

  (* the variable id counter for the currently read term/clause *)
  let var_id_counter = ref 0
      
  (* mapping of the variable names (e.g. "X") of the currently read term/clause
     to variables. *)
  let (var_map: (string * sort * variable) list ref) =
    ref []

  (** is the current clause a conjecture? *)
  let conjecture = ref false

  (* reset everything in order to parse a new term/clause *)
  let init () =
    var_id_counter := 0;
    var_map := [];
    conjecture := false
        
  (* gets the variables associated with a string from the variable mapping
     creates a new mapping for a new variable with the given sort *)
  let get_var ?(sort=univ_) (var_name: string) =
    try 
      (* way faster than List.assoc *)
      match
        List.find
          (fun (var_name', var_sort, _) ->
             var_name = var_name' && var_sort == sort)
          !var_map
      with (_, _, t) -> t
    with
      | Not_found ->
          let new_var = 
            Terms.mk_var !var_id_counter sort
          in
            incr var_id_counter;
            var_map := (var_name, sort, new_var) :: !var_map;
            new_var

  (** table that maps symbols into sorts *)
  let sort_table = SHashtbl.create 5

  (* Get the infered sort for the given symbol and list of arguments.
     The supposed return type can be passed. *)
  let get_sort ?(sort=univ_) symb =
    try
      SHashtbl.find sort_table symb
    with Not_found ->
      SHashtbl.replace sort_table symb sort;
      sort

  let set_sort constant sort = SHashtbl.replace sort_table constant sort
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
%token TFF
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
%token GENTZEN_ARROW
%token SLASH

%token IS
%token THEORY
%token LEMMA
%token IF
%token AND
%token GC
%token WITH

%token UNKNOWN

%start parse_file
%type <Types.sourced_term list * string list> parse_file

%start parse_formula
%type <Types.term> parse_formula

%start parse_meta
%type <Meta.KB.item list> parse_meta

%%

/* start rules */

parse_file:
  | file EOI 
      {
        let clauses = $1 in
        let includes = !include_files in

        (* reset for next parser run *)
        include_files := [];
        Const.reset ();
        
        clauses, includes
      }

  | EOI
      { Const.parse_error "empty problem specification" }

parse_formula:
  | fof_formula EOI { Const.reset(); $1 }
  | EOI { Const.parse_error "could no parse clause" }

parse_meta :
  | meta_items EOI { Const.reset (); $1 }
  | EOI { Const.reset(); [] }

/* parse rules */

file:
  | tptp_input
      { match $1 with
        | Some formula -> [formula]
        | None -> []
      }
  | tptp_input file
      { match $1 with
        | Some formula -> formula :: $2
        | None -> $2
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
  | tff_annotated
    { $1 }
  | thf_annotated
    { $1 }

thf_annotated:
  | THF LEFT_PARENTHESIS name COMMA formula_role COMMA
    fof_formula annotations RIGHT_PARENTHESIS DOT
    { failwith "Parser_tptp: tfh syntax not supported." }

tff_annotated:
  | TFF LEFT_PARENTHESIS name COMMA formula_role COMMA
    fof_formula annotations RIGHT_PARENTHESIS DOT
    { failwith "Parser_tptp: tff syntax not (yet) supported." }

fof_annotated:
  | FOF LEFT_PARENTHESIS name COMMA formula_role COMMA
    fof_formula annotations RIGHT_PARENTHESIS DOT
    { 
      let formula = 
        let filename = !Const.cur_filename in  (* ugly *)
        if !conjecture
          then Terms.mk_not $7, filename, $3
          else $7, filename, $3
      in
      init ();  (* reset global state *)
      formula
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
    { fun x y -> Terms.mk_equiv (Terms.cast x bool_) (Terms.cast y bool_) }
  | LEFT_IMPLICATION
    { fun x y -> Terms.mk_imply (Terms.cast x bool_) (Terms.cast y bool_) }
  | RIGHT_IMPLICATION
    { fun x y -> Terms.mk_imply (Terms.cast y bool_) (Terms.cast x bool_) }
  | XOR
    { fun x y -> Terms.mk_xor (Terms.cast x bool_) (Terms.cast y bool_) }
  | NEGATION OR
    { fun x y -> Terms.mk_not (Terms.mk_or (Terms.cast x bool_) (Terms.cast y bool_)) }
  | NEGATION AND
    { fun x y -> Terms.mk_not (Terms.mk_and (Terms.cast x bool_) (Terms.cast y bool_)) }

assoc_binary:
  | or_formula
    { $1 }
  | and_formula
    { $1 }

or_formula:
  | unitary_formula OR more_or_formula
    { Terms.mk_or (Terms.cast $1 bool_) (Terms.cast $3 bool_) }

more_or_formula:
  | unitary_formula
    { (Terms.cast $1 bool_) }
  | unitary_formula OR more_or_formula
    { Terms.mk_or (Terms.cast $1 bool_) (Terms.cast $3 bool_) }

and_formula:
  | unitary_formula AND more_and_formula
    { Terms.mk_and (Terms.cast $1 bool_) (Terms.cast $3 bool_) }

more_and_formula:
  | unitary_formula
    { (Terms.cast $1 bool_) }
  | unitary_formula AND more_and_formula
    { Terms.mk_and (Terms.cast $1 bool_) (Terms.cast $3 bool_) }

unitary_formula:
  | quantified_formula
    { (Terms.cast $1 bool_) }
  | unary_formula
    { (Terms.cast $1 bool_) }
  | LEFT_PARENTHESIS fof_formula RIGHT_PARENTHESIS
    { (Terms.cast $2 bool_) }
  | atomic_formula
    { (Terms.cast $1 bool_) }

quantified_formula:
  | quantifier LEFT_BRACKET variable_list RIGHT_BRACKET
    COLON unitary_formula
    { $1 $3 (Terms.cast $6 bool_) }

quantifier:
  | FORALL
    { Terms.mk_forall_var }
  | EXISTS
    { Terms.mk_exists_var }

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
    { Terms.mk_not }


cnf_annotated:
  | CNF LEFT_PARENTHESIS name COMMA formula_role COMMA
  cnf_formula annotations RIGHT_PARENTHESIS DOT
      // ignore everything except for the formula
      {
        let formula = 
          let filename = !Const.cur_filename in  (* ugly *)
          $7, filename, $3
        in
        init ();
        formula
      }

formula_role:
  | LOWER_WORD
    { let role = $1 in
      (if role = "conjecture" then
        conjecture := true);
      $1
    }
  | LEMMA { "lemma" }

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
      { (Terms.cast $1 bool_) }
  | literal OR disjunction
      { Terms.mk_or (Terms.cast $1 bool_) (Terms.cast $3 bool_) }


literal:
  | atomic_formula
      { (Terms.cast $1 bool_) }
  | NEGATION atomic_formula
      { Terms.mk_not (Terms.cast $2 bool_) }

atomic_formula:
  | plain_atom
      { $1 }
  | defined_atom
      { $1 }
  | system_atom
      { $1 }

plain_atom:
  | plain_term_top
      { let t = Terms.cast $1 bool_ in (* cast term to bool *)
        (* Some type inference now *)
        (match t.term with
        | Node (s, l) -> set_sort s (bool_ <== List.map (fun x -> x.sort) l)
        | Bind (s, l) -> set_sort s t.sort
        | Var _ | BoundVar _ -> failwith "variable at top level");
        t
      }

arguments:
  | term
      { [ $1 ] }

  | term COMMA arguments
      { $1 :: $3 }

defined_atom:
  | DOLLAR_TRUE
      { Terms.true_term }

  | DOLLAR_FALSE
      { Terms.false_term }

  | term EQUALITY term
      { Terms.mk_eq $1 $3 }
  | term DISEQUALITY term
      { Terms.mk_neq $1 $3 }

system_atom:
  | system_term_top
      { let t = Terms.cast $1 bool_ in
        (* Some type inference now *)
        (match t.term with
        | Node (s, l) -> set_sort s (bool_ <== List.map (fun x -> x.sort) l)
        | Bind (s, t') -> set_sort s t.sort
        | Var _ | BoundVar _ -> failwith "variable at top level");
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
        Terms.mk_const $1 sort }
  | functor_ LEFT_PARENTHESIS arguments RIGHT_PARENTHESIS
      { let sort = get_sort $1 in
        Terms.mk_node $1 sort $3
      }

plain_term:
  | constant
      { let sort = get_sort $1 in
        Terms.mk_const $1 sort }

  | functor_ LEFT_PARENTHESIS arguments RIGHT_PARENTHESIS
      { let sort = get_sort $1 in
        Terms.mk_node $1 sort $3
      }

constant:
  | atomic_word
      { mk_symbol $1 }

functor_:
  | atomic_word
      { mk_symbol $1 }

defined_term:
  | number
    {
      let n = Num.num_of_string $1 in
      let symbol = Symbols.mk_num n in
      let sort = match n with
        | Num.Int _ | Num.Big_int _ -> int_
        | Num.Ratio _ -> rat_
      in
      Terms.mk_const symbol sort }
  | DISTINCT_OBJECT
    { let sort = univ_ in
      let symbol = Symbols.mk_distinct $1 in
      Terms.mk_const symbol sort }

system_term_top:
  | system_constant
      { let sort = get_sort $1 in  (* FIXME: is the sort univ_ ? *)
        Terms.mk_const $1 sort }
  | system_functor LEFT_PARENTHESIS arguments RIGHT_PARENTHESIS
      { let sort = get_sort $1 in
        Terms.mk_node $1 sort $3 }

system_term:
  | system_constant
      { let sort = get_sort $1 in
        Terms.mk_const $1 sort }

  | system_functor LEFT_PARENTHESIS arguments RIGHT_PARENTHESIS
      { let sort = get_sort $1 in
        Terms.mk_node $1 sort $3 }

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
      { $1 }

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


/* Meta-prover parsing */

meta_items:
  | meta_item_dot { [$1] }
  | meta_item_dot meta_items { $1 :: $2 }

meta_item_dot:
  | meta_item DOT { $1 }

meta_item:
  | meta_theory { $1 }
  | meta_lemma { $1 }
  | meta_gc { $1 }
  | LEFT_PARENTHESIS meta_item RIGHT_PARENTHESIS { $2 }
  | meta_rule { $1 }

/*
theory_named_formula:
  | datalog_atom IS fof_formula DOT
    {
      let open Patterns in let open Theories in
      let atom_name, atom_args = $1 in
      let f = $3 in
      (* transform to hclause *)
      let ord = Orderings.default_ordering (Simple.signature [f]) in
      let ctx = {ctx_ord=ord; ctx_select=no_select; } in
      let hc = Clauses.from_simple ~ctx (f, Simple.Axiom ("theory", "theory")) in
      let hc = Superposition.basic_simplify (Clauses.clause_of_fof hc) in
      (* transform f into a pattern *)
      let rev_map = empty_rev_mapping () in
      let pc = pclause_of_clause ~rev_map hc in
      (* translate symbols of the atom *)
      let atom_args = List.map
        (fun s ->
          try SHashtbl.find rev_map.rm_symbol s
          with Not_found -> failwith ("symbol not found " ^ (name_symbol s)))
        atom_args in
      (* ensure the order of atom symbols is the same as in the pclause *)
      let pc = { pc with pc_vars = atom_args; } in
      let nf = { np_pattern = pc; np_name = atom_name; } in
      (* ready for next thing to parse *)
      SHashtbl.clear sort_table;
      nf
    }
*/

meta_rule:
  | meta_item IF meta_item_list { Meta.KB.Rule ($1, $3) }

meta_item_list:
  | meta_item { [$1] }
  | meta_item AND meta_item_list { $1 :: $3 }

meta_theory:
  | THEORY LOWER_WORD
    { Meta.KB.Theory ($2, []) }
  | THEORY LOWER_WORD LEFT_PARENTHESIS meta_variables RIGHT_PARENTHESIS
    { Meta.KB.Theory ($2, $4) }

meta_lemma:
  | LEMMA meta_pattern IF meta_patterns
    { Meta.KB.Lemma ($2, $4) }

meta_gc:
  | GC meta_patterns
    WITH LOWER_WORD LEFT_PARENTHESIS meta_variables RIGHT_PARENTHESIS
    { let open Meta.KB in
      let gc_vars = [] in (* TODO: renaming and gathering of vars *)
      let gc_ord = $4 in
      let gc_prec = $6 in
      let gc_eqns = $2 in
      Meta.KB.GC {
        gc_vars;
        gc_ord;
        gc_prec;
        gc_eqns;
      }
    }

meta_theory:
  | THEORY LOWER_WORD
    { Meta.KB.Theory ($2, []) }
  | THEORY LOWER_WORD LEFT_PARENTHESIS meta_variables RIGHT_PARENTHESIS
    { Meta.KB.Theory ($2, $4) }

meta_rule:
  | meta_item IF meta_item_list
    { Meta.KB.Rule ($1, $3) }

meta_patterns:
  | meta_pattern { [$1] }
  | meta_pattern AND meta_patterns { $1 :: $3 }

meta_pattern:
  | fof_formula
    { let t = $1 in
      Meta.Pattern.of_term t
    }

meta_variables:
  | meta_variable { [$1] }
  | meta_variable COMMA meta_variables { $1 :: $3 }

meta_variable:
  | UPPER_WORD
    { (* FIXME: type inference??*)
      get_var $1
    }


%%





(*
    ||M||  This file is part of HELM, an Hypertextual, Electronic        
    ||A||  Library of Mathematics, developed at the Computer Science     
    ||T||  Department, University of Bologna, Italy.                     
    ||I||                                                                
    ||T||  HELM is free software; you can redistribute it and/or         
    ||A||  modify it under the terms of the GNU General Public License   
    \   /  version 2 or (at your option) any later version.      
     \ /   This software is distributed as is, NO WARRANTY.     
      V_______________________________________________________________ *)

(* $Id: terms.mli 10720 2010-02-08 07:24:34Z asperti $ *)

(* Signature for terms parametrized by leaves *)
type leaf = Signature.symbol

(* a sort for terms (only the return sort is kept) *)
type sort = leaf

(* some special sorts *)
val bool_sort : sort
val univ_sort : sort

(* exception raised when sorts are mismatched *)
exception SortError of string

(* a type term *)
type foterm = typed_term Hashcons.hash_consed
and typed_term = private {
  term : foterm_cell;   (* the term itself *)
  sort : sort;          (* the sort of the term *)
  vars : foterm list Lazy.t;   (* the variables of the term *)
}
and foterm_cell = private
  | Leaf of leaf  (* constant *)
  | Var of int  (* variable *)
  | Node of foterm list  (* term application *)

(* iterate through existing terms *)
val iter_terms : (foterm -> unit) -> unit

(* smart constructors, with type-checking *)
val mk_var : int -> sort -> foterm
val mk_leaf : leaf -> sort -> foterm
val mk_node : foterm list -> foterm

(* special terms *)
val eq_term : foterm  (* equality of terms *)
val true_term : foterm  (* tautology symbol *)

(* membership: [a] [b] checks if a subterm of b *)
val member_term : foterm -> foterm -> bool 
(* standard equality on terms *)
val eq_foterm : foterm -> foterm -> bool

(* cast (change sort) *)
val cast : foterm -> sort -> foterm
(* list of variables *)
type varlist = foterm list
(* free variables in the term *)
val vars_of_term : foterm -> varlist

(* substitution, a list of variables -> term *)
type substitution = (foterm * foterm) list

(* partial order comparison *)
type comparison = Lt | Eq | Gt | Incomparable | Invertible
(* direction of an equation (for rewriting) *)
type direction = Left2Right | Right2Left | Nodir
(* position in a term *)
type position = int list

(* left and right position in equation *)
val left_pos : int
val right_pos : int

(* a literal, that is, a signed equation *)
type literal = 
 | Equation of    foterm  (* lhs *)
                * foterm  (* rhs *)
                * bool    (* sign *)
                * comparison (* orientation *)

(* a first order clause *)
type clause =
    int (* ID *)
  * literal list  (* the equations *)
  * foterm list  (* the free variables *)
  * proof (* the proof for this clause *)
(* a proof step for a clause *)
and proof = Axiom of string | Proof of rule * proof_clauses
(* an inference rule name *)
and rule = string
(* a list of terms in clauses involved in an inference *)
and proof_clauses = (clause * position * substitution) list

module M : Map.S with type key = int 

(* multiset of clauses *)
type bag = {
  bag_id : int; (* max ID  *)
  bag_clauses : (clause * bool * int) M.t;
}

(* also gives a fresh ID to the clause *)
val add_to_bag : clause -> bag -> bag * clause

val replace_in_bag : clause * bool * int -> bag -> bag

val get_from_bag : int -> bag -> clause * bool * int

val empty_bag : bag

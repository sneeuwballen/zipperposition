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
}
and foterm_cell = private
  | Leaf of leaf  (* constant *)
  | Var of int  (* variable *)
  | Node of foterm list  (* term application *)

(* smart constructors, with type-checking *)
val mk_var : int -> sort -> foterm
val mk_leaf : leaf -> sort -> foterm
val mk_node : foterm list -> foterm

(* special terms *)
val eq_term : foterm  (* equality of terms *)
val true_term : foterm  (* tautology symbol *)

(* cast (change sort) *)
val cast : foterm -> sort -> foterm

(* free variables in the term *)
val vars_of_term : foterm -> foterm list

(* substitution, a list of variables -> term *)
type substitution = (foterm * foterm) list

(* partial order comparison *)
type comparison = Lt | Eq | Gt | Incomparable | Invertible

(* direction of an equation (for rewriting) *)
type direction = Left2Right | Right2Left | Nodir
(* side of an equation *)
type side = LeftSide | RightSide
(* position in a term *)
type position = int list

(* a literal, that is, a signed equation *)
type literal = 
 | Equation of    foterm  (* lhs *)
                * foterm  (* rhs *)
                * bool    (* sign *)
                (* * comparison (* orientation *) *)

(* build literals. If sides so not have the same sort,
 * this will raise a SortError *)
val mk_eq : foterm -> foterm -> literal
val mk_neq : foterm -> foterm -> literal
(* negate literal *)
val negate_lit : literal -> literal

(* a proof step for a clause *)
type proof =
  | Axiom of string (* axiom of input *)
  | SuperpositionLeft of sup_position
  | SuperpositionRight of sup_position
  | EqualityFactoring of eq_factoring_position  
  | EqualityResolution of eq_resolution_position
and sup_position = {
  (* describes a superposition inference *)
  sup_active : clause;  (* rewriting clause *)
  sup_passive : clause; (* rewritten clause *)
  sup_active_pos : (int * side * position);
  sup_passive_pos : (int * side * position);
  sup_subst : substitution;
}
and eq_factoring_position = {
  (* describes an equality factoring inference *)
  eqf_clause : clause;
  eqf_bigger : (int * side);  (* bigger equation s=t, s > t *)
  eqf_smaller : (int * side); (* smaller equation u=v *)
  eqf_subst : substitution; (* subst(s) = subst(u) *)
}
and eq_resolution_position = {
  (* describes an equality resolution inference *)
  eqr_clause : clause;
  eqr_position : int;
  eqr_subst : substitution;
}
and clause =
    (* a first order clause *)
    int (* ID *)
  * literal list  (* the equations *)
  * foterm list  (* the free variables *)
  * proof (* the proof for this clause *)

(* build a clause with a new ID *)
val mk_clause : literal list -> proof -> clause

module M : Map.S with type key = int 

(* multiset of clauses TODO use a map? *)
type bag = int (* max ID  *)
              * ((clause * bool * int) M.t)

(* also gives a fresh ID to the clause *)
val add_to_bag : clause -> bag -> bag * clause

val replace_in_bag : clause * bool * int -> bag -> bag

val get_from_bag : int -> bag -> clause * bool * int

val empty_bag : bag

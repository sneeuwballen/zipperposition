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

(* $Id: terms.ml 9836 2009-06-05 15:33:35Z denes $ *)

(* lexicographic order on lists l1,l2 which elements are ordered by f *)
val lexicograph : ('a -> 'b -> int) -> 'a list -> 'b list -> int

open Terms

(* standard equality on terms *)
val eq_foterm : foterm -> foterm -> bool
(* a simple order on terms *)
val compare_foterm : foterm -> foterm -> int
(* fresh term, which variables are all > maxvar *)
val fresh_foterm : int -> foterm -> foterm

(* equality literals *)
val eq_literal : literal -> literal -> bool
(* lexicographic comparison of literals *)
val compare_literal : literal -> literal -> int

(* build literals. If sides so not have the same sort,
 * this will raise a SortError. A term comparison
 * function can be provided. *)
val mk_eq : ?comp:(foterm -> foterm -> comparison) ->
             foterm -> foterm -> literal
val mk_neq : ?comp:(foterm -> foterm -> comparison) ->
              foterm -> foterm -> literal
(* negate literal *)
val negate_lit : literal -> literal
(* fmap in literal *)
val fmap_lit : ?comp:(foterm -> foterm -> comparison) ->
               (foterm -> foterm) -> literal -> literal

(* compare clauses *)
val eq_clause : clause -> clause -> bool
val compare_clause : clause -> clause -> int

(* build a clause with a new ID *)
val mk_clause : literal list -> proof -> clause
(* rename a clause w.r.t. maxvar *)
val fresh_clause : int -> clause -> clause * int

(* find the maximum variable index in the varlist *)
val max_var : varlist -> int

(* perform renaming to get disjoint variables sets,
   ie the resulting substitution's domain has no common
   variable with [varlist], and its new domain is newvarlist
   relocate [maxvar] [varlist] [subst] ->
   [newmaxvar] * [newvarlist] * [relocsubst] *)
val relocate : 
      int -> varlist -> substitution -> 
        (int * varlist * substitution)

(* rename clauses and terms so that they have no variable in varlist *)
val relocate_term : varlist -> foterm -> foterm
val relocate_clause : varlist -> clause -> clause

(*
val compare_passive_clauses_weight : passive_clause -> passive_clause -> int

val compare_passive_clauses_age : passive_clause -> passive_clause -> int
*)

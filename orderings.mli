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

(* $Id: nCic.ml 9058 2008-10-13 17:42:30Z tassi $ *)

(* ----------------------------------------------------------------------
 module interface
 ---------------------------------------------------------------------- *)

val compute_clause_weight : Terms.clause -> int

module type S =
  sig 
    type foterm = Terms.foterm

    (* This order relation should be:
     * - stable for instantiation
     * - total on ground terms
     *
     *)
    val compare_terms : Terms.foterm -> Terms.foterm -> Terms.comparison

    (* these could be outside the module, but to ease experimentation
     * we allow them to be tied with the ordering *)
    val compute_clause_weight : Terms.clause -> int

    val name : string
  end

module NRKBO : S

module KBO  : S 

module LPO  : S

(* default ordering (LPO) *)
module Default : S

(* ----------------------------------------------------------------------
 class interface
 ---------------------------------------------------------------------- *)


(* the interface of a type *)
class type ordering =
  object
    method compare_terms : Terms.foterm -> Terms.foterm -> Terms.comparison
    method compute_clause_weight : Terms.clause -> int
    method name : string
  end

class nrkbo : ordering

class kbo : ordering

class lpo : ordering

class default : ordering

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

exception UnificationFailure of string Lazy.t;;

val unification:
  (* global varlist for both terms t1 and t2 (UNUSED) *)
  (* Terms.varlist -> *)
  (* locked variables: if equal to FV(t2) we match t1 with t2*)
  Terms.varlist -> 
  Terms.foterm ->
  Terms.foterm ->
  Terms.substitution 

val alpha_eq: Terms.foterm -> Terms.foterm -> Terms.substitution


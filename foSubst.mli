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

exception OccurCheck of (Terms.foterm * Terms.foterm)

val id_subst : Terms.substitution

(* add v -> t to the substitution. If recursive is true,
 * then v -> subst(t) is considered instead.
 * If v occurs in t, OccurCheck (v,t) is raised. *)
val build_subst : Terms.foterm -> Terms.foterm -> ?recursive:bool ->
                  Terms.substitution -> Terms.substitution

val lookup : Terms.foterm -> Terms.substitution -> Terms.foterm
val is_in_subst : Terms.foterm -> Terms.substitution -> bool
val filter : Terms.substitution -> Terms.varlist -> Terms.varlist
val reloc_subst : Terms.substitution -> Terms.foterm -> Terms.foterm
val apply_subst : Terms.substitution -> ?recursive:bool ->
                  Terms.foterm -> Terms.foterm
val flat: Terms.substitution -> Terms.substitution
val concat: Terms.substitution -> Terms.substitution -> Terms.substitution

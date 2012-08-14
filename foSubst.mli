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

open Types

exception OccurCheck of (foterm * foterm)

val id_subst : substitution

(** add v -> t to the substitution. If recursive is true,
    then v -> subst(t) is considered instead.
    If v occurs in t, OccurCheck (v,t) is raised. *)
val build_subst : ?recursive:bool -> foterm -> foterm -> substitution -> substitution

(** lookup variable in substitution *)
val lookup : foterm -> substitution -> foterm
val is_in_subst : foterm -> substitution -> bool

(** filter out from the varlist the variables bound by subst *)
val filter : substitution -> varlist -> varlist

val apply_subst : ?recursive:bool -> substitution -> foterm -> foterm

(** normalize the substitution, such that subst(subst(v)) = subst(v)
    for all v. The result is idempotent. *)
val flat: substitution -> substitution
val concat: substitution -> substitution -> substitution

(** perform renaming to get disjoint variables sets,
    ie the resulting substitution's domain has no common
    variable with [varlist], and its new domain is newvarlist
    relocate [maxvar] [varlist] [subst] ->
    [newmaxvar] * [newvarlist] * [relocsubst] *)
val relocate : ?recursive:bool -> int -> varlist -> substitution
            -> (int * varlist * substitution)

val fresh_foterm : int -> foterm -> foterm      (** fresh term, with all variables > maxvar *)
val relocate_term : varlist -> foterm -> foterm (** rename the term so that
                                                    it has no variable in varlist *)

val pp_substitution: Format.formatter -> substitution -> unit

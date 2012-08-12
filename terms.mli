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

(** some special sorts *)
val bool_sort : sort
val univ_sort : sort

val iter_terms : (foterm -> unit) -> unit     (** iterate through existing terms *)
val all_terms : unit -> foterm list           (** all currently existing terms *)

(** smart constructors, with a bit of type-checking *)
val mk_var : int -> sort -> foterm
val mk_leaf : leaf -> sort -> foterm
val mk_node : foterm list -> foterm

val eq_symbol : foterm                        (** equality symbol *)
val true_symbol : foterm                      (** tautology symbol *)

val member_term : foterm -> foterm -> bool    (** membership: [a] [b] checks if a subterm of b *)
val eq_foterm : foterm -> foterm -> bool      (** standard equality on terms *)
val compare_foterm : foterm -> foterm -> int  (** a simple order on terms *)
val cast : foterm -> sort -> foterm           (** cast (change sort) *)

val vars_of_term : foterm -> varlist          (** free variables in the term *)
val is_ground_term : foterm -> bool           (** is the term ground? *)
val merge_varlist : varlist -> varlist -> varlist (** set union of variable list *)

val max_var : varlist -> int                  (** find the maximum variable index
                                                  in the varlist *)

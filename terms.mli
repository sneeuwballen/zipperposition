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


val str_to_sym : string -> symbol

(** some special sorts *)
val bool_sort : sort
val univ_sort : sort
val true_symbol : symbol
val eq_symbol : symbol

val iter_terms : (foterm -> unit) -> unit     (** iterate through existing terms *)
val all_terms : unit -> foterm list           (** all currently existing terms *)

(** smart constructors, with a bit of type-checking *)
val mk_var : int -> sort -> foterm
val mk_leaf : symbol -> sort -> foterm
val mk_node : foterm list -> foterm

val is_var : foterm -> bool
val is_leaf : foterm -> bool
val is_node : foterm -> bool
val hd_term : foterm -> foterm option         (** the head of the term *)
val hd_symbol : foterm -> symbol option       (** the head of the term *)

val eq_term : foterm                          (** equality symbol *)
val true_term : foterm                        (** tautology symbol *)

val member_term : foterm -> foterm -> bool    (** [a] [b] checks if a subterm of b *)
val eq_foterm : foterm -> foterm -> bool      (** standard equality on terms *)
val compare_foterm : foterm -> foterm -> int  (** a simple order on terms *)
val cast : foterm -> sort -> foterm           (** cast (change sort) *)

val at_pos : foterm -> position -> foterm     (** retrieve subterm at pos, or
                                                  raise Invalid_argument
                                                  TODO also return a context? *)
val replace_pos : foterm -> position          (** replace t|_p by the second term *)
               -> foterm -> foterm

val vars_of_term : foterm -> varlist          (** free variables in the term *)
val is_ground_term : foterm -> bool           (** is the term ground? *)
val merge_varlist : varlist -> varlist -> varlist (** set union of variable list *)

val max_var : varlist -> int                  (** find the maximum variable index
                                                  in the varlist *)

val pp_symbol : Format.formatter -> symbol -> unit
val pp_foterm: Format.formatter -> ?sort:bool -> foterm -> unit
val pp_signature : Format.formatter -> symbol list -> unit

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

(* $Id: index.mli 9822 2009-06-03 15:37:06Z tassi $ *)

module type InferenceContext =
  sig
    module Ord : Orderings.S
    module Idx : Index.Index(Ord)
  end

module Superposition (Ctx : InferenceContext) :
  sig
    module Ord = Ctx.Ord
    module Idx = Ctx.Idx

    (* bag, maxvar, empty clause *)
    exception Success of 
      Terms.bag 
      * int 
      * Terms.clause

    (* The returned active set is the input one + the selected clause *)
    val infer_right :
      Terms.bag -> 
      int -> (* maxvar *)
      Terms.clause -> (* selected passive *)
      Idx.active_set ->
      Terms.bag * int * Idx.active_set * Terms.clause list

    val infer_left :  
      Terms.bag -> 
      int -> (* maxvar *)
      Terms.clause -> (* selected goal *)
      Idx.active_set ->
      Terms.bag * int * Terms.unit_clause list

    val demodulate : 
      Terms.bag ->
      Terms.clause ->
      Idx.DT.t ->  (* index of unit clauses *)
      Terms.bag * Terms.clause

    val simplify : 
      Idx.DT.t ->
      int ->
      Terms.bag ->
      Terms.clause ->
      Terms.bag * (Terms.clause option)

    (* may raise success *)
    val simplify_goal :
      no_demod:bool ->
      int ->
      Idx.DT.t ->
      Terms.bag ->
      Terms.clause list ->
      Terms.clause ->
      (Terms.bag * Terms.clause) option

    val one_pass_simplification:
      Terms.unit_clause ->
      Idx.active_set ->
      Terms.bag ->
      int ->
      Terms.bag * (Terms.clause * Idx.active_set) option

    val keep_simplified:
      Terms.clause ->
      Idx.active_set ->
      Terms.bag ->
      int ->
      Terms.bag * (Terms.clause * Idx.active_set) option

    val orphan_murder:
      Terms.bag ->
      Terms.clause list ->
      Terms.clause ->
      bool

    val are_alpha_eq : 
      Terms.unit_clause ->
      Terms.unit_clause ->
      bool
  end

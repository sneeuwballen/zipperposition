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

(* $Id: index.mli 10591 2009-12-09 15:35:07Z asperti $ *)

module Index (Ord : Orderings.S) :
  sig
    (* a set of (clause, position in clause). A position is a
     * list [lit index, 1|2 (left or right), ...]
     * where ... is the path in the term *)
    module ClauseSet : Set.S with 
      type elt = Terms.clause * Terms.position

    (* make terms indexable by discrimination_tree *)
    module FotermIndexable : Discrimination_tree.Indexable with 
      type constant_name = Signature.symbol and
      type input = Terms.foterm 

    module DT : Discrimination_tree.DiscriminationTree with 
      type constant_name = Signature.symbol and 
      type input = Terms.foterm and 
      type data = ClauseSet.elt and 
      type dataset = ClauseSet.t

    type input = DT.input
    type data = DT.data
    type dataset = DT.dataset

    (* the main index type. It contains two trees, that are
     * used to index all subterms of a clause, and terms
     * that occur directly under an equation. *)
    type t = {
      root_index : DT.t;
      subterm_index : DT.t;
    }

    val empty : t
    
    val index_clause : t -> Terms.clause -> t 

    val remove_clause : t -> Terms.clause -> t 

    val fold : 
      DT.t ->
      (Signature.symbol Discrimination_tree.path -> ClauseSet.t -> 'a -> 'a) 
      -> 'a -> 'a

    val elems : DT.t -> ClauseSet.t

    (* an active set contains a list of clauses and an index on those clauses *)
    type active_set = Terms.clause list * t
  end

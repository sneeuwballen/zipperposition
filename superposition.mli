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

(** a conclusion is a clause, plus the clauses used to infer it *)
type conclusion = clause * hclause list

(** raised when the empty clause is found *)
exception Success of hclause

(** inferences *)
type inference_rule = ProofState.active_set -> clause -> conclusion list

val infer_right : inference_rule

val infer_left : inference_rule

val infer_equality_resolution : inference_rule

val infer_equality_factoring : inference_rule

(* simplifications *)

val is_tautology : clause -> bool

val demodulate : ProofState.active_set
                -> clause   (** the clause to simplify *)
		-> clause   (** the simplified clause *)

(** subsumes c1 c2 iff c1 subsumes c2 *)
val subsumes : clause -> clause -> bool

(** check whether the clause is subsumed by any clause in the set *)
val subsumed_by_set : ProofState.active_set -> clause -> bool

(** remove from the set the clauses subsumed by c *)
val subsumed_in_set : ProofState.active_set -> clause -> hclause list

(** remove from the passive_set the list of orphans of clause *)
val orphan_murder: ProofState.passive_set
                -> clause   (** the clause whose orphans are to be deleted *)
                -> ProofState.passive_set

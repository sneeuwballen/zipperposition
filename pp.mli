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
open Format

(* print a list of items using the printing function *)
val pp_list: ?sep:string -> (formatter -> 'a -> unit)
    -> formatter -> 'a list -> unit

(* pretty printing of objects *)
val pp_foterm: formatter -> foterm -> unit
val pp_substitution: formatter -> substitution -> unit
val pp_clause : formatter -> clause -> unit
val pp_clause_pos : formatter -> (clause * position) -> unit
val pp_hclause : formatter -> hclause -> unit
val pp_hclause_pos : formatter -> (hclause * position) -> unit
val pp_bag: formatter -> Clauses.bag -> unit
val pp_index : formatter -> Index.t -> unit
val pp_queue : formatter -> ClauseQueue.queue -> unit
val pp_queues : formatter -> (ClauseQueue.queue * int) list -> unit
val pp_state : formatter -> ProofState.state -> unit

(* debug functions: much more detailed printing *)
val debug_state : formatter -> ProofState.state -> unit

(*
val pp_proof: Format.formatter -> T.proof -> unit
*)


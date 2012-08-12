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

(* print a list of items using the printing function *)
val pp_list: ?sep:string -> (Format.formatter -> 'a -> unit)
    -> Format.formatter -> 'a list -> unit

val pp_foterm: Format.formatter -> foterm -> unit

(*
val pp_proof: Format.formatter -> T.proof -> unit
*)

val pp_substitution: Format.formatter -> substitution -> unit

val pp_clause : Format.formatter -> clause -> unit

val pp_clause_pos : Format.formatter -> (clause * position) -> unit

val pp_bag: Format.formatter -> Clauses.bag -> unit

(* print into a string *)
val on_buffer: ?margin:int -> (Format.formatter -> 'a -> 'b)
                -> 'a -> string


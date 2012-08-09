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

(* print a list of items using the printing function *)
val pp_list: ?sep:string -> (Format.formatter -> 'a -> unit)
    -> Format.formatter -> 'a list -> unit

val pp_foterm: Format.formatter -> Terms.foterm -> unit

(*
val pp_proof: Format.formatter -> T.proof -> unit
*)

val pp_substitution: Format.formatter -> Terms.substitution -> unit

val pp_clause : Format.formatter -> Terms.clause -> unit

val pp_clause_pos : Format.formatter -> (Terms.clause * Terms.position) -> unit

val pp_bag: Format.formatter -> Terms.bag -> unit

(* print into a string *)
val on_buffer: ?margin:int -> (Format.formatter -> 'a -> 'b)
                -> 'a -> string


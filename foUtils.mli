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

(** lexicographic order on lists l1,l2 which elements are ordered by f *)
val lexicograph : ('a -> 'b -> int) -> 'a list -> 'b list -> int

(** conversion from partial order to a total order, in which incomparable
    elements are considered to be in the same congruence class *)
val partial_to_total : ('a -> 'b -> comparison) -> 'a -> 'b -> int
val total_to_partial : ('a -> 'b -> int) -> 'a -> 'b -> comparison
val or_partial : comparison -> comparison -> comparison
val not_partial : comparison -> comparison

(** the opposite order, that sorts elements the opposite way *)
val opposite_order : ('a -> 'b -> int) -> 'a -> 'b -> int

(** multiset equality given partial order f *)
val multiset_eq : ('a -> 'a -> comparison) -> 'a list -> 'a list -> bool
(** multiset order on lists which elements are ordered by f *)
val multiset_partial : ('a -> 'a -> comparison) -> 'a list -> 'a list -> comparison

(** get n-th element of list (linear) *)
val list_get : 'a list -> int -> 'a
(** set n-th element of list (linear) *)
val list_set : 'a list -> int -> 'a -> 'a list
(** all the list but i-th element (linear) *)
val list_remove : 'a list -> int -> 'a list
(** zip the list with positions (starting at 0) *)
val list_pos : 'a list -> ('a * int) list
(** test for membership using the given comparison function *)
val list_mem : ('a -> 'a -> bool) -> 'a -> 'a list -> bool
(** list intersection, given the comparison function *)
val list_inter : ('a -> 'a -> bool) -> 'a list -> 'a list -> 'a list

(** pretty-print into a string *)
val on_buffer: ?margin:int -> (Format.formatter -> 'a -> 'b) -> 'a -> string

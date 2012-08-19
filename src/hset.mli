(**************************************************************************)
(*                                                                        *)
(*  Copyright (C) Jean-Christophe Filliatre                               *)
(*                                                                        *)
(*  This software is free software; you can redistribute it and/or        *)
(*  modify it under the terms of the GNU Library General Public           *)
(*  License version 2.1, with the special exception on linking            *)
(*  described in file LICENSE.                                            *)
(*                                                                        *)
(*  This software is distributed in the hope that it will be useful,      *)
(*  but WITHOUT ANY WARRANTY; without even the implied warranty of        *)
(*  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.                  *)
(*                                                                        *)
(**************************************************************************)

(*i $Id: hset.mli,v 1.7 2008-07-21 14:53:06 filliatr Exp $ i*)

(*s Sets of hash-consed values, implemented as Patricia trees. 
    See the modules [Hashcons] and [Ptset]. *)

type 'a t

type 'a elt = 'a Hashcons.hash_consed

val empty : 'a t

val is_empty : 'a t -> bool

val mem : 'a elt -> 'a t -> bool

val add : 'a elt -> 'a t -> 'a t

val singleton : 'a elt -> 'a t

val remove : 'a elt -> 'a t -> 'a t

val union : 'a t -> 'a t -> 'a t

val subset : 'a t -> 'a t -> bool

val inter : 'a t -> 'a t -> 'a t

val diff : 'a t -> 'a t -> 'a t

val equal : 'a t -> 'a t -> bool

val compare : 'a t -> 'a t -> int

val elements : 'a t -> 'a elt list

val choose : 'a t -> 'a elt

val cardinal : 'a t -> int

val iter : ('a elt -> unit) -> 'a t -> unit

val fold : ('a elt -> 'b -> 'b) -> 'a t -> 'b -> 'b

val for_all : ('a elt -> bool) -> 'a t -> bool

val exists : ('a elt -> bool) -> 'a t -> bool

val filter : ('a elt -> bool) -> 'a t -> 'a t

val partition : ('a elt -> bool) -> 'a t -> 'a t * 'a t

(*s Warning: [min_elt] and [max_elt] are linear w.r.t. the size of the
    set. In other words, [min_elt t] is barely more efficient than [fold
    min t (choose t)]. *)

val min_elt : 'a t -> 'a elt
val max_elt : 'a t -> 'a elt

(*s Additional functions not appearing in the signature [Set.S] from ocaml
    standard library. *)

(* [intersect u v] determines if sets [u] and [v] have a non-empty 
   intersection. *) 

val intersect : 'a t -> 'a t -> bool

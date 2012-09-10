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

(*i $Id: ptset.mli,v 1.10 2008-07-21 14:53:06 filliatr Exp $ i*)

(*s Sets of integers implemented as Patricia trees.  The following
    signature is exactly [Set.S with type elt = int], with the same
    specifications. This is a purely functional data-structure. The
    performances are similar to those of the standard library's module
    [Set]. The representation is unique and thus structural comparison
    can be performed on Patricia trees. *)

type t

type elt = int

val empty : t

val is_empty : t -> bool

val mem : int -> t -> bool

val add : int -> t -> t

val singleton : int -> t

val remove : int -> t -> t

val union : t -> t -> t

val subset : t -> t -> bool

val inter : t -> t -> t

val diff : t -> t -> t

val equal : t -> t -> bool

val compare : t -> t -> int

val elements : t -> int list

val choose : t -> int

val cardinal : t -> int

val iter : (int -> unit) -> t -> unit

val fold : (int -> 'a -> 'a) -> t -> 'a -> 'a

val for_all : (int -> bool) -> t -> bool

val exists : (int -> bool) -> t -> bool

val filter : (int -> bool) -> t -> t

val partition : (int -> bool) -> t -> t * t

val split : int -> t -> t * bool * t

(*s Warning: [min_elt] and [max_elt] are linear w.r.t. the size of the
    set. In other words, [min_elt t] is barely more efficient than [fold
    min t (choose t)]. *)

val min_elt : t -> int
val max_elt : t -> int

(*s Additional functions not appearing in the signature [Set.S] from ocaml
    standard library. *)

(* [intersect u v] determines if sets [u] and [v] have a non-empty 
   intersection. *) 

val intersect : t -> t -> bool


(*s Big-endian Patricia trees *)

module Big : sig
  include Set.S with type elt = int
  val intersect : t -> t -> bool
end


(*s Big-endian Patricia trees with non-negative elements. Changes:
    - [add] and [singleton] raise [Invalid_arg] if a negative element is given
    - [mem] is slightly faster (the Patricia tree is now a search tree)
    - [min_elt] and [max_elt] are now O(log(N))
    - [elements] returns a list with elements in ascending order
 *)

module BigPos : sig
  include Set.S with type elt = int
  val intersect : t -> t -> bool
end



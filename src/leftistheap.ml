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

(* Leftist heaps.

   See for instance Chris Okasaki's "Purely Functional Data Structures".

   Simon Cruanes: added a remove (slow) function; OO interface *)

exception Empty

(* -----------------------------------------------------------------------------
  functor interface
 ----------------------------------------------------------------------------- *)

module type Ordered = sig
  type t
  val le: t -> t -> bool
end

module Make(X : Ordered) :
sig
  type t
  val empty : t
  val is_empty : t -> bool
  val insert : X.t -> t -> t
  val min : t -> X.t
  val extract_min : t -> X.t * t
  val merge : t -> t -> t
  val remove: t -> X.t list -> t
end
=
struct

  type t = E | T of int * X.t * t * t

  let rank = function E -> 0 | T (r,_,_,_) -> r

  let make x a b =
    let ra = rank a and rb = rank b in
    if ra >= rb then T (rb + 1, x, a, b) else T (ra + 1, x, b, a)

  let empty = E

  let is_empty = function E -> true | T _ -> false

  let rec merge h1 h2 = match h1,h2 with
    | E, h | h, E ->
	h
    | T (_,x,a1,b1), T (_,y,a2,b2) ->
	if X.le x y then make x a1 (merge b1 h2) else make y a2 (merge h1 b2)

  let insert x h = merge (T (1, x, E, E)) h

  let min = function E -> raise Empty | T (_,x,_,_) -> x

  let extract_min = function
    | E -> raise Empty
    | T (_,x,a,b) -> x, merge a b

  let rec remove t l = match t with
    | E -> E
    | T (_,x,a,b) when List.exists (fun y -> X.le x y && X.le y x) l ->
      (* eliminate x, it is in the list if X.le is a total order *)
      merge (remove a l) (remove b l)
    | T (_,x,a,b) ->
      make x (remove a l) (remove b l)

end

(* -----------------------------------------------------------------------------
  object interface
 ----------------------------------------------------------------------------- *)

class type ['a] ordered =
  object
    method le : 'a -> 'a -> bool
  end

module T = struct
  type 'a t = E | T of int * 'a * 'a t * 'a t

  let rank t = match t with E -> 0 | T (r,_,_,_) -> r

  let make x a b =
    let ra = rank a and rb = rank b in
    if ra >= rb then T (rb + 1, x, a, b) else T (ra + 1, x, b, a)

  let empty = E

  let is_empty t = match t with E -> true | T _ -> false

  let rec merge ~ord h1 h2 = match h1,h2 with
    | E, h | h, E ->
        h
    | T (_,x,a1,b1), T (_,y,a2,b2) ->
        if ord#le x y then make x a1 (merge ~ord b1 h2) else make y a2 (merge ~ord h1 b2)

  let insert ~ord x h = merge ~ord (T (1, x, E, E)) h

  let min t = match t with E -> raise Empty | T (_,x,_,_) -> x

  let extract_min ~ord t = match t with
    | E -> raise Empty
    | T (_,x,a,b) -> x, merge ~ord a b

  let rec remove ~ord t l = match t with
    | E -> E
    | T (_,x,a,b) when List.exists (fun y -> ord#le x y && ord#le y x) l ->
      (* eliminate x, it is in the list if X.le is a total order *)
      merge ~ord (remove ~ord a l) (remove ~ord b l)
    | T (_,x,a,b) ->
      make x (remove ~ord a l) (remove ~ord b l)
end

class ['a] leftistheap (ord : 'a ordered) =
  object (_ : 't)
    val ord = ord
    val tree = T.empty
    method is_empty = T.is_empty tree
    method insert x = {< tree = T.insert ~ord x tree >}
    method min = T.min tree
    method extract_min =
      let m, new_tree = T.extract_min ~ord tree
      in m, ({< tree = new_tree >} :> 't)
    method remove xs = {< tree = T.remove ~ord tree xs >}
  end


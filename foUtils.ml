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

let rec lexicograph f l1 l2 =
  match l1, l2 with
  | [], [] -> 0
  | x::xs, y::ys ->
     let c = f x y in
     if c <> 0 then c else lexicograph f xs ys
  | [],_ -> ~-1
  | _,[] -> 1

let partial_to_total ord a b = match ord a b with
  | Lt -> -1
  | Gt -> 1
  | Eq | Invertible | Incomparable -> 0

let total_to_partial ord a b = match ord a b with
  | x when x > 0 -> Gt
  | x when x = 0 -> Eq
  | _ -> Lt

let opposite_order ord a b = - (ord a b)

let multiset_partial f l1 l2 = 
  (* sort lists in decreasing order w.r.t f *)
  let l1 = List.stable_sort (opposite_order (partial_to_total f)) l1
  and l2 = List.stable_sort (opposite_order (partial_to_total f)) l2 in
  (* now for Mana and Dershowitz algorithm, as presented in
     "on multiset ordering" by Jouannaud and Lescanne, p.7 *)
  let rec aux_cmp l1 l2 = match l1, l2 with
  | [], [] -> Eq
  | [], _ -> Lt
  | _, [] -> Gt
  | x1::xs1, x2::xs2 ->
    match f x1 x2 with
    | Eq | Invertible -> aux_cmp xs1 xs2
    | Incomparable -> Incomparable  (* maximal elements are incomparable *)
    | Lt -> aux_cmp (remove_smaller x1 xs1) l2
    | Gt -> aux_cmp l1 (remove_smaller x2 xs2)
  (* remove from l elements that are smaller than x *)
  and remove_smaller x l = match l with
  | [] -> []
  | y::ys ->
    if f x y = Gt
      then remove_smaller x ys
      else y :: (remove_smaller x ys)
  in aux_cmp l1 l2

let rec list_get l i = match l, i with
  | [], i -> invalid_arg "index too high"
  | x::_, i when i = 0 -> x
  | _::xs, i when i > 0 -> list_get xs (i-1)
  | _ -> failwith "error in list_get"

let rec list_set l i elem = match l, i with
  | [], i -> invalid_arg "index too high"
  | _::xs, i when i = 0 -> elem::xs
  | x::xs, i when i > 0 -> x::(list_set xs (i-1) elem)
  | _ -> failwith "error in list_set"

let rec list_remove l i = match l, i with
  | [], i -> invalid_arg "index too high"
  | _::xs, i when i = 0 -> xs
  | x::xs, i when i > 0 -> x::(list_remove xs (i-1))
  | _ -> failwith "error in list_remove"

let list_pos l =
  let rec aux l idx = match l with
  | [] -> []
  | x::xs -> (x,idx) :: (aux xs (idx+1))
  in
  aux l 0

let on_buffer ?(margin=80) f t =
  let buff = Buffer.create 100 in
  let formatter = Format.formatter_of_buffer buff in
  Format.pp_set_margin formatter margin;
  f formatter t;
  Format.fprintf formatter "@?";
  Buffer.contents buff

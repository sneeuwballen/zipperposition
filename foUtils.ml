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

(** combine two partial comparisons, that are assumed to be
    compatible, ie they do not order differently if
    Incomparable is not one of the values *)
let or_partial cmp1 cmp2 = match cmp1, cmp2 with
  | Eq, Eq | Eq, Invertible | Invertible, Eq
  | Eq, Incomparable | Incomparable, Eq -> Eq
  | Lt, Incomparable | Incomparable, Lt -> Lt
  | Gt, Incomparable | Incomparable, Gt -> Gt
  | Incomparable, Incomparable -> Incomparable
  | Invertible, Invertible -> Invertible
  | _ -> assert false  (* not compatible *)

(** negation of a partial order relation *)
let not_partial cmp = match cmp with
  | Eq | Invertible | Incomparable -> cmp
  | Lt -> Gt
  | Gt -> Lt

(** remove from l1, l2 elements that compare equal using f. This
    should do a quadratic number of comparisons (at worst, compares
    all elementts of l1 with all elements of l2) *)
let multiset_remove_eq f l1 l2 =
  let rec aux l1 acc1 l2 acc2 = match l1, l2 with
  | [], [] | _, [] | [], _ -> l1 @ acc1, l2 @ acc2
  | x1::xs1, x2::xs2 when f x1 x2 = Eq ->
    aux xs1 acc1 xs2 acc2 (* drop x1 and x2 *)
  | x1::xs1, x2::xs2 ->
    match remove x1 [] xs2, remove x2 [] xs1 with
      | None, None -> aux xs1 (x1::acc1) xs2 (x2::acc2) (* keep both *)
      | Some l2', None -> aux xs1 acc1 l2' (x2::acc2)
      | None, Some l1' -> aux l1' (x1::acc1) xs2 acc2
      | Some l2', Some l1' -> aux l1' acc1 l2' acc2  (* drop both *)
  (* if l contains an element equal to x, returns Some(l')
     where l' is l without this element. Otherwise, None. *)
  and remove x acc l = match l with
  | [] -> None
  | y::ys when f x y = Eq -> Some (acc @ ys)
  | y::ys -> remove x (y :: acc) ys
  in aux l1 [] l2 []

(* check that l1 and l2 are equal multisets under f *)
let multiset_eq f l1 l2 =
  let l1, l2 = multiset_remove_eq f l1 l2 in
  match l1, l2 with
  | [], [] -> true
  | _ -> false

(* naive recursive version, tries all permutations *)
let multiset_partial f l1 l2 = 
  (* first, remove common elements *)
  let l1, l2 = multiset_remove_eq f l1 l2 in
  (* now for a naive Mana and Dershowitz ordering, as presented in
     chapter "paramodulation-based theorem proving" of the
     handbook of automated reasoning. We look for an element that
     dominates the whole other multiset *)
  let rec find_dominating l1' l2' = match l1', l2' with
  | [], [] -> Incomparable
  | x1::xs1, [] -> if dominates x1 l2 then Gt else find_dominating xs1 []
  | [], x2::xs2 -> if dominates x2 l1 then Lt else find_dominating [] xs2
  | x1::xs1, x2::xs2 ->
    let x1_win = dominates x1 l2
    and x2_win = dominates x2 l1 in
    assert ((not x1_win) || (not x2_win));
    if x1_win then Gt else if x2_win then Lt else find_dominating xs1 xs2
  and dominates x l = match l with
  | [] -> true
  | y::ys when f x y = Gt -> dominates x ys
  | _ -> false
  in match l1, l2 with
  | [], [] -> Eq (* all elements removed by multiset_remove_eq *)
  | _ -> find_dominating l1 l2

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

let rec list_mem comp x l = match l with
  | [] -> false
  | y::ys when comp x y -> true
  | _::ys -> list_mem comp x ys

let rec list_uniq comp l = match l with
  | [] -> []
  | x::xs -> x :: list_uniq comp (List.filter (fun x' -> not (comp x x')) xs)

let rec list_inter comp l1 l2 = match l1 with
  | [] -> []
  | x::xs when list_mem comp x l2 -> x::(list_inter comp xs l2)
  | _::xs -> list_inter comp xs l2

let on_buffer ?(margin=80) f t =
  let buff = Buffer.create 100 in
  let formatter = Format.formatter_of_buffer buff in
  Format.pp_set_margin formatter margin;
  f formatter t;
  Format.fprintf formatter "@?";
  Buffer.contents buff

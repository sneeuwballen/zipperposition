(*
Zipperposition: a functional superposition prover for prototyping
Copyright (C) 2012 Simon Cruanes

This is free software; you can redistribute it and/or
modify it under the terms of the GNU General Public License
as published by the Free Software Foundation; either version 2
of the License, or (at your option) any later version.

This is distributed in the hope that it will be useful,
but WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
GNU General Public License for more details.

You should have received a copy of the GNU General Public License
along with this program; if not, write to the Free Software
Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA
02110-1301 USA.
*)

open Types

(* debugging facilities *)
let debug_level_ = ref 0
let set_debug l = debug_level_ := l
let debug l s = if l <= !debug_level_ then print_endline (Lazy.force s) else ()
let debug_level () = !debug_level_

module SHashSet =
  struct
    type t = (string, unit) Hashtbl.t
    let create () = Hashtbl.create 13
    let member t s = Hashtbl.mem t s
    let add t s = Hashtbl.replace t s ()
    let from_list ss =
      let t = create () in
      List.iter (add t) ss;
      t
  end

let murmur_hash i =
  let m = 0xd1e995
  and r = 24
  and seed = 0x47b28c in
  let hash = seed lxor 32 in
  let k = i * m in
  let k = k lxor (k lsr r) in
  let k = k * m in
  let hash = (hash * m) lxor k in
  let hash = hash lxor (hash lsr 13) in
  let hash = hash lxor (hash lsr 15) in
  abs hash

let rec lexicograph f l1 l2 =
  match l1, l2 with
  | [], [] -> 0
  | x::xs, y::ys ->
     let c = f x y in
     if c <> 0 then c else lexicograph f xs ys
  | [],_ -> ~-1
  | _,[] -> 1

let rec lexicograph_partial f l1 l2 =
  match l1, l2 with
  | [], [] -> Eq
  | x::xs, y::ys ->
    (match f x y with
    | Lt -> Lt | Gt -> Gt | Incomparable -> Incomparable
    | Eq -> lexicograph_partial f xs ys)
  | [], _ | _, [] -> Incomparable

let partial_to_total ord a b = match ord a b with
  | Lt -> -1
  | Gt -> 1
  | Eq | Incomparable -> 0

let total_to_partial ord a b = match ord a b with
  | x when x > 0 -> Gt
  | x when x = 0 -> Eq
  | _ -> Lt

let opposite_order ord a b = - (ord a b)

(** combine two partial comparisons, that are assumed to be
    compatible, ie they do not order differently if
    Incomparable is not one of the values *)
let or_partial cmp1 cmp2 = match cmp1, cmp2 with
  | Eq, Eq
  | Eq, Incomparable | Incomparable, Eq -> Eq
  | Lt, Incomparable | Incomparable, Lt -> Lt
  | Gt, Incomparable | Incomparable, Gt -> Gt
  | Incomparable, Incomparable -> Incomparable
  | _ -> assert false  (* not compatible *)

(** negation of a partial order relation *)
let not_partial cmp = match cmp with
  | Eq | Incomparable -> cmp
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
  | [], i -> raise Not_found
  | x::_, i when i = 0 -> x
  | _::xs, i when i > 0 -> list_get xs (i-1)
  | _ -> failwith "error in list_get"

let rec list_set l i elem = match l, i with
  | [], i -> invalid_arg "index too high"
  | _::xs, i when i = 0 -> elem::xs
  | x::xs, i when i > 0 -> x::(list_set xs (i-1) elem)
  | _ -> failwith "error in list_set"

let list_mapi l f =
  let rec recurse l i =
    match l with
    | [] -> []
    | x::l' -> f i x :: recurse l' (i+1)
  in recurse l 0

let list_iteri l f =
  let rec recurse l i =
    match l with
    | [] -> ()
    | x::l' -> f i x; recurse l' (i+1)
  in recurse l 0

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

let rec list_subset cmp l1 l2 =
  List.for_all
    (fun t -> list_mem cmp t l2)
    l1

let rec list_uniq eq l = match l with
  | [] -> []
  | x::xs when List.exists (eq x) xs -> list_uniq eq xs
  | x::xs -> x :: list_uniq eq xs

let list_merge comp l1 l2 =
  let rec recurse l1 l2 = match l1,l2 with
  | [], _ -> l2
  | _, [] -> l1
  | x::l1', y::l2' ->
    let cmp = comp x y in
    if cmp < 0 then x::(recurse l1' l2)
    else if cmp > 0 then y::(recurse l1 l2')
    else x::(recurse l1' l2')
  in
  List.rev (recurse l1 l2)

let rec list_union comp l1 l2 = match l1 with
  | [] -> l2
  | x::xs when list_mem comp x l2 -> list_union comp xs l2
  | x::xs -> x::(list_union comp xs l2)

let rec list_inter comp l1 l2 = match l1 with
  | [] -> []
  | x::xs when list_mem comp x l2 -> x::(list_inter comp xs l2)
  | _::xs -> list_inter comp xs l2

let list_flatmap f l =
  let rec recurse acc l = match l with
  | [] -> List.rev acc
  | x::l' -> recurse ((f x) @ acc) l'
  in recurse [] l

let rec list_take n l = match n, l with
  | 0, _ -> l
  | _, [] -> l
  | _, x::xs when n > 0 -> x :: (list_take (n-1) xs)
  | _ -> assert false

let rec list_min cmp l =
  match l with
  | [] -> []
  | x::xs when List.exists (fun x' -> cmp x x' = Gt) xs ->
    list_min cmp xs
  | x::xs -> x :: (list_min cmp xs)

let rec list_range low high =
  assert (low <= high);
  match low, high with
  | _, _ when low = high -> []
  | _ -> low :: (list_range (low+1) high)

let rec times i f =
  if i = 0 then []
  else (f ()) :: (times (i-1) f)

let on_buffer ?(margin=80) f t =
  let buff = Buffer.create 100 in
  let formatter = Format.formatter_of_buffer buff in
  Format.pp_set_margin formatter margin;
  f formatter t;
  Format.fprintf formatter "@?";
  Buffer.contents buff

let sprintf format =
  let buffer = Buffer.create 512 in
  let fmt = Format.formatter_of_buffer buffer in
  Format.kfprintf
    (begin fun fmt ->
    Format.pp_print_flush fmt ();
    let s = Buffer.contents buffer in
    Buffer.clear buffer;
    s
    end)
  fmt
  format

(** print a list of items using the printing function *)
let rec pp_list ?(sep=", ") pp_item formatter = function
  | x::y::xs -> Format.fprintf formatter "%a%s@,%a"
      pp_item x sep (pp_list ~sep:sep pp_item) (y::xs)
  | x::[] -> pp_item formatter x
  | [] -> ()

(** print an array of items using the printing function *)
let rec pp_array ?(sep=", ") pp_item formatter a =
  match a with
  | [||] -> ()
  | [|x|] -> pp_item formatter 0 x
  | _ ->
    for i = 0 to Array.length a -1 do
      (if i < Array.length a - 1 then Format.pp_print_string formatter sep);
      pp_item formatter i a.(i)
    done

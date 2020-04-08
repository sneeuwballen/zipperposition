
(* This file is free software, part of Zipperposition. See file "license" for more details. *)

(** {1 Syntactic Ordinals} *)

module Fmt = CCFormat

type t =
  | Zero
  | Sum of (int * t) list
  (* invariant: the list is sorted by decreasing power *)

let rec pp out = function
  | Zero -> Fmt.string out "0"
  | Sum [] -> assert false
  | Sum l ->
    let pp_pair out (n,b) =
      if b=Zero then Fmt.int out n
      else Fmt.fprintf out "@<1>%d · ω^@[%a@]" n pp_inner b
    in
    Fmt.fprintf out "%a" (Util.pp_list ~sep:" + " pp_pair) l

(* wrap with "()" *)
and pp_inner out = function
  | Zero -> pp out Zero
  | Sum [_] as x -> pp out x
  | x -> Fmt.within "(" ")" pp out x

let to_string = Fmt.to_string pp

let zero = Zero

let mult_const n x =
  assert (n>=0);
  if n=0 then zero
  else Sum [n, x]

let const n = mult_const n zero

let rec compare (a:t) (b:t): int = match a, b with
  | Zero, Zero -> 0
  | Zero, Sum _ -> -1
  | Sum _, Zero -> 1
  | Sum l1, Sum l2 ->
    CCList.compare compare_pair l1 l2

and compare_pair (n1,b1)(n2,b2) =
  let c = compare b1 b2 in
  if c=0 then CCInt.compare n1 n2 else c

let equal a b = compare a b = 0

let rec add (a:t) (b:t) : t = match a, b with
  | Zero, Zero -> zero
  | Zero, _ -> b
  | _, Zero -> a
  | Sum l1, Sum l2 -> Sum (add_l l1 l2)

(* merge of two sorted lists *)
and add_l l1 l2 : (int * t) list = match l1, l2 with
  | [], [] -> []
  | [], _ -> l2
  | _, [] -> l1
  | (n1,b1) :: tail1, (n2,b2) :: tail2 ->
    let c = compare b1 b2 in
    if c=0 then (n1+n2, b1) :: add_l tail1 tail2
    else if c>0 then (n1,b1) :: add_l tail1 l2
    else (n2,b2) :: add_l l1 tail2

let rec of_list l = match l with
  | [] -> zero
  | [i,b] -> mult_const i b
  | (i,b) :: tail -> add (mult_const i b) (of_list tail)

(* sort the list and sum same-power coefficients *)
let normalize_ (l:(int*t) list): (int*t) list =
  let rec merge_succ l = match l with
    | [] | [_] -> l
    | (0,_) :: tail -> merge_succ tail
    | (n1,b1) :: (((n2,b2) :: tail2) as tail) ->
      let c = compare b1 b2 in
      assert (c >= 0);
      if c=0
      then merge_succ ((n1+n2,b1) :: tail2)
      else (n1,b1) :: merge_succ tail
  in
  (* sort by decreasing order *)
  List.sort (fun pair1 pair2 -> compare_pair pair2 pair1) l
  |> merge_succ

let rec mult (a:t) (b:t) : t = match a, b with
  | Zero, _ | _, Zero -> zero
  | Sum l1, Sum l2 -> Sum (mult_l l1 l2)

(* product of two lists *)
and mult_l l1 l2 : (int * t) list =
  CCList.product
    (fun (i1,b1) (i2,b2) ->
       let i = i1 * i2 in
       assert (i>0);
       i, add b1 b2)
    l1 l2
  |> normalize_

(* check invariants *)
let check_inv_ t =
  let rec check_ord_ : t -> bool = function
    | Zero -> true
    | Sum [] -> false
    | Sum l ->
      CCList.is_sorted ~cmp:(CCOrd.opp compare_pair) l &&
      List.for_all (fun (_,b) -> check_ord_ b) l
  in
  check_ord_ t



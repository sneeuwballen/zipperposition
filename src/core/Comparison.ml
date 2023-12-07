
(* This file is free software, part of Logtk. See file "license" for more details. *)

(** {1 Partial Ordering values} *)

(** {2 Combined nonstrict-strict partial orders} *)

type t = Lt | Leq | Eq | Geq | Gt | Incomparable

type comparison = t
let equal : t -> t -> bool = Stdlib.(=)

let to_string = function
  | Lt -> "<"
  | Leq -> "<="
  | Eq -> "="
  | Geq -> ">="
  | Gt -> ">"
  | Incomparable -> "<=>?"

let pp out c = CCFormat.string out (to_string c)

let opp cmp = match cmp with
  | Eq | Incomparable -> cmp
  | Lt -> Gt
  | Gt -> Lt
  | Leq -> Geq
  | Geq -> Leq

let to_total ord = match ord with
  | Lt -> -1
  | Gt -> 1
  | Geq | Leq | Eq | Incomparable -> 0

let of_total ord = match ord with
  | 0 -> Eq
  | x when x > 0 -> Gt
  | _ -> Lt

let merge_with_Geq = function
  | Lt | Leq -> Incomparable
  | Eq -> Geq
  | cmp -> cmp

let merge_with_Leq = function
  | Gt | Geq -> Incomparable
  | Eq -> Leq
  | cmp -> cmp

let smooth = function
  | Gt -> Geq
  | Lt -> Leq
  | cmp -> cmp

type 'a comparator = 'a -> 'a -> t

let (@>>) f g x y =
  match f x y with
  | Geq -> merge_with_Geq (g x y)
  | Eq -> g x y
  | Leq -> merge_with_Leq (g x y)
  | cmp -> cmp

let is_Gt_or_Geq = function
  | Gt | Geq -> true
  | _ -> false

let is_Gt_or_Geq_or_Eq = function
  | Gt | Geq | Eq -> true
  | _ -> false

let is_Lt_or_Leq = function
  | Lt | Leq -> true
  | _ -> false

let is_Lt_or_Leq_or_Eq = function
  | Lt | Leq | Eq -> true
  | _ -> false

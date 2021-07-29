
(* This file is free software, part of Logtk. See file "license" for more details. *)

(** {1 Partial Ordering values} *)

(** {2 Strict partial orders} *)

type t = Lt | Eq | Gt | Incomparable
(** partial order *)

type comparison = t
let equal : t -> t -> bool = Pervasives.(=)

let to_string = function
  | Lt -> "<"
  | Eq -> "="
  | Gt -> ">"
  | Incomparable -> "<?>"

let pp out c = CCFormat.string out (to_string c)

let combine cmp1 cmp2 = match cmp1, cmp2 with
  | Eq, Eq
  | Eq, Incomparable | Incomparable, Eq -> Eq
  | Lt, Incomparable | Incomparable, Lt -> Lt
  | Gt, Incomparable | Incomparable, Gt -> Gt
  | Incomparable, Incomparable -> Incomparable
  | _ ->
    invalid_arg "inconsistent comparisons"

let opp cmp = match cmp with
  | Eq | Incomparable -> cmp
  | Lt -> Gt
  | Gt -> Lt

let to_total ord = match ord with
  | Lt -> -1
  | Gt -> 1
  | Eq | Incomparable -> 0

let of_total ord = match ord with
  | 0 -> Eq
  | x when x > 0 -> Gt
  | _ -> Lt

let lexico a b = match a with
  | Incomparable -> b
  | _ -> a

type 'a comparator = 'a -> 'a -> t

let (++) = lexico

let (@>>) f g x y =
  match f x y with
  | Eq -> g x y
  | res -> res

type ('a, 'b) combination = {
  call : 'a -> 'a -> 'b;
  ignore : t -> 'a -> 'a -> 'b;
}

let last f = {
  call = f;
  ignore = (fun res _ _ -> res);
}

let call f x y = f.call x y

let dominates f l1 l2 =
  let rec find_x l1 y = match l1 with
    | [] -> false
    | x::l1' ->
      match f x y with
      | Gt -> true
      | _ -> find_x l1' y
  and check_all l2 = match l2 with
    | [] -> true
    | y :: l2' -> find_x l1 y && check_all l2'
  in
  check_all l2;;

module type PARTIAL_ORD = sig
  type t

  val partial_cmp : t -> t -> comparison
end

(** {2 Combined nonstrict-strict partial orders} *)

module Nonstrict = struct

type t = Lt | Leq | Eq | Geq | Gt | Incomparable

type comparison = t
let equal : t -> t -> bool = Pervasives.(=)

let to_string = function
  | Lt -> "<"
  | Leq -> "<="
  | Eq -> "="
  | Geq -> ">="
  | Gt -> ">"
  | Incomparable -> "<?>"

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

let sharpen = function
  | Geq -> Gt
  | Leq -> Lt
  | cmp -> cmp

type 'a comparator = 'a -> 'a -> t

let (@>>) f g x y =
  match f x y with
  | Geq -> merge_with_Geq (g x y)
  | Eq -> g x y
  | Leq -> merge_with_Leq (g x y)
  | cmp -> cmp

end

let is_Gt_or_Geq = function
  | Nonstrict.Gt | Geq -> true
  | _ -> false

let is_Lt_or_Leq = function
  | Nonstrict.Lt | Leq -> true
  | _ -> false

let of_nonstrict = function
  | Nonstrict.Lt -> Lt
  | Eq -> Eq
  | Gt -> Gt
  | Leq | Geq | Incomparable -> Incomparable

let to_nonstrict = function
  | Lt -> Nonstrict.Lt
  | Eq -> Eq
  | Gt -> Gt
  | Incomparable -> Incomparable


(* This file is free software, part of Logtk. See file "license" for more details. *)

(** {1 Partial Ordering values} *)

(** {2 Combined nonstrict-strict partial orders} *)

type nonstrict_t = NLt | NLeq | NEq | NGeq | NGt | NIncomparable

type nonstrict_comparison = nonstrict_t
let equal : nonstrict_t -> nonstrict_t -> bool = Pervasives.(=)

let to_string = function
  | NLt -> "=<="
  | NLeq -> "<=="
  | NEq -> "==="
  | NGeq -> "==>"
  | NGt -> "=>="
  | NIncomparable -> "=?="

let pp out c = CCFormat.string out (to_string c)

(** {2 Strict partial orders} *)

type t = Lt | Eq | Gt | Incomparable
(** partial order *)

type comparison = t
let equal : t -> t -> bool = Pervasives.(=)

let to_string = function
  | Lt -> "=<="
  | Eq -> "==="
  | Gt -> "=>="
  | Incomparable -> "=?="

let pp out c = CCFormat.string out (to_string c)

let strict_of_nonstrict cmp = match cmp with
  | NLt -> Lt
  | NEq -> Eq
  | NGt -> Gt
  | NLeq | NGeq | NIncomparable -> Incomparable

let nonstrict_of_strict cmp = match cmp with
  | Lt -> NLt
  | Eq -> NEq
  | Gt -> NGt
  | Incomparable -> NIncomparable

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

type ('a,'b) combination = {
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

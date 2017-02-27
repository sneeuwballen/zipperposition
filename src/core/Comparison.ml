
(* This file is free software, part of Logtk. See file "license" for more details. *)

(** {1 Partial Ordering values} *)

type t = Lt | Eq | Gt | Incomparable
(** partial order *)

type comparison = t

let to_string = function
  | Lt -> "=<="
  | Gt -> "=>="
  | Eq -> "==="
  | Incomparable -> "=?="

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

type ('a,'b) combination = {
  call : 'a -> 'a -> 'b;
  ignore : t -> 'a -> 'a -> 'b;
}

let last f = {
  call = f;
  ignore = (fun res _ _ -> res);
}

let (>>>) f g = {
  call = (fun x y -> match f x y with
    | Incomparable -> g.call
    | res -> g.ignore res
  );
  ignore = (fun res _ _ -> g.ignore res);
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

(*
Copyright (c) 2013, Simon Cruanes
All rights reserved.

Redistribution and use in source and binary forms, with or without
modification, are permitted provided that the following conditions are met:

Redistributions of source code must retain the above copyright notice, this
list of conditions and the following disclaimer.  Redistributions in binary
form must reproduce the above copyright notice, this list of conditions and the
following disclaimer in the documentation and/or other materials provided with
the distribution.

THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS "AS IS" AND
ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED
WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE ARE
DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT HOLDER OR CONTRIBUTORS BE LIABLE
FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL
DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR
SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER
CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY,
OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE
OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
*)

(** {1 Partial LogtkOrdering values} *)

type t = Lt | Eq | Gt | Incomparable
  (** partial order *)

type comparison = t

let to_string = function
  | Lt -> "=<="
  | Gt -> "=>="
  | Eq -> "==="
  | Incomparable -> "=?="

let combine cmp1 cmp2 = match cmp1, cmp2 with
  | Eq, Eq
  | Eq, Incomparable | Incomparable, Eq -> Eq
  | Lt, Incomparable | Incomparable, Lt -> Lt
  | Gt, Incomparable | Incomparable, Gt -> Gt
  | Incomparable, Incomparable -> Incomparable
  | _ ->
    raise (Invalid_argument "inconsistent comparisons") (* not compatible *)

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
  ignore = (fun res x y -> res);
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


(* This file is free software, part of Zipperposition. See file "license" for more details. *)

(** {1 Boolean Trail} *)

open Hornet_types

type t = bool_trail

let cmp_blit = Hornet_types_util.compare_bool_lit
let equal_blit = Hornet_types_util.equal_bool_lit
let neg_blit = Hornet_types_util.neg_bool_lit
let cmp_ (lazy a)(lazy b) = cmp_blit a b
let is_empty = function [] -> true | _::_ -> false

let empty = []

let make l = CCList.sort_uniq ~cmp:cmp_ l

let merge l1 l2 =
  CCList.sorted_merge_uniq ~cmp:cmp_ l1 l2

let exists f l = List.exists (fun (lazy blit) -> f blit) l
let to_list = List.map Lazy.force
let of_list = List.map Lazy.from_val

let subsumes l1 l2: bool =
  let rec aux l1 l2 = match l1, l2 with
    | [], _ -> true
    | _, [] -> false
    | lazy t1 :: tail1, lazy t2 :: tail2 ->
      begin match cmp_blit t1 t2 with
        | 0 -> aux tail1 tail2
        | n when n<0 -> false (* all elements of [l2] are bigger than [t1] *)
        | _ -> aux l1 tail2 (* drop [t2] *)
      end
  in
  aux l1 l2

(* absurd if it contains [a] and [not a] *)
let is_absurd l =
  List.exists
    (fun (lazy a) ->
       List.exists (fun (lazy b) -> equal_blit a (neg_blit b)) l)
    l

let pp = Hornet_types_util.pp_bool_trail
let pp_opt = Hornet_types_util.pp_bool_trail_opt
let to_string = CCFormat.to_string pp
let equal = Hornet_types_util.equal_bool_trail

let bool_lits (t:t): bool_lit Sequence.t =
  Sequence.of_list t
  |> Sequence.map Lazy.force

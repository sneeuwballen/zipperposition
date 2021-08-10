
(* This file is free software, part of Zipperposition. See file "license" for more details. *)

(** {1 Simplification Monad} *)

type +'a t = 'a * [ `Same | `New]

let return_same x = x, `Same
let return_new x = x, `New
let return = return_same

let get = fst
let is_new (_,st) = match st with `New -> true | `Same -> false
let is_same (_,st) = match st with `New -> false | `Same -> true

let map f (x,st) = f x, st

let return_opt ~old = function
  | None -> return_same old
  | Some x -> return_new x

(* chaining simplifications *)
let (>>=) (c,state) f =
  let c', state2 = f c in
  let state_res = match state, state2 with
    | `Same, `Same -> `Same
    | `New, _ | _, `New -> `New
  in
  c', state_res

let rec app_list l x = match l with
  | [] -> return_same x
  | f :: l' -> f x >>= app_list l'

let rec map_l f l = match l with
  | [] -> return_same []
  | x :: tail ->
    f x >>= fun x' ->
    map_l f tail >>= fun tail' ->
    return_same (x' :: tail')

let rec fold_l f acc l = match l with
  | [] -> return_same acc
  | x :: tail ->
    f acc x >>= fun acc -> fold_l f acc tail

module Infix = struct
  let (>>=) = (>>=)
  let (>|=) x f = map f x
end

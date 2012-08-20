(** test terms *)

open Types

module T = Terms
module H = Helpers
module Utils = FoUtils

let preds = ["p"; "q"; "r"; "s"]
let funs = ["f"; "g"; "h"]
let symbols = ["a"; "b"; "c"]

(** random term *)
let random_term () =
  let depth = H.random_in 0 3 in
  let rec aux depth = match depth with
  | 0 -> random_leaf ()
  | n ->
    let arity = H.random_in 1 3
    and head = random_fun () in
    let subterms =
      Utils.times arity (fun _ -> aux (depth - (H.random_in 1 2)))
    in
    T.mk_node (head::subterms)
  and random_leaf () =
    if H.R.bool ()
      then T.mk_leaf (H.choose symbols) T.univ_sort
      else T.mk_var (H.random_in 0 3) T.univ_sort
  and random_fun () =
    T.mk_leaf (H.choose funs) T.univ_sort
  in aux depth

(** random bool-sorted term *)
let random_pred () =
  let p = T.mk_leaf (H.choose preds) T.bool_sort in
  let arity = H.random_in 0 3 in
  if arity = 0
    then p
    else T.mk_node (p::(Utils.times arity random_term))

let run () =
  Format.printf "run terms test@."

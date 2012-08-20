(** helpers for tests *)

open Types

module R = Random
module Utils = FoUtils

let seed = 42   (* use a fixed seed for random, for repeatable tests *)

let random_in lower higher =
  assert (lower <= higher);
  (R.int (higher - lower)) + lower

let choose l =
  let len = List.length l in
  let idx = random_in 0 len in
  Utils.list_get l idx

let random_list size mk_elem =
  let rec aux acc = function
  | 0 -> acc
  | size -> aux ((mk_elem ())::acc) (size-1)
  in aux []

let () =
  R.init seed

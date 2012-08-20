(** helpers for tests *)

open Types

module R = Random
module Utils = FoUtils

let seed = 42   (* use a fixed seed for random, for repeatable tests *)

(** a test result on 'a *)
type 'a test_result = TestOk | TestFail of 'a | TestPreconditionFalse

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

(** check the property on elements from the given generator, num times.
    Returns (num, number of success, list of failures, number of no preconditions) *)
let check_prop prop generator num = 
  let failures = ref []
  and success_count = ref 0
  and noprecond_count = ref 0 in
  for i = 1 to num do
    match prop (generator ()) with
    | TestOk -> incr success_count
    | TestPreconditionFalse -> incr noprecond_count
    | TestFail _ as r -> failures := r :: !failures
  done;
  (num, !success_count, !failures, !noprecond_count)

(** print results *)
let print_results ~name (num, success_count, failures, noprecond_count) pp_element =
  let print_failure formater res = match res with
  | TestFail a -> pp_element formater a
  | _ -> assert false
  in
  Format.printf "%d tests for %s : %d success, %d failures, %d preconditions failed@."
    num name success_count (List.length failures) noprecond_count;
  if failures <> []
    then Format.printf "failures @[<v>%a@]@." (Utils.pp_list ~sep:"" print_failure) failures
    else ()

(** check property and then print results *)
let check_and_print ~name prop generator pp_element num =
  let res = check_prop prop generator num in
  print_results ~name res pp_element

let () =
  R.init seed

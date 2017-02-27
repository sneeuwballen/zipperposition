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

(** helpers for tests *)

open Logtk
module R = Random

let seed = 42   (* use a fixed seed for random, for repeatable tests *)

let random_in lower higher =
  assert (lower <= higher);
  (R.int (higher - lower)) + lower

(** a test result on 'a *)
type 'a test_result = TestOk | TestFail of 'a | TestPreconditionFalse

let choose l =
  let len = List.length l in
  let idx = random_in 0 len in
  List.nth l idx

let random_list _size mk_elem =
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
  for _i = 1 to num do
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
    then Format.printf "failures @[%a@]@." (Util.pp_list ~sep:"" print_failure) failures
    else ()

(** check property and then print results *)
let check_and_print ~name prop generator pp_element num =
  let res = check_prop prop generator num in
  print_results ~name res pp_element

let () =
  R.init seed

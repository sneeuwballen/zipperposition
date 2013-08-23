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

(** Testing of rewriting *)

open OUnit

module T = Term
module S = Substs
module Rw = Rewriting.TRS

let a = T.mk_const (Symbol.mk_symbol "a")
let b = T.mk_const (Symbol.mk_symbol "b") 
let c = T.mk_const (Symbol.mk_symbol "c") 
let d = T.mk_const (Symbol.mk_symbol "d") 
let f x y = T.mk_node (Symbol.mk_symbol "f") [x; y]
let g x = T.mk_node (Symbol.mk_symbol "g") [x]
let h x = T.mk_node (Symbol.mk_symbol "h") [x]
let zero = T.mk_const (Symbol.mk_symbol "0") 
let succ n = T.mk_node (Symbol.mk_symbol "s") [n]
let plus a b = T.mk_node (Symbol.mk_symbol "+") [a; b]
let minus a = T.mk_node (Symbol.mk_symbol "-") [a]
let times a b = T.mk_node (Symbol.mk_symbol "x") [a; b]
let x = T.mk_var 1
let y = T.mk_var 2
let z = T.mk_var 3
let u = T.mk_var 4

let rec from_int n =
  assert (n >= 0);
  if n = 0 then zero else succ (from_int (n-1))

(** convert Peano term t to int *)
let peano_to_int t =
  let rec count t n =
    match t.T.term with
    | _ when T.eq t zero -> n
    | T.Node (s, [t2]) when Symbol.eq s (Symbol.mk_symbol "s") -> count t2 (n+1)
    | _ -> failwith "not peano!"
  in count t 0

(** print a term with a nice representation for Peano numbers *)
let rec print_peano_nice buf t =
  try
    Printf.bprintf buf "%d" (peano_to_int t)
  with Failure _ ->
    match t.T.term with
    | T.Var _ | T.BoundVar _ -> T.pp buf t
    | T.Bind (s, t') ->
      Printf.bprintf buf "%a(%a)" Symbol.pp s print_peano_nice t'
    | T.Node (h, []) -> Printf.bprintf buf "%a" Symbol.pp h
    | T.Node (h, l) -> Printf.bprintf buf "%a(%a)" Symbol.pp h (Util.pp_list T.pp) l
    | T.At _ -> assert false

(** Simple rewriting system for Peano arithmetic with + and x *)
let peano_trs =
  Rw.of_list
    [ (plus (succ x) y, succ (plus x y));
      (plus zero x, x);
      (times (succ x) y, plus (times x y) y);
      (times zero x, zero);
    ]

(** associative group theory: -y+y=0, x+0=x, (x+y)+z=x+(y+z) *)
let group_trs =
  Rw.of_list
    [ (plus zero x, x);
      (plus (minus x) x, zero);
      (plus (plus x y) z, plus x (plus y z));
    ]

(** check equality of normal forms *)
let test trs t1 t2 =
  Util.debug 3 "test with %a %a" T.pp t1 T.pp t2;
  let t1' = Rw.rewrite trs t1 in
  let t2' = Rw.rewrite trs t2 in
  Util.debug 3 "normal form of %a = normal form of %a (ie %a)"
                print_peano_nice t1 print_peano_nice t2 print_peano_nice t1';
  OUnit.assert_equal ~printer:T.to_string ~cmp:T.eq t1' t2';
  ()

(** compute normal form of (n+n) in peano TRS *)
let test_peano n () =
  let a = plus (from_int n) (from_int n)
  and b = from_int (2 * n) in
  test peano_trs a b

(** compute normal form of n+n and 2xn in Peano *)
let test_peano_bis n () =
  let a = plus (from_int n) (from_int n)
  and b = times (from_int 2) (from_int n) in
  test peano_trs a b
  

let tests:(unit -> unit) list =
  [ test_peano 2; test_peano 4; test_peano 100; test_peano 1000;
    test_peano_bis 2; test_peano_bis 4; test_peano_bis 100; test_peano_bis 1000 ]

let benchmark_count = 1  (* with caching, not accurate to do it several times *)

let benchmark ?(count=benchmark_count) trs a b =
  (* rewrite to normal form *)
  let one_step () =
    let a' = Rw.rewrite trs a
    and b' = Rw.rewrite trs b in
    OUnit.assert_equal ~printer:T.to_string ~cmp:T.eq a' b';
  in
  Gc.major ();
  let start = Unix.gettimeofday () in
  for i = 1 to count do one_step () done;
  let stop = Unix.gettimeofday () in
  Util.printf "%f seconds to do %d joins of %a and %a (%f each)\n"
    (stop -. start) count print_peano_nice a print_peano_nice b
    ((stop -. start) /. (float_of_int count))

let benchmark_peano n () =
  let a = plus (from_int n) (from_int n)
  and b = times (from_int 2) (from_int n) in
  benchmark peano_trs a b

let benchmark_peano_bis n () =
  let a = plus (plus (from_int n) (from_int n)) (plus (from_int n) (from_int n))
  and b = times (from_int 4) (from_int n) in
  benchmark peano_trs a b

let suite =
  "test_rewriting" >:::
    [ "test-peano2" >:: test_peano 2
    ; "test-peano4" >:: test_peano 4
    ; "test-peano100" >:: test_peano 100
    ; "test-peano1000" >:: test_peano 1000
    ; "test-peano-bis2" >:: test_peano_bis 2 
    ; "test-peano-bis4" >:: test_peano_bis 4
    ; "test-peano-bis50" >:: test_peano_bis 50
    ; "test-peano-bis500" >:: test_peano_bis 500
    ; "bench-peano2" >:: benchmark_peano 2
    ; "bench-peano4" >:: benchmark_peano 4
    ; "bench-peano100" >:: benchmark_peano 100
    ; "bench-peano1000" >:: benchmark_peano 1000
    ; "bench-peano-bis2" >:: benchmark_peano_bis 2 
    ; "bench-peano-bis4" >:: benchmark_peano_bis 4
    ; "bench-peano-bis50" >:: benchmark_peano_bis 50
    ; "bench-peano-bis500" >:: benchmark_peano_bis 500
    ]


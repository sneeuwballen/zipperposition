
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

(** {1 Benchmarks} *)

open Logtk

module T = Term

let a = T.mk_const (Symbol.mk_const "a")
let b = T.mk_const (Symbol.mk_const "b")
let c = T.mk_const (Symbol.mk_const "c")
let d = T.mk_const (Symbol.mk_const "d")
let f x y = T.mk_node (Symbol.mk_const "f") [x; y]
let g x = T.mk_node (Symbol.mk_const "g") [x]
let h x = T.mk_node (Symbol.mk_const "h") [x]
let zero = T.mk_const (Symbol.mk_const "0")
let succ n = T.mk_node (Symbol.mk_const "s") [n]
let plus a b = T.mk_node (Symbol.mk_const "+") [a; b]
let minus a = T.mk_node (Symbol.mk_const "-") [a]
let times a b = T.mk_node (Symbol.mk_const "x") [a; b]
let x = T.mk_var 1
let y = T.mk_var 2
let z = T.mk_var 3
let u = T.mk_var 4

let rec from_int n =
  assert (n >= 0);
  if n = 0 then zero else succ (from_int (n-1))

(* deterministic gen *)
let mk_rand () =
  Random.State.make [| 42 |]

module MakeBench(I : functor(E : Index.EQUATION) -> Index.UNIT_IDX with module E = E) = struct
  module TRS = Rewriting.MakeTRS(I)

  (** Simple rewriting system for Peano arithmetic with + and x *)
  let peano_trs =
    TRS.of_list
      [ (plus (succ x) y, succ (plus x y));
        (plus zero x, x);
        (times (succ x) y, plus (times x y) y);
        (times zero x, zero);
      ]

  let bench_peano n () =
    let a = plus (from_int n) (from_int n)
    and b = from_int (2 * n) in
    let a' = TRS.rewrite peano_trs a in
    let b' = TRS.rewrite peano_trs b in
    assert (T.eq a' b');
    ()  (* TODO: rewriting big Peano numbers *)

  (* trivial "equation" *)
  module E2 = struct
    type t = T.t * unit
    type rhs = unit
    let extract (t,()) = t, (), true
    let compare (t1,_) (t2,_) = T.compare t1 t2
    let priority _ = 1
  end

  module Idx2 = I(E2)

  let bench_random n () =
    let terms = QCheck.Arbitrary.generate ~rand:(mk_rand()) ~n T.arbitrary in
    let idx = Idx2.add_seq Idx2.empty
      (Sequence.map (fun t -> t,()) (Sequence.of_list terms)) in
    List.iter
      (fun t ->
        Idx2.retrieve ~sign:true idx 1 t 0 () (fun _ _ _ _ _ -> ()))
      terms;
    ()
end

let run_bench () =
  let module B_Dtree = MakeBench(Dtree.Make) in
  let module B_NPDtree = MakeBench(NPDtree.Make) in
  let conf = Bench.config in
  let old_sample = conf.Bench.samples in
  conf.Bench.samples <- 100;
  Bench.bench
    [ "dtree_peano_1000", B_Dtree.bench_peano 1000
    ; "npdtree_peano_1000", B_NPDtree.bench_peano 1000
    ];
  Bench.bench
    [ "dtree_peano_10_000", B_Dtree.bench_peano 10_000
    ; "npdtree_peano_10_000", B_NPDtree.bench_peano 10_000
    ];
  Bench.bench
    [ "dtree_rand_1000", B_Dtree.bench_random 1000
    ; "npdtree_rand_1000", B_NPDtree.bench_random 1000
    ];
  conf.Bench.samples <- old_sample;
  ()

let _ = run_bench ()

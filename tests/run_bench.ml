
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
open Logtk_arbitrary

module T = FOTerm

let a = T.mk_const ~ty:Type.i (Symbol.mk_const "a")
let b = T.mk_const ~ty:Type.i (Symbol.mk_const "b")
let c = T.mk_const ~ty:Type.i (Symbol.mk_const "c")
let d = T.mk_const ~ty:Type.i (Symbol.mk_const "d")
let f x y = T.mk_node ~ty:Type.i (Symbol.mk_const "f") [x; y]
let g x = T.mk_node ~ty:Type.i (Symbol.mk_const "g") [x]
let h x = T.mk_node ~ty:Type.i (Symbol.mk_const "h") [x]
let zero = T.mk_const ~ty:Type.i (Symbol.mk_const "0")
let succ n = T.mk_node ~ty:Type.i (Symbol.mk_const "s") [n]
let plus a b = T.mk_node ~ty:Type.i (Symbol.mk_const "+") [a; b]
let minus a = T.mk_node ~ty:Type.i (Symbol.mk_const "-") [a]
let times a b = T.mk_node ~ty:Type.i (Symbol.mk_const "x") [a; b]
let x = T.mk_var ~ty:Type.i 1
let y = T.mk_var ~ty:Type.i 2
let z = T.mk_var ~ty:Type.i 3
let u = T.mk_var ~ty:Type.i 4

let rec from_int n =
  assert (n >= 0);
  if n = 0 then zero else succ (from_int (n-1))

(* deterministic gen *)
let rand =
  Random.State.make [| 42 |]

(** {2 Benchmark rewriting indexes} *)

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
    let terms = QCheck.Arbitrary.generate ~rand ~n ArTerm.default in
    let idx = Idx2.add_seq (Idx2.empty ())
      (Sequence.map (fun t -> t,()) (Sequence.of_list terms)) in
    List.iter
      (fun t ->
        Idx2.retrieve ~sign:true idx 1 t 0 () (fun _ _ _ _ _ -> ()))
      terms;
    ()
end

(** {2 Benchmark indexes} *)

module MakeIdxBench(I : Index.TERM_IDX with type elt = int) = struct
  let idx_of_terms terms =
    let idx, _ = List.fold_left
      (fun (idx,i) t -> I.add idx t i, i+1)
      (I.empty (),0) terms
    in
    idx

  let bench idx terms () =
    List.iter
      (fun t ->
        I.retrieve_unifiables idx 0 t 1 ()
          (fun () t' i subst -> ()))
      terms
end

module OrderedInt = struct type t = int let compare i j = i-j end
module IntFingerprint = Fingerprint.Make(OrderedInt)
module BenchFingerprint = MakeIdxBench(IntFingerprint)
module IntFastFingerprint = FastFingerprint.Make(OrderedInt)
module BenchFastFingerprint = MakeIdxBench(IntFastFingerprint)

module BenchFingerprint16 = MakeIdxBench(struct include IntFingerprint let empty () = empty_with Fingerprint.fp16 end)
module BenchFastFingerprint16 = MakeIdxBench(struct include IntFastFingerprint let empty () = empty_with FastFingerprint.fp16 end)

module IntNPDtree = NPDtree.MakeTerm(OrderedInt)
module BenchNPDTree = MakeIdxBench(IntNPDtree)

let bench_idx n =
  let terms = QCheck.Arbitrary.generate ~rand ~n ArTerm.default in
  let ifinger = BenchFingerprint.idx_of_terms terms in
  let ifastfinger = BenchFastFingerprint.idx_of_terms terms in
  let ifinger16 = BenchFingerprint16.idx_of_terms terms in
  let ifastfinger16 = BenchFastFingerprint16.idx_of_terms terms in
  let inpdtree = BenchNPDTree.idx_of_terms terms in
  Bench.bench
    [ Util.sprintf "bench_fingerprint_%d" n, BenchFingerprint.bench ifinger terms
    ; Util.sprintf "bench_fast_fingerprint_%d" n, BenchFastFingerprint.bench ifastfinger terms
    ; Util.sprintf "bench_fingerprint_fp16_%d" n, BenchFingerprint16.bench ifinger16 terms
    ; Util.sprintf "bench_fast_fingerprint_fp16_%d" n, BenchFastFingerprint16.bench ifastfinger16 terms
    ; Util.sprintf "bench_npdtree_%d" n, BenchNPDTree.bench inpdtree terms
    ]

(** {2 Type checking} *)

let bench_type_inf n =
  let terms = QCheck.Arbitrary.generate ~rand ~n
    ArTerm.ArbitraryUntyped.default in
  let ctx = TypeInference.Ctx.create () in
  List.iter
    (fun t -> ignore (TypeInference.FO.infer ctx t 0))
    terms;
  ()

(** {2 Main} *)

let draw_line () =
  Printf.printf "\n--------------------------------------------------------\n";
  ()

let run_bench () =
  let module B_Dtree = MakeBench(Dtree.Make) in
  let module B_NPDtree = MakeBench(NPDtree.Make) in
  let conf = Bench.config in
  let old_sample = conf.Bench.samples in
  conf.Bench.samples <- 100;
  (* type inference *)
  draw_line ();
  let res = Bench.bench_arg ["type_inf_200", bench_type_inf, 200] in
  Bench.summarize 1. res;
  draw_line ();
  (* indexing *)
  bench_idx 1000;
  draw_line ();
  bench_idx 2_000;
  draw_line();
  (* rewriting *)
  Bench.bench
    [ "dtree_peano_1000", B_Dtree.bench_peano 1000
    ; "npdtree_peano_1000", B_NPDtree.bench_peano 1000
    ];
  draw_line ();
  Bench.bench
    [ "dtree_peano_5_000", B_Dtree.bench_peano 5_000
    ; "npdtree_peano_5_000", B_NPDtree.bench_peano 5_000
    ];
  draw_line ();
  Bench.bench
    [ "dtree_rand_1000", B_Dtree.bench_random 1000
    ; "npdtree_rand_1000", B_NPDtree.bench_random 1000
    ];
  conf.Bench.samples <- old_sample;
  ()

let _ = run_bench ()

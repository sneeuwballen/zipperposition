
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

module T = Term

let _const ~ty s =
  T.const ~ty (Symbol.of_string s)

let a = T.const ~ty:Type.TPTP.i (Symbol.of_string "a")
let b = T.const ~ty:Type.TPTP.i (Symbol.of_string "b")
let c = T.const ~ty:Type.TPTP.i (Symbol.of_string "c")
let d = T.const ~ty:Type.TPTP.i (Symbol.of_string "d")
let f x y = T.app (_const ~ty:Type.TPTP.i "f") [x; y]
let g x = T.app (_const ~ty:Type.(TPTP.i <=. TPTP.i) "g") [x]
let h x = T.app (_const ~ty:Type.(TPTP.i <=. TPTP.i) "h") [x]
let zero = _const ~ty:Type.TPTP.i "0"
let succ n = T.app (_const ~ty:Type.(TPTP.i <=. TPTP.i) "s") [n]
let plus a b = T.app (_const ~ty:Type.(TPTP.i <== [TPTP.i;TPTP.i]) "+") [a; b]
let minus a = T.app (_const ~ty:Type.(TPTP.i <=. TPTP.i) "-") [a]
let times a b = T.app (_const ~ty:Type.(TPTP.i <== [TPTP.i;TPTP.i]) "x") [a; b]
let x = T.var ~ty:Type.TPTP.i 1
let y = T.var ~ty:Type.TPTP.i 2
let z = T.var ~ty:Type.TPTP.i 3
let u = T.var ~ty:Type.TPTP.i 4

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

  let bench_peano n =
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
    let compare (t1,_) (t2,_) = T.cmp t1 t2
    let priority _ = 1
  end

  module Idx2 = I(E2)

  let bench_random n =
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

  let bench idx terms =
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

module BenchFingerprint16 = MakeIdxBench(struct
  include IntFingerprint
  let empty () = empty_with Fingerprint.fp16
end)
module BenchFastFingerprint16 = MakeIdxBench(struct
  include IntFastFingerprint
  let empty () = empty_with FastFingerprint.fp16
end)

module IntNPDtree = NPDtree.MakeTerm(OrderedInt)
module BenchNPDTree = MakeIdxBench(IntNPDtree)

let bench_idx n =
  let terms = List.filter
    (fun t -> not (T.is_var t))
    (QCheck.Arbitrary.generate ~rand ~n ArTerm.default) in
  let ifinger = BenchFingerprint.idx_of_terms terms in
  let ifastfinger = BenchFastFingerprint.idx_of_terms terms in
  let ifinger16 = BenchFingerprint16.idx_of_terms terms in
  let ifastfinger16 = BenchFastFingerprint16.idx_of_terms terms in
  let inpdtree = BenchNPDTree.idx_of_terms terms in
  let res = Benchmark.throughputN 3
    [ Util.sprintf "fingerprint_%d" n, BenchFingerprint.bench ifinger, terms
    ; Util.sprintf "fast_fingerprint_%d" n, BenchFastFingerprint.bench ifastfinger, terms
    ; Util.sprintf "fingerprint_fp16_%d" n, BenchFingerprint16.bench ifinger16, terms
    ; Util.sprintf "fast_fingerprint_fp16_%d" n, BenchFastFingerprint16.bench ifastfinger16, terms
    ; Util.sprintf "npdtree_%d" n, BenchNPDTree.bench inpdtree, terms
    ]
  in
  Benchmark.tabulate res

(** {2 Type checking} *)

let bench_type_inf n =
  let terms = QCheck.Arbitrary.generate ~rand ~n
    ArTerm.PT.default in
  let ctx = TypeInference.Ctx.create Signature.empty in
  List.iter
    (fun t -> ignore (TypeInference.FO.infer ctx t))
    terms;
  ()

(** {2 Term Hashconsing} *)

let bench_hashcons n =
  let terms = QCheck.Arbitrary.generate ~rand ~n ArTerm.PT.default in
  let ctx = TypeInference.Ctx.create Signature.empty in
  let terms = CCList.map
    (fun t ->
      let ty, t' = TypeInference.FO.infer_exn ctx t in
      TypeInference.Ctx.constrain_type_type ctx ty Type.TPTP.i;
      TypeInference.Ctx.bind_to_default ctx;
      t' ctx)
    terms
  in
  (* print hashconsing stats *)
  let print_hashcons_stats (sz, num, sum_length, small, median, big) =
    Util.printf ("hashcons stats for terms: size %d, num %d, sum length %d, "
                ^^ "buckets: small %d, median %d, big %d\n\n")
      sz num sum_length small median big;
    flush stdout;
  in
  (* terms can die now *)
  ignore terms;
  print_hashcons_stats (ScopedTerm.hashcons_stats ());
  ()

(** {2 Main} *)

let draw_line () =
  Printf.printf "\n--------------------------------------------------------\n";
  ()

let run_bench () =
  let module B_Dtree = MakeBench(Dtree.Make) in
  let module B_NPDtree = MakeBench(NPDtree.Make) in
  (* type inference *)
  draw_line ();
  let res = Benchmark.throughput1 2 ~name:"type_inf_200" bench_type_inf 200 in
  Benchmark.tabulate res;
  (* hashconsing *)
  List.iter
    (fun n ->
      draw_line();
      Util.printf "generate %d random terms...\n" n;
      flush stdout;
      Gc.major();
      bench_hashcons n;
    ) [ 1000; 100_000; 500_000 ];
  draw_line ();
  (* indexing *)
  bench_idx 1000;
  draw_line ();
  bench_idx 2_000;
  draw_line();
  (* rewriting *)
  let res = Benchmark.throughputN 2
    [ "dtree_peano_1000", B_Dtree.bench_peano, 1000
    ; "npdtree_peano_1000", B_NPDtree.bench_peano, 1000
    ]
  in
  Benchmark.tabulate res;
  draw_line ();
  let res = Benchmark.throughputN 2
    [ "dtree_peano_5_000", B_Dtree.bench_peano, 5_000
    ; "npdtree_peano_5_000", B_NPDtree.bench_peano, 5_000
    ]
  in
  Benchmark.tabulate res;
  draw_line ();
  let res = Benchmark.throughputN 2
    [ "dtree_rand_1000", B_Dtree.bench_random, 1000
    ; "npdtree_rand_1000", B_NPDtree.bench_random, 1000
    ]
  in
  Benchmark.tabulate res;
  ()

let _ = run_bench ()

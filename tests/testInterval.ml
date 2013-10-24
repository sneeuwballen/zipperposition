
(*
Zipperposition: a functional superposition prover for prototyping
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


(** {1 Test for intervals}

we only test integer intervals for now *)

open Logtk
open Libzipperposition
open QCheck

module I = Interval.Int

let _gen_bigint = Arbitrary.(lift Big_int.big_int_of_int (~-100 -- 100))
let _ppbi = Big_int.string_of_big_int
let _gen_interval = I.arbitrary _gen_bigint

(* check that x is member of [x,x] *)
let check_range_mem =
  let gen = _gen_bigint in
  let prop x = I.mem (I.range x x) x in
  let name = "interval_check_range_mem" in
  mk_test ~name ~n:1000 ~pp:_ppbi gen prop

let _imply a b = not a || b

(* check that if points belong to a, then they belong to a union b *)
let check_union_bigger n =
  let gen = Arbitrary.(triple (list _gen_bigint) _gen_interval _gen_interval) in
  let prop (l, a, b) =
    let aUb = I.union a b in
    List.for_all
      (fun x ->
        _imply (I.mem a x || I.mem b x) (I.mem aUb x))
      l
  in
  let name = "interval_union_implies_inclusion" in
  let pp = PP.(triple (list _ppbi) I.to_string I.to_string) in
  mk_test ~name ~pp ~n gen prop

(* check that if points belong to a inter b, then they belong to a and b *)
let check_inter_smaller n =
  let gen = Arbitrary.(triple (list _gen_bigint) _gen_interval _gen_interval) in
  let prop (l, a, b) =
    let aIb = I.inter a b in
    List.for_all
      (fun x ->
        _imply (I.mem aIb x) (I.mem a x && I.mem b x))
      l
  in
  let name = "interval_mem_intersection_implies_mem" in
  let pp = PP.(triple (list _ppbi) I.to_string I.to_string) in
  mk_test ~name ~pp ~n gen prop

let props =
  [ check_range_mem
  ; check_union_bigger 10_000
  ; check_inter_smaller 10_000
  ]

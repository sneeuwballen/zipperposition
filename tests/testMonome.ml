
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

(** {1 Tests for Monome} *)

open Logtk
open Libzipperposition
open QCheck

module M = Monome

(* m1 + m2 == m1 - (- m2) *)
let check_add_diff ar name =
  let gen = Arbitrary.pair ar ar in
  let prop (m1,m2) =
    M.eq (M.sum m1 m2) (M.difference m1 (M.uminus m2))
  in
  let pp = PP.pair M.to_string M.to_string in
  let name = "monome_add_diff_uminus_" ^ name in
  mk_test ~name ~pp gen prop

(* deserialise (serialise m) = m *)
let check_bij =
  let gen = M.arbitrary_int in
  let prop m =
    let bij = M.bij in
    let m = M.normalize m in
    let s = Bij.TrBencode.to_string ~bij m in
    let m' = Bij.TrBencode.of_string ~bij s in
    M.eq m m'
  in
  mk_test ~name:"monome_bij" ~pp:M.to_string gen prop

let check_has_instances_rat =
  let gen = M.arbitrary_rat in
  let prop m = M.has_instances m in
  mk_test ~name:"monome_rat_has_instances" ~pp:M.to_string gen prop

(* m has instances (for int) => m*c has instances too *)
let check_has_instances_int_prod =
  let gen = Arbitrary.(pair M.arbitrary_int (lift Symbol.mk_int (2 -- 5))) in
  let prop (m,c) =
    Prop.assume (M.has_instances m);
    M.has_instances (M.product m c)
  in
  let pp = PP.(pair M.to_string Symbol.to_string) in
  let name = "monome_int_has_instances_kept_by_product" in
  mk_test ~pp ~name ~n:1000 gen prop

let check_has_instances_int_normalize =
  let gen = M.arbitrary_int in
  let prop m =
    (M.has_instances m) = (M.has_instances (M.normalize m))
  in
  let name = "monome_int_has_instances_inv_by_normalization" in
  mk_test ~name ~pp:M.to_string gen prop

let check_normalization_idempotent =
  let gen = M.arbitrary in
  let prop m =
    M.eq (M.normalize m) (M.normalize (M.normalize m))
  in
  let name = "monome_normalization_idempotent" in
  mk_test ~name ~pp:M.to_string gen prop

let props =
  [ check_add_diff M.arbitrary_int "int"
  ; check_add_diff M.arbitrary_int "rat"
  ; check_bij
  ; check_has_instances_rat
  ; check_has_instances_int_prod
  ; check_has_instances_int_normalize
  ; check_normalization_idempotent
  ]

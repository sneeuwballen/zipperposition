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

(** Test terms *)

open Logtk
open Logtk_arbitrary
open OUnit

module T = FOTerm
module H = Helpers
module S = Substs
module HOT = HOTerm

(** unit tests *)

let ty = Type.TPTP.i
let f x y = HOT.at_list (HOT.const ~ty:Type.(ty <== [ty;ty]) (Symbol.of_string "f")) [x; y]
let g x = HOT.at (HOT.const ~ty:Type.(ty <=. ty) (Symbol.of_string"g")) x
let h x y z = HOT.at_list (HOT.const ~ty:Type.(ty <== [ty;ty;ty]) (Symbol.of_string "h")) [x;y;z]
let a = HOT.const ~ty (Symbol.of_string "a")
let b = HOT.const ~ty (Symbol.of_string "b")
let x = HOT.var ~ty 0
let y = HOT.var ~ty 1

let test_db_shift () =
  let t = HOT.lambda [x] (f x (g (HOT.bvar ~ty 0))) in
  let t' = HOT.of_term_exn (ScopedTerm.DB.shift 1 (t:HOT.t:>ScopedTerm.t)) in
  let t1 = HOT.lambda [x] (f x (g (HOT.bvar ~ty 1))) in
  assert_equal ~cmp:HOT.eq ~printer:HOT.to_string t1 t';
  ()

let test_db_unshift () =
  let t = HOT.__mk_lambda ~varty:ty (f (HOT.bvar ~ty 0) (g (HOT.bvar ~ty 2))) in
  let t' = HOT.of_term_exn (ScopedTerm.DB.unshift 1 (t:HOT.t:>ScopedTerm.t)) in
  let t1 = HOT.__mk_lambda ~varty:ty (f (HOT.bvar ~ty 0) (g (HOT.bvar ~ty 1))) in
  assert_equal ~cmp:HOT.eq ~printer:HOT.to_string t1 t';
  ()

let test_beta_reduce () =
  let redex =
    let x' = HOT.var ~ty:Type.(ty <=. ty) 2 in
    HOT.at
      (HOT.lambda [x'] (f (HOT.at x' a) (HOT.at x' b)))
      (HOT.lambda [x] (g x))
  in
  let t' = Lambda.beta_reduce redex in
  let t1 = f (g a) (g b) in
  assert_equal ~cmp:HOT.eq ~printer:HOT.to_string t1 t';
  ()

let suite =
  "test_term" >:::
    [ "test_db_shift" >:: test_db_shift
    ; "test_db_unshift" >:: test_db_unshift
    ; "test_beta_reduce" >:: test_beta_reduce
    ]

(** Properties *)

open QCheck

(* subterm is smaller than term *)
let check_size_subterm =
  (* choose a subterm of t *)
  let gen = Arbitrary.(ArTerm.default >>= fun t ->
    ArTerm.pos t >>= fun pos ->
    return (t, T.Pos.at t pos))
  in
  let pp = PP.(pair T.to_string T.to_string) in
  let prop (t1, t2) =
    T.subterm ~sub:t2 t1 &&
    T.size t1 >= T.size t2
  in
  mk_test ~pp ~name:"term_size_subterm" gen prop

(* replace subterm by itself yields same term *)
let check_replace_id =
  let gen = Arbitrary.(
    ArTerm.default >>= fun t ->
    ArTerm.pos t >>= fun pos ->
    return (t, pos))
  in
  let pp = PP.(pair T.to_string Position.to_string) in
  let prop (t, pos) =
    let sub = T.Pos.at t pos in
    Prop.assume (ScopedTerm.DB.closed (sub : T.t :> ScopedTerm.t));
    let t' = T.Pos.replace t pos ~by:sub in
    T.eq t t'
  in
  mk_test ~pp ~name:"term_replace_same_subterm" gen prop

let check_ground_novar =
  let gen = ArTerm.default in
  let pp = T.to_string in
  let prop t =
    T.is_ground t = Sequence.is_empty (T.Seq.vars t)  (* ground <=> no vars *)
  in
  mk_test ~n:1000 ~pp ~name:"term_ground_has_no_var" gen prop

let check_min_max_vars =
  let gen = ArTerm.default in
  let prop t =
    let vars = T.vars (Sequence.singleton t) in
    T.min_var vars <= T.max_var vars
  in
  mk_test ~n:1000 ~pp:T.to_string ~name:"term_min_max_var" gen prop

(* TODO: write a term arbitrary instance for DB terms (lambda terms?)
   and check that a shifted/unshifted closed term remains closed *)

let props =
  [ check_size_subterm
  ; check_replace_id
  ; check_ground_novar
  ; check_min_max_vars
  ]

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
open OUnit

module T = Term
module H = Helpers
module S = Substs

(** unit tests *)

let pterm s = Parse_tptp.parse_term Lex_tptp.token (Lexing.from_string s);;

let f x y = T.mk_node (Symbol.mk_const "f") [x;y]
let g x = T.mk_node (Symbol.mk_const "g") [x]
let h x y z = T.mk_node (Symbol.mk_const "h") [x;y;z]
let a = T.mk_const (Symbol.mk_const "a")
let b = T.mk_const (Symbol.mk_const "b")
let x = T.mk_var 0
let y = T.mk_var 1

let test_db_lift () =
  let t = T.mk_lambda (f (T.mk_bound_var 0) (g (T.mk_bound_var 1))) in
  let t' = T.db_lift 1 t in
  let t1 = T.mk_lambda (f (T.mk_bound_var 0) (g (T.mk_bound_var 2))) in
  assert_equal ~cmp:T.eq ~printer:T.to_string t1 t';
  ()

let test_db_unlift () =
  let t = T.mk_lambda (f (T.mk_bound_var 0) (g (T.mk_bound_var 2))) in
  let t' = T.db_unlift t in
  let t1 = T.mk_lambda (f (T.mk_bound_var 0) (g (T.mk_bound_var 1))) in
  assert_equal ~cmp:T.eq ~printer:T.to_string t1 t';
  ()

let redex = T.mk_at
  (T.mk_lambda_var [x] (f (T.mk_at x a) (T.mk_at x b)))
  (T.mk_at
    (T.mk_lambda_var [y;x] (T.mk_at y x))
    (T.mk_lambda_var [x] (g x))
  )

let test_beta_reduce () =
  let t' = HO.beta_reduce redex in
  let t1 = pterm "f(g(a), g(b))" in
  assert_equal ~cmp:T.eq ~printer:T.to_string t1 t';
  ()

let suite =
  "test_term" >:::
    [ "test_db_lift" >:: test_db_lift
    ; "test_db_unlift" >:: test_db_unlift
    ; "test_beta_reduce" >:: test_beta_reduce
    ]

(** Properties *)

open QCheck

(* subterm is smaller than term *)
let check_size_subterm =
  (* choose a subterm of t *)
  let gen = Arbitrary.(T.arbitrary >>= fun t ->
    T.arbitrary_pos t >>= fun pos ->
    return (t, T.at_pos t pos))
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
    T.arbitrary >>= fun t ->
    T.arbitrary_pos t >>= fun pos ->
    return (t, pos))
  in
  let pp = PP.(pair T.to_string Position.to_string) in
  let prop (t, pos) =
    let sub = T.at_pos t pos in
    Prop.assume (T.db_closed sub);
    let t' = T.replace_pos t pos sub in
    T.eq t t'
  in
  mk_test ~pp ~name:"term_replace_same_subterm" gen prop
    
let check_ground_novar =
  let gen = T.arbitrary in
  let pp = T.to_string in
  let prop t =
    T.is_ground t = (T.vars t = [])  (* ground <=> no vars *)
  in
  mk_test ~n:1000 ~pp ~name:"term_ground_has_no_var" gen prop

let check_min_max_vars =
  let gen = T.arbitrary in
  let prop t =
    let vars = T.vars t in
    T.min_var vars <= T.max_var vars
  in
  mk_test ~n:1000 ~pp:T.to_string ~name:"term_min_max_var" gen prop

(* TODO: write a term arbitrary instance for DB terms (lambda terms?)
   and check that a lifted/unlifted closed term remains closed *)

let props =
  [ check_size_subterm
  ; check_replace_id
  ; check_ground_novar
  ; check_min_max_vars
  ]

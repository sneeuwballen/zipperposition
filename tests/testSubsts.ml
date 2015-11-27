
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

open Logtk
open OUnit

module T = FOTerm
module S = Substs

let ty = Type.TPTP.i
let __const ?(ty=ty) s = T.const ~ty (Symbol.of_string s)

let a = __const "a"
let b = __const "b"
let x = T.var ~ty 1
let y = T.var ~ty 2
let f x y = T.app (__const ~ty:Type.(TPTP.i <== [TPTP.i;TPTP.i]) "f") [x; y]
let g x = T.app (__const ~ty:Type.(TPTP.i <=. TPTP.i) "g") [x]
let h x y z = T.app (__const ~ty:Type.(TPTP.i <== [TPTP.i;TPTP.i;TPTP.i]) "h") [x;y;z]
let nil = __const ~ty:Type.(forall [var 0] (app (Symbol.of_string "list") [var 0])) "nil"

let test_rename () =
  let t1 = f x (g y) in
  let t2 = f x (g a) in
  let t3 = g (g x) in
  let subst = Unif.FO.unification t1 1 t2 0 in
  let renaming = S.Renaming.create () in
  let t1' = S.FO.apply ~renaming subst t1 1 in
  let t2' = S.FO.apply ~renaming subst t2 0 in
  let t3' = h (S.FO.apply ~renaming subst y 1) t1' (S.FO.apply ~renaming subst t3 0) in
  assert_bool "must be equal" (T.equal t1' t2');
  let t3'' = h a (f x (g a)) (g (g x)) in
  assert_equal ~cmp:Unif.FO.are_variant ~printer:T.to_string t3'' t3';
  ()

let test_unify () =
  let v = Type.var 0 in
  let f ty x y = T.app_full
    (__const ~ty:Type.(forall [v] (v <== [v;v])) "f") [ty] [x; y] in
  let g ty x = T.app_full
    (__const ~ty:Type.(forall [v] (v <=. v)) "g") [ty] [x] in
  let nil ty = T.tyapp
    (__const ~ty:Type.(forall [v] (app (Symbol.of_string "list") [v])) "nil") ty in
  let x = T.var ~ty:Type.(app (Symbol.of_string "list") [var 3]) 0 in
  let y = T.var ~ty:Type.(app (Symbol.of_string "list") [TPTP.int]) 1 in
  let t1 = let ty = Type.(app (Symbol.of_string "list") [var 3]) in f ty x (g ty x) in
  let t2 = let ty = Type.(app (Symbol.of_string "list") [TPTP.int]) in
    f ty (nil Type.TPTP.int) (g ty y) in
  let subst = Unif.FO.unification t1 0 t2 1 in
  let renaming = S.Renaming.create () in
  let t1' = S.FO.apply subst ~renaming t1 0 in
  let t2' = S.FO.apply subst ~renaming t2 1 in
  (*
  T.print_var_types := true;
  Util.printf "t1: %a, t2: %a, subst: %a\n" T.pp t1 T.pp t2 S.pp subst;
  *)
  assert_equal ~cmp:T.equal ~printer:T.to_string t1' t2';
  ()

let suite =
  "test_substs" >:::
    [ "test_rename" >:: test_rename
    ; "test_unify" >:: test_unify
    ]

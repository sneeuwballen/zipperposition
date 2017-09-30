
(* This file is free software, part of Zipperposition. See file "license" for more details. *)

open Logtk
open OUnit

module T = Term
module S = Subst

let ty = Type.TPTP.i
let __const ?(ty=ty) s = T.const ~ty s

let f_ = ID.make "f"
let g_ = ID.make "g"
let h_ = ID.make "h"
let list_ = ID.make "list"
let nil_ = ID.make "nil"

let a = __const (ID.make "a")
let b = __const (ID.make "b")
let x = T.var_of_int ~ty 1
let y = T.var_of_int ~ty 2
let f x y = T.app (__const ~ty:Type.([term;term] ==> term) f_) [x; y]
let g x = T.app (__const ~ty:Type.([term] ==> term) g_) [x]
let h x y z = T.app (__const ~ty:Type.([term;term;term] ==> term) h_) [x;y;z]
let nil = __const ~ty:Type.(forall (app list_ [bvar 0])) nil_

let test_rename _ =
  let t1 = f x (g y) in
  let t2 = f x (g a) in
  let t3 = g (g x) in
  let subst = Unif.FO.unify_syn (t1,1) (t2,0) in
  let renaming = S.Renaming.create () in
  let t1' = S.FO.apply renaming subst (t1,1) in
  let t2' = S.FO.apply renaming subst (t2,0) in
  let t3' = h (S.FO.apply renaming subst (y,1)) t1' (S.FO.apply renaming subst (t3,0)) in
  assert_bool "must be equal" (T.equal t1' t2');
  let t3'' = h a (f x (g a)) (g (g x)) in
  assert_equal ~cmp:Unif.FO.are_variant ~printer:T.to_string t3'' t3';
  ()

let test_unify _ =
  let f ty x y =
    T.app
      (__const ~ty:Type.(forall ([bvar 0; bvar 0] ==> bvar 0)) f_)
      [T.of_ty ty; x; y]
  in
  let g ty x =
    T.app
      (__const ~ty:Type.(forall ([bvar 0] ==> bvar 0)) g_)
      [T.of_ty ty; x] in
  let nil ty =
    T.app
      (__const ~ty:Type.(forall (app list_ [bvar 0])) nil_)
      [T.of_ty ty] in
  let x = T.var_of_int ~ty:Type.(app list_ [var_of_int 3]) 0 in
  let y = T.var_of_int ~ty:Type.(app list_ [int]) 1 in
  let t1 =
    let ty = Type.(app list_ [var_of_int 3]) in
    f ty x (g ty x) in
  let t2 =
    let ty = Type.(app list_ [int]) in
    f ty (nil Type.TPTP.int) (g ty y) in
  let subst = Unif.FO.unify_syn (t1,0) (t2,1) in
  let renaming = S.Renaming.create () in
  let t1' = S.FO.apply renaming subst (t1,0) in
  let t2' = S.FO.apply renaming subst (t2,1) in
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

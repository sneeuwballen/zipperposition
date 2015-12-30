
(* This file is free software, part of Zipperposition. See file "license" for more details. *)

(** Test terms *)

open Libzipperposition
open Libzipperposition_arbitrary
open OUnit

module T = FOTerm
module H = Helpers
module S = Substs
module HOT = HOTerm

(* unit tests *)

let f_ = ID.make "f"
let g_ = ID.make "g"
let h_ = ID.make "h"

let ty = Type.term
let f x y = HOT.app (HOT.const ~ty:Type.([ty;ty] ==> ty) f_) [x; y]
let g_fun = HOT.const ~ty:Type.([ty] ==> ty) g_
let g x = HOT.app g_fun [x]
let h x y z = HOT.app (HOT.const ~ty:Type.([ty;ty;ty] ==> ty) h_) [x;y;z]
let a = HOT.const ~ty (ID.make "a")
let b = HOT.const ~ty (ID.make "b")
let x = HOT.var_of_int ~ty 0
let y = HOT.var_of_int ~ty 1

let test_db_shift () =
  let t = HOT.lambda ~varty:ty (f (HOT.bvar ~ty 0) (g (HOT.bvar ~ty 1))) in
  let t' = HOT.of_term_unsafe (InnerTerm.DB.shift 1 (t:HOT.t:>InnerTerm.t)) in
  let t1 = HOT.lambda ~varty:ty (f (HOT.bvar ~ty 0) (g (HOT.bvar ~ty 2))) in
  assert_equal ~cmp:HOT.equal ~printer:HOT.to_string t1 t';
  ()

let test_db_unshift () =
  let t = HOT.lambda ~varty:ty (f (HOT.bvar ~ty 0) (g (HOT.bvar ~ty 2))) in
  let t' = HOT.of_term_unsafe (InnerTerm.DB.unshift 1 (t:HOT.t:>InnerTerm.t)) in
  let t1 = HOT.lambda ~varty:ty (f (HOT.bvar ~ty 0) (g (HOT.bvar ~ty 1))) in
  assert_equal ~cmp:HOT.equal ~printer:HOT.to_string t1 t';
  ()

let test_whnf1 () =
  (* eta expansion of [g] *)
  let g_eta = HOT.lambda ~varty:ty (g (HOT.bvar ~ty 0)) in
  let redex =
    let ty2 = Type.([ty] ==> ty) in
    HOT.app
      (HOT.lambda ~varty:ty2
         (f
           (HOT.app (HOT.bvar ~ty:ty2 0) [a])
           (HOT.app (HOT.bvar ~ty:ty2 0) [b])))
      [g_eta]
  in
  let t' = Lambda.whnf redex in
  (* WHNF: does not reduce in subterms *)
  let t1 = f (HOT.app g_eta [a]) (HOT.app g_eta [b]) in
  assert_equal ~cmp:HOT.equal ~printer:HOT.to_string t1 t';
  ()

let test_whnf2 () =
  let redex =
    let ty2 = Type.([ty] ==> ty) in
    HOT.app
      (HOT.lambda ~varty:ty2
         (f
           (HOT.app (HOT.bvar ~ty:ty2 0) [a])
           (HOT.app (HOT.bvar ~ty:ty2 0) [b])))
      [g_fun]
  in
  let t' = Lambda.whnf redex in
  let t1 = f (g a) (g b) in
  assert_equal ~cmp:HOT.equal ~printer:HOT.to_string t1 t';
  ()

let suite =
  "test_term" >:::
    [ "test_db_shift" >:: test_db_shift
    ; "test_db_unshift" >:: test_db_unshift
    ; "test_whnf1" >:: test_whnf1
    ; "test_whnf2" >:: test_whnf2
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
    Prop.assume (InnerTerm.DB.closed (sub : T.t :> InnerTerm.t));
    let t' = T.Pos.replace t pos ~by:sub in
    T.equal t t'
  in
  mk_test ~pp ~name:"term_replace_same_subterm" gen prop

let check_ground_novar =
  let gen = ArTerm.default in
  let pp = T.to_string in
  let prop t =
    not (T.is_ground t) || Sequence.is_empty (T.Seq.vars t)  (* ground => no vars *)
  in
  mk_test ~n:1000 ~pp ~name:"term_ground_has_no_var" gen prop

let check_min_max_vars =
  let gen = ArTerm.default in
  let prop t =
    let vars = T.vars (Sequence.singleton t) in
    T.VarSet.is_empty vars || (T.min_var vars <= T.max_var vars)
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

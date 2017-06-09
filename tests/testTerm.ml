
(* This file is free software, part of Zipperposition. See file "license" for more details. *)

(** Test terms *)

open Logtk
open Logtk_arbitrary
open OUnit

module T = Term
module H = Helpers
module S = Substs

let (==>) = QCheck.(==>)

(* unit tests *)

let f_ = ID.make "f"
let g_ = ID.make "g"
let h_ = ID.make "h"

let ty = Type.term
let f x y = T.app (T.const ~ty:Type.([ty;ty] ==> ty) f_) [x; y]
let g_fun = T.const ~ty:Type.([ty] ==> ty) g_
let g x = T.app g_fun [x]
let h x y z = T.app (T.const ~ty:Type.([ty;ty;ty] ==> ty) h_) [x;y;z]
let a = T.const ~ty (ID.make "a")
let b = T.const ~ty (ID.make "b")
let x = T.var_of_int ~ty 0
let y = T.var_of_int ~ty 1

let test_db_shift () =
  let t = T.fun_ ty (f (T.bvar ~ty 0) (g (T.bvar ~ty 1))) in
  let t' = T.of_term_unsafe (InnerTerm.DB.shift 1 (t:T.t:>InnerTerm.t)) in
  let t1 = T.fun_ ty (f (T.bvar ~ty 0) (g (T.bvar ~ty 2))) in
  assert_equal ~cmp:T.equal ~printer:T.to_string t1 t';
  ()

let test_db_unshift () =
  let t = T.fun_ ty (f (T.bvar ~ty 0) (g (T.bvar ~ty 2))) in
  let t' = T.of_term_unsafe (InnerTerm.DB.unshift 1 (t:T.t:>InnerTerm.t)) in
  let t1 = T.fun_ ty (f (T.bvar ~ty 0) (g (T.bvar ~ty 1))) in
  assert_equal ~cmp:T.equal ~printer:T.to_string t1 t';
  ()

let test_whnf1 () =
  (* eta expansion of [g] *)
  let g_eta = T.fun_ ty (g (T.bvar ~ty 0)) in
  let redex =
    let ty2 = Type.([ty] ==> ty) in
    T.app
      (T.fun_ ty2
         (f
           (T.app (T.bvar ~ty:ty2 0) [a])
           (T.app (T.bvar ~ty:ty2 0) [b])))
      [g_eta]
  in
  let t' = Lambda.whnf redex in
  (* WHNF: does not reduce in subterms *)
  let t1 = f (T.app g_eta [a]) (T.app g_eta [b]) in
  assert_equal ~cmp:T.equal ~printer:T.to_string t1 t';
  ()

let test_whnf2 () =
  let redex =
    let ty2 = Type.([ty] ==> ty) in
    T.app
      (T.fun_ ty2
         (f
           (T.app (T.bvar ~ty:ty2 0) [a])
           (T.app (T.bvar ~ty:ty2 0) [b])))
      [g_fun]
  in
  let t' = Lambda.whnf redex in
  let t1 = f (g a) (g b) in
  assert_equal ~cmp:T.equal ~printer:T.to_string t1 t';
  ()

let suite =
  "test_term" >:::
    [ "test_db_shift" >:: test_db_shift
    ; "test_db_unshift" >:: test_db_unshift
    ; "test_whnf1" >:: test_whnf1
    ; "test_whnf2" >:: test_whnf2
    ]

(** Properties *)

(* subterm is smaller than term *)
let check_size_subterm =
  (* choose a subterm of t *)
  let gen = QCheck.Gen.(
    ArTerm.default_g >>= fun t ->
    ArTerm.pos t >>= fun pos ->
    return (t, T.Pos.at t pos))
  in
  let pp = QCheck.Print.(pair T.to_string T.to_string) in
  let gen = QCheck.make ~print:pp gen in
  let prop (t1, t2) =
    T.subterm ~sub:t2 t1 &&
    T.size t1 >= T.size t2
  in
  QCheck.Test.make ~name:"term_size_subterm" gen prop

(* replace subterm by itself yields same term *)
let check_replace_id =
  let gen = QCheck.Gen.(
    ArTerm.default_g >>= fun t ->
    ArTerm.pos t >>= fun pos ->
    return (t, pos))
  in
  let pp = QCheck.Print.(pair T.to_string Position.to_string) in
  let gen = QCheck.make ~print:pp gen in
  let prop (t, pos) =
    let sub = T.Pos.at t pos in
    InnerTerm.DB.closed (sub : T.t :> InnerTerm.t)
    ==>
      let t' = T.Pos.replace t pos ~by:sub in
      T.equal t t'
  in
  QCheck.Test.make ~name:"term_replace_same_subterm" gen prop

let check_ground_novar =
  let gen = ArTerm.default in
  let prop t =
    not (T.is_ground t) || Sequence.is_empty (T.Seq.vars t)  (* ground => no vars *)
  in
  QCheck.Test.make ~count:1000 ~name:"term_ground_has_no_var" gen prop

let check_min_max_vars =
  let gen = ArTerm.default in
  let prop t =
    let vars = T.vars t in
    T.VarSet.is_empty vars || (T.min_var vars <= T.max_var vars)
  in
  QCheck.Test.make ~count:1000 ~name:"term_min_max_var" gen prop

let check_hash_mod_alpha =
  let gen = QCheck.pair ArTerm.default ArTerm.default in
  let prop (t1,t2) =
    if not (T.equal t1 t2) && Unif.FO.are_variant t1 t2
    then
      T.hash_mod_alpha t1 = T.hash_mod_alpha t2
    else QCheck.assume_fail ()
  in
  QCheck.Test.make gen prop
    ~count:10_000 ~long_factor:20
    ~name:"variant_have_same_hash_mod_alpha"

(* TODO: write a term arbitrary instance for DB terms (lambda terms?)
   and check that a shifted/unshifted closed term remains closed *)

let is_fun t = match T.view t with T.Fun _ -> true | _ -> false

let num_lam t =
  T.Seq.subterms t
  |> Sequence.filter is_fun
  |> Sequence.length

let num_var_app t =
  T.Seq.subterms t
  |> Sequence.filter T.is_ho_app
  |> Sequence.length

(* NOTE: this enables stats *)
let add_stat = ref false

let gen_ho =
  let a = {ArTerm.default_ho with QCheck.gen=ArTerm.default_ho_fuel 10} in
  if !add_stat then (
    a
    |> QCheck.add_stat ("lambdas",num_lam)
    |> QCheck.add_stat ("var_app",num_var_app)
    |> QCheck.add_stat ("size", T.size)
  ) else a

let check_whnf =
  let gen = gen_ho in
  let prop t =
    if not (T.DB.is_closed t) then QCheck.assume_fail()
    else (
      let t' = Lambda.whnf t in
      T.DB.is_closed t'
    )
  in
  QCheck.Test.make gen prop
    ~count:10_000 ~long_factor:10
    ~name:"whnf_preserve_closed"

let check_snf =
  let gen = gen_ho in
  let prop t =
    if not (T.DB.is_closed t) then QCheck.assume_fail()
    else(
      let t' = Lambda.snf t in
      T.DB.is_closed t'
    )
  in
  QCheck.Test.make gen prop
    ~count:10_000 ~long_factor:10
    ~name:"snf_preserve_closed"

let check_snf_no_redex =
  let gen = gen_ho in
  let prop t =
    if not (T.DB.is_closed t) then QCheck.assume_fail()
    else (
      let t' = Lambda.snf t in
      T.Seq.subterms t'
      |> Sequence.for_all
        (fun t -> match T.view t with
           | T.App (f, _) -> not (is_fun f)
           | _ -> true)
    )
  in
  QCheck.Test.make gen prop
    ~count:10_000 ~long_factor:10
    ~name:"snf_no_remaining_redex"

let props =
  [ check_size_subterm
  ; check_replace_id
  ; check_ground_novar
  ; check_hash_mod_alpha
  ; check_min_max_vars
  ; check_whnf
  ; check_snf
  ; check_snf_no_redex
  ]

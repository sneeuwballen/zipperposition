
(* This file is free software, part of Zipperposition. See file "license" for more details. *)

(** {1 Arbitrary Typed Terms and Formulas} *)

open Logtk

module QA = QCheck
module T = Term
module Sym = Symbol
module HOT = HOTerm

type 'a arbitrary = 'a QCheck.arbitrary
type 'a gen = 'a QCheck.Gen.t

module PT = struct
  module PT = TypedSTerm
  let _const ~ty s = PT.const ~ty (ID.make s)

  (* strict subterms *)
  let shrink t = TypedSTerm.Seq.subterms t |> Sequence.drop 1

  let mk_ gen =
    QA.make ~print:TypedSTerm.to_string ~shrink gen

  let ty_term = PT.Ty.term
  let ty_fun1 = PT.Ty.([term] ==> term)
  let ty_fun2 = PT.Ty.([term; term] ==> term)
  let ty_fun3 = PT.Ty.([term; term; term] ==> term)

  let a = _const ~ty:ty_term "a"
  let b = _const ~ty:ty_term "b"
  let c = _const ~ty:ty_term "c"
  let d = _const ~ty:ty_term "d"
  let e = _const ~ty:ty_term "e"
  let f x y = PT.app ~ty:ty_term (_const ~ty:ty_fun2 "f") [x; y]
  let sum x y = PT.app ~ty:ty_term (_const ~ty:ty_fun2 "sum") [x; y]
  let g x = PT.app ~ty:ty_term (_const ~ty:ty_fun1 "g") [x]
  let h x = PT.app ~ty:ty_term (_const ~ty:ty_fun1 "h") [x]
  let ite x y z = PT.app ~ty:ty_term (_const ~ty:ty_fun3 "ite") [x; y; z]

  let ground_g =
    let open QA.Gen in
    let base = oneofl [a; b; c; d; e; ] in
    let t =
      fix
        (fun self n ->
           let self = self (n-1) in
           if n<=0 then base
           else frequency
               [ 1, map2 f self self
               ; 1, map g self
               ; 1, map  h self
               ; 1, oneof [map2 sum self self; map3 ite self self self]
               ; 3, base
               ])
    in
    (1 -- 4) >>= t

  let ground = mk_ ground_g

  let default_fuel n =
    let x = PT.var (Var.of_string ~ty:ty_term "X") in
    let y = PT.var (Var.of_string ~ty:ty_term "Y") in
    let z = PT.var (Var.of_string ~ty:ty_term "Z") in
    let open QA.Gen in
    let base = oneofl [a;b;c;d;e;x;y;z] in
    let gen =
      fix
        (fun self n ->
           let self = self (n-1) in
           if n<=0 then base
           else frequency
               [ 3, base
               ; 1, map2 f self self
               ; 1, map2 sum self self
               ; 1, map g self
               ; 1, map h self
               ; 1, map3 ite self self self
               ])
    in
    QA.Gen.((1 -- n) >>= gen)

  let default_g = QA.Gen.(1 -- 4 >>= default_fuel)
  let default = mk_ default_g

  let ty_prop = PT.Ty.prop
  let ty_pred1 = PT.Ty.([term] ==> prop)
  let ty_pred2 = PT.Ty.([term; term] ==> prop)

  let p x y = PT.app ~ty:ty_prop (_const ~ty:ty_pred2 "p") [x; y]
  let q x = PT.app ~ty:ty_prop (_const ~ty:ty_pred1 "q") [x]
  let r x = PT.app ~ty:ty_prop (_const ~ty:ty_pred1 "r") [x]
  let s = PT.const ~ty:ty_prop (ID.make "s")

  let pred_g =
    let sub = default_g in
    let open  QA.Gen in
    oneof
      [ map2 p sub sub
      ; map q sub
      ; map r sub
      ; return s
      ]

  let pred = mk_ pred_g
end

let shrink t = T.Seq.subterms t |> Sequence.drop 1

let mk_ gen = QA.make ~print:T.to_string ~shrink gen

let ctx = Term.Conv.create()

let default_g = QCheck.Gen.map (Term.Conv.of_simple_term_exn ctx) PT.default_g
let default = mk_ default_g

let default_fuel f =
  QA.Gen.map (Term.Conv.of_simple_term_exn ctx) (PT.default_fuel f)

let ground_g = QCheck.Gen.map (Term.Conv.of_simple_term_exn ctx) PT.ground_g
let ground = mk_ ground_g

let pred =
  QA.map (Term.Conv.of_simple_term_exn ctx) PT.pred

let pos t =
  let module PB = Position.Build in
  let open QA.Gen in
  let rec recurse t pb st =
    let stop = return (PB.to_pos pb) in
    match T.view t with
      | T.App (_, [])
      | T.Const _
      | T.Var _
      | T.DB _ -> PB.to_pos pb
      | T.AppBuiltin (_, l)
      | T.App (_, l) ->
        oneof (stop :: List.mapi (fun i t' -> recurse t' (PB.arg i pb)) l) st
  in
  recurse t PB.empty

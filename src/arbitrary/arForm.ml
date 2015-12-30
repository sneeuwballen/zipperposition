
(* This file is free software, part of Zipperposition. See file "license" for more details. *)

(** {1 Arbitrary Basic Terms} *)

open Libzipperposition

module QA = QCheck.Arbitrary
module PT = TypedSTerm
module F = PT.Form
module AT = ArTerm.PT

type 'a arbitrary = 'a QA.t
type form = PT.t

let atom =
  QA.(
    choose
      [ AT.pred
      ; lift F.not_ AT.pred
      ; lift2 F.eq AT.default AT.default
      ; lift2 F.neq AT.default AT.default
      ; among [ F.true_; F.false_ ]
      ])

let map1_ f self =
  QA.(
    self 1 >|= function
    | [x] -> f x | _ -> assert false
  )

let map2_ f self =
  QA.(
    self 2 >|= function
    | [x;y] -> f x y | _ -> assert false
  )

let default_fuel fuel =
  QA.(
    let f = fix_fuel
        [ `Rec (fun self -> (small_int >>= self) >|= F.or_)
        ; `Rec (fun self -> (small_int >>= self) >|= F.and_)
        ; `Rec (fun self -> map2_ F.equiv self)
        ; `Rec (fun self -> map2_ F.imply self)
        ; `Rec (fun self -> map1_ F.not_ self)
        ; `Rec (fun self -> map1_ F.close_forall self)
        ; `Rec (fun self -> map1_ F.close_forall self)
        ]
    in
    retry (f fuel))

let clause =
  QA.(
    int 3 >>= fun len ->
    list_repeat len atom
  )

let default = QA.(int 50 >>= default_fuel)

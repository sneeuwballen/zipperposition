
(* This file is free software, part of Zipperposition. See file "license" for more details. *)

(** {1 Arbitrary Basic Terms} *)

open Libzipperposition

module QA = QCheck
module PT = TypedSTerm
module F = PT.Form
module AT = ArTerm.PT

type 'a arbitrary = 'a QA.arbitrary
type 'a gen = 'a QCheck.Gen.t
type form = PT.t

let mk_ gen =
  QA.make ~print:PT.to_string ~shrink:PT.Seq.subterms gen

let atom_g =
  let open QCheck.Gen in
  oneof
    [ AT.pred_g
    ; map F.not_ AT.pred_g
    ; map2 F.eq AT.default_g AT.default_g
    ; map2 F.neq AT.default_g AT.default_g
    ; oneofl [ F.true_; F.false_ ]
    ]

let atom = mk_ atom_g

let default_fuel n =
  let open QCheck.Gen in
  fix
    (fun self n ->
       let self = self (n-1) in
       if n=0 then atom_g
       else oneof
         [ list_size (1--10) self >|= F.or_
         ; list_size (1--10) self >|= F.and_
         ; map2 F.equiv self self
         ; map2 F.imply self self
         ; map F.not_ self
         ; map F.close_forall self
         ; map F.close_forall self
         ; atom_g
         ])
    n

let default_g = default_fuel 50
let default = mk_ default_g

let clause_g =
  let open QCheck.Gen in
  list_size (0 -- 4) atom_g

let clause =
  QA.make ~shrink:QA.Shrink.(list ~shrink:PT.Seq.subterms) clause_g

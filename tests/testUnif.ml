
(* This file is free software, part of Zipperposition. See file "license" for more details. *)

(** Test unification *)

open Libzipperposition
open Libzipperposition_arbitrary

module T = FOTerm
module S = Substs

let (==>) = QCheck.(==>)

let check_unify_gives_unifier =
  let gen = QCheck.(pair ArTerm.default ArTerm.default) in
  let name = "unify_gives_unifier" in
  let prop (t1, t2) =
    try
      let subst = Unif.FO.unification (t1,0) (t2,1) in
      let renaming = S.Renaming.create () in
      let t1' = S.FO.apply ~renaming subst (t1,0) in
      let t2' = S.FO.apply ~renaming subst (t2,1) in
      T.equal t1' t2'
    with Unif.Fail ->
      false ==> true
  in
  QCheck.Test.make ~count:1000 ~name gen prop

let check_variant =
  let gen = ArTerm.default in
  let name = "unif_term_self_variant" in
  let prop t =
    let renaming = S.Renaming.create () in
    let t' = S.FO.apply ~renaming S.empty (t,0) in
    Unif.FO.are_variant t t'
  in
  QCheck.Test.make ~name gen prop

let check_matching =
  let gen = QCheck.pair ArTerm.default ArTerm.default in
  let name = "unif_matching_gives_matcher" in
  let prop (t1, t2) =
    try
      let subst = Unif.FO.matching ~pattern:(t1,0) (t2,1) in
      let renaming = S.Renaming.create () in
      let t1' = S.FO.apply ~renaming subst (t1,0) in
      let t2' = S.FO.apply ~renaming subst (t2,1) in
      T.equal t1' t2' && Unif.FO.are_variant t2 t2'
    with Unif.Fail ->
      false ==> true
  in
  QCheck.Test.make ~count:1000 ~name gen prop

let props =
  [ check_unify_gives_unifier
  ; check_variant
  ; check_matching
  ]

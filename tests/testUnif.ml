
(* This file is free software, part of Zipperposition. See file "license" for more details. *)

(** Test unification *)

open Logtk
open Logtk_arbitrary

module T = Term
module S = Subst
module Q = QCheck

let (==>) = QCheck.(==>)

let check_unify_gives_unifier =
  let gen = QCheck.(pair ArTerm.default ArTerm.default) in
  let name = "unify_gives_unifier" in
  let prop (t1, t2) =
    try
      let subst = Unif.FO.unify_syn (t1,0) (t2,1) in
      let renaming = S.Renaming.create () in
      let t1' = S.FO.apply ~renaming subst (t1,0) in
      let t2' = S.FO.apply ~renaming subst (t2,1) in
      T.equal t1' t2'
    with Unif.Fail ->
      false ==> true
  in
  QCheck.Test.make ~long_factor:20 ~count:1000 ~name gen prop

let check_variant =
  let gen = ArTerm.default in
  let name = "unif_term_self_variant" in
  let prop t =
    let renaming = S.Renaming.create () in
    let t' = S.FO.apply ~renaming S.empty (t,0) in
    Unif.FO.are_variant t t'
  in
  QCheck.Test.make ~long_factor:20 ~name gen prop

let check_variant2 =
  let gen = ArTerm.default in
  let name = "unif_term_variant_sound" in
  let prop (t0,t1) =
    try
      let subst = Unif.FO.variant (t0,0)(t1,1) in
      (* check they are really variants *)
      let renaming = Subst.Renaming.create() in
      let t0' = Subst.FO.apply ~renaming subst (t0,0) in
      let t1' = Subst.FO.apply ~renaming subst (t1,1) in
      T.equal t0' t1'
    with Unif.Fail -> QCheck.assume_fail ()
  in
  QCheck.Test.make ~long_factor:20 ~name (Q.pair gen gen) prop

let check_variant_sym =
  let gen = ArTerm.default in
  let name = "unif_term_variant_sym" in
  let prop (t0,t1) =
    Unif.FO.are_variant t0 t1 = Unif.FO.are_variant t1 t0
  in
  QCheck.Test.make ~long_factor:20 ~name (Q.pair gen gen) prop

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
  QCheck.Test.make ~long_factor:20 ~count:1000 ~name gen prop

let check_variant_bidir_match =
  let gen = ArTerm.default in
  let name = "unif_term_variant_bidir_match" in
  let prop (t0,t1) =
    if Unif.FO.are_variant t0 t1
    then Unif.FO.matches ~pattern:t0 t1 && Unif.FO.matches ~pattern:t1 t0
    else QCheck.assume_fail()
  in
  QCheck.Test.make ~long_factor:20 ~name (Q.pair gen gen) prop

let check_lits_variant_bidir_match =
  let gen = ArLiteral.clause in
  let name = "unif_lits_variant_bidir_match" in
  let prop (lits0,lits1) =
    if Literals.are_variant lits0 lits1
    then Literals.matches lits0 lits1 && Literals.matches lits1 lits0
    else QCheck.assume_fail()
  in
  QCheck.Test.make ~long_factor:20 ~count:5_000 ~name (Q.pair gen gen) prop

(* TODO: generate random Literals.t, then check [variant a b <=> (matches a b && matches b a)] *)

let props =
  [ check_unify_gives_unifier;
    check_variant;
    check_variant2;
    check_variant_sym;
    check_variant_bidir_match;
    check_lits_variant_bidir_match;
    check_matching;
  ]

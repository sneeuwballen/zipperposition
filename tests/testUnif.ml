
(* This file is free software, part of Zipperposition. See file "license" for more details. *)

(** Test unification *)

open Logtk
open Logtk_arbitrary

module T = Term
module S = Subst
module Q = QCheck

let (==>) = QCheck.(==>)

let gen_fo = ArTerm.default
let gen_t = ArTerm.default_ho

let check_unify_gives_unifier =
  let gen = QCheck.(pair gen_t gen_t) in
  let name = "unify_gives_unifier" in
  let prop (t1, t2) =
    try
      let subst = Unif.FO.unify_syn (t1,0) (t2,1) in
      let renaming = S.Renaming.create () in
      let t1' = S.FO.apply ~renaming subst (t1,0) |> Lambda.snf in
      let t2' = S.FO.apply ~renaming subst (t2,1) |> Lambda.snf in
      if T.equal t1' t2' then true
      else QCheck.Test.fail_reportf
          "subst=%a,@ t1'=`%a`,@ t2'=`%a`" Subst.pp subst T.pp t1' T.pp t2'
    with Unif.Fail -> QCheck.assume_fail()
  in
  QCheck.Test.make ~long_factor:20 ~count:15_000 ~name gen prop

let check_unify_makes_eq  =
  let gen = QCheck.(pair gen_t gen_t) in
  let name = "unify_makes_eq" in
  let prop (t1, t2) =
    try
      let subst = Unif.FO.unify_syn (t1,0) (t2,1) in
      if Unif.FO.equal subst (t1,0) (t2,1) then true
      else QCheck.Test.fail_reportf
          "subst=%a,@ t1=`%a`,@ t2=`%a`" Subst.pp subst T.pp t1 T.pp t2
    with Unif.Fail -> QCheck.assume_fail()
  in
  QCheck.Test.make ~long_factor:20 ~count:15_000 ~name gen prop

let check_variant =
  let gen = gen_fo in
  let name = "unif_term_self_variant" in
  let prop t =
    let renaming = S.Renaming.create () in
    let t' = S.FO.apply ~renaming S.empty (t,0) in
    Unif.FO.are_variant t t'
  in
  QCheck.Test.make ~long_factor:20 ~name gen prop

let check_variant2 =
  let gen = gen_t in
  let name = "unif_term_variant_sound" in
  let prop (t0,t1) =
    try
      let subst = Unif.FO.variant (t0,0)(t1,1) in
      (* check they are really variants *)
      let renaming = Subst.Renaming.create() in
      let t0' = Subst.FO.apply ~renaming subst (t0,0) |> Lambda.snf in
      let t1' = Subst.FO.apply ~renaming subst (t1,1) |> Lambda.snf in
      T.equal t0' t1'
    with Unif.Fail -> QCheck.assume_fail ()
  in
  QCheck.Test.make ~long_factor:20 ~name (Q.pair gen gen) prop

let check_variant_sym =
  let gen = gen_t in
  let name = "unif_term_variant_sym" in
  let prop (t0,t1) =
    Unif.FO.are_variant t0 t1 = Unif.FO.are_variant t1 t0
  in
  QCheck.Test.make ~long_factor:20 ~name (Q.pair gen gen) prop

let check_matching =
  let gen = QCheck.pair gen_t gen_t in
  let name = "unif_matching_gives_matcher" in
  let prop (t1, t2) =
    try
      let subst = Unif.FO.matching ~pattern:(t1,0) (t2,1) in
      let renaming = S.Renaming.create () in
      let t1' = S.FO.apply ~renaming subst (t1,0) |> Lambda.snf in
      let t2' = S.FO.apply ~renaming subst (t2,1) |> Lambda.snf in
      if T.equal t1' t2'
      then true
      else QCheck.Test.fail_reportf "@[<v>subst=%a,@ t1'=`%a`,@ t2'=`%a`@]"
          Subst.pp subst T.pp t1' T.pp t2'
    with Unif.Fail -> QCheck.assume_fail()
  in
  QCheck.Test.make ~long_factor:20 ~count:1000 ~name gen prop

let check_matching_variant =
  let gen = QCheck.pair gen_t gen_fo in
  let name = "unif_matching_preserves_rhs" in
  let prop (t1, t2) =
    try
      let subst = Unif.FO.matching ~pattern:(t1,0) (t2,1) in
      let renaming = S.Renaming.create () in
      let t2' = S.FO.apply ~renaming subst (t2,1) |> Lambda.snf in
      if Unif.FO.are_variant t2 t2'
      then true
      else QCheck.Test.fail_reportf "@[<v>subst=%a,@ t2'=`%a`@]"
          Subst.pp subst T.pp t2'
    with Unif.Fail -> QCheck.assume_fail()
  in
  QCheck.Test.make ~long_factor:20 ~count:1000 ~name gen prop

let check_variant_bidir_match =
  let gen = gen_t in
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

let check_ho_unify_gives_unifiers =
  let gen = QCheck.(pair gen_t gen_t) in
  let name = "ho_unify_gives_unifiers" in
  let prop (t1, t2) =
    let offset =
      Sequence.doubleton t1 t2
      |> Sequence.flat_map T.Seq.vars
      |> T.Seq.max_var |> succ
    in
    (* only keep proper solutions *)
    let l =
      HO_unif.unif_pairs ~fuel:20 ~offset ([t1,t2],0)
      |> List.filter
        (fun (pairs,us,_) -> pairs=[] && not (Unif_subst.has_constr us))
    in
    if l=[] then QCheck.assume_fail()
    else (
      List.iter
        (fun (_,us,_) ->
           let subst = Unif_subst.subst us in
           let renaming = Subst.Renaming.create() in
           let u1 = Subst.FO.apply ~renaming subst (t1,0) |> Lambda.snf in
           let u2 = Subst.FO.apply ~renaming subst (t2,0) |> Lambda.snf in
           if not (T.equal u1 u2) then (
             QCheck.Test.fail_reportf
               "(@[<hv2>bad solution@ t1'=`%a`@ t2'=`%a`@ :subst %a@])"
               T.pp u1 T.pp u2 Subst.pp subst
           ))
        l;
      true
    )
  in
  QCheck.Test.make ~long_factor:20 ~count:8_000 ~name gen prop

(* TODO: generate random Literals.t, then check [variant a b <=> (matches a b && matches b a)] *)

let props =
  [ check_unify_gives_unifier;
    check_unify_makes_eq;
    check_variant;
    check_variant2;
    check_variant_sym;
    check_variant_bidir_match;
    check_lits_variant_bidir_match;
    check_matching;
    check_matching_variant;
    check_ho_unify_gives_unifiers;
  ]

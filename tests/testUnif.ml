
(* This file is free software, part of Zipperposition. See file "license" for more details. *)

(** Test unification *)

open OUnit
open Logtk
open Logtk_arbitrary

module Fmt = CCFormat
module T = Term
module S = Subst
module Q = QCheck

(** {2 Unit Tests} *)

let psterm, pstmt, pstmt_l, clear_scope, unif_ty =
  let tyctx = TypeInference.Ctx.create ~implicit_ty_args:true () in
  let pt s =
    let t = Parse_zf.parse_term Lex_zf.token (Lexing.from_string s) in
    let t = TypeInference.infer_exn tyctx t in
    (* TypeInference.Ctx.exit_scope tyctx; *)
    t
  and pst s =
    let t = Parse_zf.parse_statement Lex_zf.token (Lexing.from_string s) in
    let t = TypeInference.infer_statement_exn tyctx t in
    (* TypeInference.Ctx.exit_scope tyctx; *)
    t
  and pst_l s =
    let l = Parse_zf.parse_statement_list Lex_zf.token (Lexing.from_string s) in
    let l = TypeInference.infer_statements_exn
        ~on_var:`Default ~ctx:tyctx ~implicit_ty_args:true
        (Sequence.of_list l) in
    (* TypeInference.Ctx.exit_scope tyctx; *)
    CCVector.to_list l
  and unif_ty t u =
    TypedSTerm.unify (TypedSTerm.ty_exn t) (TypedSTerm.ty_exn u)
  in
  pt, pst, pst_l, (fun () -> TypeInference.Ctx.exit_scope tyctx), unif_ty

(* prelude *)
let () =
  ignore (pstmt_l
    "val term : type.
     val a : term.
     val b : term.
     val c : term.
     val d : term.
     val e : term.
     val f : term -> term -> term.
     val g : term -> term.
     val h : term -> term.
     val ite : term -> term -> term -> term.
     val p : term -> term -> prop.
     val q : term -> prop.
     val r : term -> prop.
     val s : prop.
     val f_ho2: (term -> term ) -> (term -> term) -> term.
     val p_ho2: (term -> term ) -> (term -> term) -> prop.
   ")

let tyctx = T.Conv.create()

(* parse Term.t *)
let pterm =
  fun ?ty s ->
    let t = psterm s in
    let ty = CCOpt.map psterm ty in
    CCOpt.iter (fun ty -> TypedSTerm.unify ty (TypedSTerm.ty_exn t)) ty;
    T.Conv.of_simple_term_exn tyctx t

(* parse two terms of same type *)
let pterm2 =
  fun ?ty s1 s2 ->
    let t1 = psterm s1 in
    let t2 = psterm s2 in
    unif_ty t1 t2;
    let ty = CCOpt.map psterm ty in
    CCOpt.iter (fun ty -> TypedSTerm.unify ty (TypedSTerm.ty_exn t1)) ty;
    T.Conv.of_simple_term_exn tyctx t1,
    T.Conv.of_simple_term_exn tyctx t2

let ppair = function
  | `With_ty (ty, `Unif (t,u)) -> pterm2 ~ty t u
  | `Unif (t,u) -> pterm2 t u

let check_variant t u =
  if Unif.FO.are_variant t u then ()
  else (
    let msg =
      CCFormat.sprintf "@[<2>`%a`@ and `%a`@ should be variant@]@."
        T.ZF.pp t T.ZF.pp u
    in
    OUnit.assert_failure msg
  )

let check_matches t u =
  if Unif.FO.matches ~pattern:t u then ()
  else (
    let msg =
      CCFormat.sprintf "@[<2>`%a`@ should match@ `%a`@]@."
        T.ZF.pp t T.ZF.pp u
    in
    OUnit.assert_failure msg
  )

let check_eq t1 t2 =
  OUnit.assert_equal ~printer:T.ZF.to_string ~cmp:T.equal t1 t2

let unifier2 t u =
  try
    let subst = Unif.FO.unify_syn (t,0)(u,0) in
    let renaming = Subst.Renaming.create() in
    Subst.FO.apply renaming subst (t,0) |> Lambda.snf,
    Subst.FO.apply renaming subst (u,0) |> Lambda.snf,
    renaming,
    subst
  with Unif.Fail ->
    let msg = CCFormat.sprintf "@[<2>`%a`@ and `%a`@ should be unifiable@]@."
        T.ZF.pp t T.ZF.pp u in
    OUnit.assert_failure msg

let unifier t u =
  let t', u', _, _ = unifier2 t u in
  OUnit.assert_equal ~printer:T.ZF.to_string ~cmp:T.equal t' u';
  t'

let check_unifiable t u =
  let name = Fmt.sprintf "(@[unifiable `%a`@ `%a`@])" T.ZF.pp t T.ZF.pp u in
  name >:: fun () ->
    let _ = unifier2 t u in
    ()

let check_unify_correct t u =
  let name = Fmt.sprintf "(@[unify_correct `%a`@ `%a`@])" T.ZF.pp t T.ZF.pp u in
  name >:: fun () ->
    let t', u', _, _ = unifier2 t u in
    check_eq t' u'

let check_unifier t u ~res =
  let name = Fmt.sprintf "(@[unify `%a`@ `%a`@ :gives `%a`@])" T.ZF.pp t T.ZF.pp u T.ZF.pp res in
  name >:: fun () ->
    let t' = unifier t u in
    check_variant t' res

let check_unifier_matches t u =
  let name = Fmt.sprintf "(@[unify_matches `%a`@ `%a`@])" T.ZF.pp t T.ZF.pp u in
  name >:: fun () ->
    let t' = unifier t u in
    check_matches t t';
    check_matches u t'

let check_same t u t1 t2 =
  let name = Fmt.sprintf "(@[unify `%a`@ `%a`@ :makes-eq `%a`@ `%a`@])"
      T.ZF.pp t T.ZF.pp u T.ZF.pp t1 T.ZF.pp t2 in
  name >:: fun () ->
    let _, _, renaming, subst = unifier2 t u in
    let t1 = Subst.FO.apply renaming subst (t1,0) |> Lambda.snf in
    let t2 = Subst.FO.apply renaming subst (t2,0) |> Lambda.snf in
    check_eq t1 t2

(* parse action *)
let paction = function
  | `With_ty (ty, `Yield  r) -> `Yield (pterm ~ty r)
  | `Yield r -> `Yield (pterm r)
  | `Eq (s1,s2) ->
    let t1, t2 = pterm2 s1 s2 in
    `Eq (t1,t2)
  | `With_ty (ty, `Eq (s1,s2)) ->
    let t1, t2 = pterm2 ~ty s1 s2 in
    `Eq (t1,t2)

let check_action t u a = match a with
  | `Yield res -> check_unifier t u ~res
  | `Eq (t1,t2) -> check_same t u t1 t2

let suite_unif1 : OUnit.test list =
  let (=?=) a b = `Unif (a,b) in (* unif pair *)
  let (>->) a b = `With_ty (b, a) in (* specify return type *)
  let mk_tests (pair,actions) =
    let t, u = ppair pair in
    let actions = List.map paction actions in
    clear_scope();
    check_unifiable t u ::
      check_unify_correct t u ::
      check_unifier_matches t u ::
      List.map (check_action t u) actions
  in
  CCList.flat_map mk_tests
    [ "f X b" =?= "f a Y", [
          `Yield "f a b";
          `Eq ("X", "a");
          `Eq ("Y", "b");
        ];
      "F a" =?= "f a (g (g a))", [
        `Yield "f a (g (g a))";
        `Eq ("F", "fun (x:term). f x (g (g x))");
      ];
      ("fun (x y:term). F x" =?= "fun x y. G x y") >-> "term -> term -> term", [
        `Yield "fun x y. H x" >-> "term -> term -> term";
        `Eq ("G", "fun x y. F x") >-> "term -> term -> term";
      ];
      ("fun (x y z:term). F x" =?= "fun x y z. G x y z") >-> "term -> term -> term -> term", [
        `Yield "fun x y z. H x" >-> "term -> term -> term -> term";
        `Eq ("G", "fun x y z. F x") >-> "term -> term -> term -> term";
      ];
      ("X" =?= "(fun Y. X1) (fun (x y:term). c)") >-> "term", [
        `Yield "Y" >-> "term";
      ];
      ("p_ho2 (fun a. F1 a) (fun a. F2 a)" =?= "p_ho2 (fun a. G a) (fun a. G a)"), [
        `Yield "p_ho2 (fun a. G a) (fun a. G a)";
        `Eq ("F1", "G") >-> "term -> term";
        `Eq ("F2", "G") >-> "term -> term";
      ];
      ("p_ho2 (fun Y0. d) (fun Y0. F1 Y0)" =?=
       "p_ho2 (fun Y0. d) (fun Y0. (f_ho2 (fun Y1. Y1) (fun Y2. X)))"), [
      ];
    ]

let reg_matching1 () =
  let t1, t2 =
    pterm "p_ho2 (fun a. F a) (fun a. F a)",
    pterm "p_ho2 (fun a. G a) (fun a. H a)"
  in
  try
    let _ = Unif.FO.matching ~pattern:(t1,0) (t2,1) in
    OUnit.assert_failure
      (Fmt.sprintf "@[<hv>`%a`@ and `%a@ should not match@]" T.ZF.pp t1 T.ZF.pp t2)
  with Unif.Fail -> ()

let suite_unif2 : _ list = [
  "reg_matching1" >:: reg_matching1;
]

let suite =
  "unif" >:::
    (List.flatten
       [ suite_unif1;
         suite_unif2;
       ])

(** {2 Properties} *)

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
      let t1' = S.FO.apply renaming subst (t1,0) |> Lambda.snf in
      let t2' = S.FO.apply renaming subst (t2,1) |> Lambda.snf in
      if T.equal t1' t2' then true
      else QCheck.Test.fail_reportf
          "subst=%a,@ t1'=`%a`,@ t2'=`%a`" Subst.pp subst T.ZF.pp t1' T.ZF.pp t2'
    with Unif.Fail -> QCheck.assume_fail()
  in
  QCheck.Test.make ~long_factor:20 ~count:15_000 ~name gen prop

(* σ=mgu(t1,t2) means t1,t2 both match t1σ (== t2σ). In practice, with
   our matching, it only works for FO terms *)
let check_unifier_matches =
  let gen = QCheck.(pair gen_fo gen_fo) in
  let name = "unifier_matches_unified_terms" in
  let prop (t1, t2) =
    try
      let subst = Unif.FO.unify_syn (t1,0) (t2,1) in
      let renaming = S.Renaming.create () in
      let t1' = S.FO.apply renaming subst (t1,0) |> Lambda.snf in
      let t2' = S.FO.apply renaming subst (t2,1) |> Lambda.snf in
      if Unif.FO.matches ~pattern:t1 t1' &&
         Unif.FO.matches ~pattern:t2 t2'
      then true
      else ( QCheck.Test.fail_reportf
          "(@[<hv2>subst=%a,@ t1'=`%a`,@ t2'=`%a`@])"
          Subst.pp subst T.ZF.pp t1' T.ZF.pp t2'
      )
    with Unif.Fail -> QCheck.assume_fail()
  in
  QCheck.Test.make ~long_factor:20 ~count:15_000 ~name gen prop

let check_unify_makes_eq  =
  let gen = QCheck.(pair gen_t gen_t) in
  let name = "unify_makes_eq" in
  let prop (t1, t2) =
    try
      let subst = Unif.FO.unify_syn (t1,0) (t2,1) in
      if Unif.FO.equal ~subst (t1,0) (t2,1) then true
      else QCheck.Test.fail_reportf
          "subst=%a,@ t1=`%a`,@ t2=`%a`" Subst.pp subst T.ZF.pp t1 T.ZF.pp t2
    with Unif.Fail -> QCheck.assume_fail()
  in
  QCheck.Test.make ~long_factor:20 ~count:15_000 ~name gen prop

let check_equal =
  let gen = gen_t in
  let name = "unif_term_self_equal" in
  let prop t =
    Unif.FO.equal ~subst:Subst.empty (t,0) (t,0)
  in
  QCheck.Test.make ~long_factor:20 ~count:2_000 ~name gen prop

let check_variant =
  let gen = gen_fo in
  let name = "unif_term_self_variant" in
  let prop t =
    let renaming = S.Renaming.create () in
    let t' = S.FO.apply renaming S.empty (t,0) in
    Unif.FO.are_variant t t'
  in
  QCheck.Test.make ~long_factor:20 ~count:2_000 ~name gen prop

let check_variant2 =
  let gen = gen_t in
  let name = "unif_term_variant_sound" in
  let prop (t0,t1) =
    try
      let subst = Unif.FO.variant (t0,0)(t1,1) in
      (* check they are really variants *)
      let renaming = Subst.Renaming.create() in
      let t0' = Subst.FO.apply renaming subst (t0,0) |> Lambda.snf in
      let t1' = Subst.FO.apply renaming subst (t1,1) |> Lambda.snf in
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
      let t1' = S.FO.apply renaming subst (t1,0) |> Lambda.snf in
      let t2' = S.FO.apply renaming subst (t2,1) |> Lambda.snf in
      if T.equal t1' t2'
      then true
      else QCheck.Test.fail_reportf "@[<v>subst=%a,@ t1'=`%a`,@ t2'=`%a`@]"
          Subst.pp subst T.ZF.pp t1' T.ZF.pp t2'
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
      let t2' = S.FO.apply renaming subst (t2,1) |> Lambda.snf in
      if Unif.FO.are_variant t2 t2'
      then true
      else QCheck.Test.fail_reportf "@[<v>subst=%a,@ t2'=`%a`@]"
          Subst.pp subst T.ZF.pp t2'
    with Unif.Fail -> QCheck.assume_fail()
  in
  QCheck.Test.make ~long_factor:20 ~count:1000 ~name gen prop

let check_matching_variant_same_scope =
  let gen = QCheck.pair gen_t gen_fo in
  let name = "unif_matching_same_scope_preserves_rhs" in
  let prop (t1, t2) =
    try
      let subst = Unif.FO.matching_same_scope ~scope:0 ~pattern:t1 t2 in
      let t2' = S.FO.apply Subst.Renaming.none subst (t2,0) |> Lambda.snf in
      if Unif.FO.are_variant t2 t2'
      then true
      else QCheck.Test.fail_reportf "@[<v>subst=%a,@ t2'=`%a`@]"
          Subst.pp subst T.ZF.pp t2'
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
      HO_unif.unif_pairs ~fuel:20 ~offset ([[],t1,t2],0)
      |> List.filter
        (fun (pairs,us,_) -> pairs=[] && not (Unif_subst.has_constr us))
    in
    if l=[] then QCheck.assume_fail()
    else (
      List.iter
        (fun (_,us,_) ->
           let subst = Unif_subst.subst us in
           let renaming = Subst.Renaming.create() in
           let u1 = Subst.FO.apply renaming subst (t1,0) |> Lambda.snf in
           let u2 = Subst.FO.apply renaming subst (t2,0) |> Lambda.snf in
           if not (T.equal u1 u2) then (
             QCheck.Test.fail_reportf
               "(@[<hv2>bad solution@ t1'=`%a`@ t2'=`%a`@ :subst %a@])"
               T.ZF.pp u1 T.ZF.pp u2 Subst.pp subst
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
    check_unifier_matches;
    check_equal;
    check_variant;
    check_variant2;
    check_variant_sym;
    check_variant_bidir_match;
    check_lits_variant_bidir_match;
    check_matching;
    check_matching_variant;
    check_matching_variant_same_scope;
    check_ho_unify_gives_unifiers;
  ]


(* This file is free software, part of Zipperposition. See file "license" for more details. *)

(** test orderings *)

open Logtk
open Logtk_arbitrary

module T = Term
module S = Subst.FO
module O = Ordering

let comp_test = Alcotest.testable Comparison.pp Comparison.equal

let count = 1_000
let long_factor = 10

let a_ = ID.make "a"
let b_ = ID.make "b"
let c_ = ID.make "c"
let f_ = ID.make "f"
let g_ = ID.make "g"
let h_ = ID.make "h"

let ty = Type.term

(* [more_specific cm1 cm2] is true if [cmp2] is compatible with, and possibly
    more accurate, than [cmp1]. For instance, Incomparable/Gt is ok, but
    not Lt/Eq *)
let more_specific cmp1 cmp2 = Comparison.(match cmp1, cmp2 with
  | Lt, Lt
  | Gt, Gt
  | Eq, Eq
  | Incomparable, _ -> true
  | _, _ -> false
  )

let gen_t = ArTerm.default_ho_g
let arb_t = ArTerm.default_ho

let check_ordering_inv_by_subst ord =
  let name = CCFormat.sprintf "ordering_%s_inv_by_subst" (O.name ord) in
  let pp = QCheck.Print.triple T.to_string T.to_string Subst.to_string in
  (* generate pairs of terms, and grounding substitutions *)
  let gen = QCheck.Gen.(
    (pair gen_t gen_t)
    >>= fun (t1, t2) ->
    let vars = Iter.of_list [t1; t2]
      |> Iter.flat_map T.Seq.vars
      |> T.VarSet.of_seq
    in
    (* grounding substitution *)
    let subst st = T.VarSet.fold
      (fun v subst ->
        let v = (v : Type.t HVar.t :> InnerTerm.t HVar.t) in
        S.bind subst (v,1) (ArTerm.ground_g st,0))
      vars Subst.empty in
    triple (return t1) (return t2) subst)
  in
  let size (t1, t2, s) =
    T.size t1 + T.size t2 +
      (Subst.fold (fun n _ (t,_) -> n + T.size (T.of_term_unsafe t)) 0 s)
  in
  let gen = QCheck.make ~print:pp ~small:size gen in
  (* do type inference on the fly
  let tyctx = TypeInference.Ctx.create () in
  let signature = ref Signature.empty in
  *)
  let ord = ref ord in
  let prop (t1, t2, subst) =
    (* declare symbols *)
    Iter.of_list [t1;t2]
      |> Iter.flat_map T.Seq.symbols
      |> ID.Set.of_seq |> ID.Set.to_seq
      |> O.add_seq !ord;
    let t1' = S.apply Subst.Renaming.none subst (t1,0) in
    let t2' = S.apply Subst.Renaming.none subst (t2,0) in
    (* check that instantiating variables preserves ordering *)
    let o1 = O.compare !ord t1 t2 in
    let o2 = O.compare !ord t1' t2' in
    more_specific o1 o2
  in
  QCheck.Test.make ~count ~long_factor ~name gen prop

let check_ordering_trans ord =
  let name = CCFormat.sprintf "ordering_%s_transitive" (O.name ord) in
  let arb = QCheck.triple arb_t arb_t arb_t in
  let ord = ref ord in
  let prop (t1, t2, t3) =
    (* declare symbols *)
    Iter.of_list [t1;t2;t3]
      |> Iter.flat_map T.Seq.symbols
      |> ID.Set.of_seq |> ID.Set.to_seq
      |> O.add_seq !ord;
    (* check that instantiating variables preserves ordering *)
    let o12 = O.compare !ord t1 t2 in
    let o23 = O.compare !ord t2 t3 in
    if o12 = Comparison.Lt && o23 = Comparison.Lt
    then
      let o13 = O.compare !ord t1 t3 in
      o13 = Comparison.Lt
    else QCheck.assume_fail ()
  in
  QCheck.Test.make ~count ~long_factor ~name arb prop

let check_ordering_swap_args ord =
  let name = CCFormat.sprintf "ordering_%s_swap_args" (O.name ord) in
  let arb = QCheck.pair arb_t arb_t in
  let ord = ref ord in
  let prop (t1, t2) =
    (* declare symbols *)
    Iter.of_list [t1;t2]
      |> Iter.flat_map T.Seq.symbols
      |> ID.Set.of_seq |> ID.Set.to_seq
      |> O.add_seq !ord;
    (* check that instantiating variables preserves ordering *)
    let o12 = O.compare !ord t1 t2 in
    let o21 = O.compare !ord t2 t1 in
    let open Comparison in
    begin match o12, o21 with
      | Lt, Gt
      | Gt, Lt
      | Eq, Eq
      | Incomparable, Incomparable -> true
      | _ -> false
    end
  in
  QCheck.Test.make ~count ~long_factor ~name arb prop

let contains_ho t =
  T.Seq.subterms t
  |> Iter.exists
    (fun t -> T.is_ho_at_root t || T.is_fun t)

let check_ordering_subterm ord =
  let name = CCFormat.sprintf "ordering_%s_subterm_property" (O.name ord) in
  let arb = arb_t in
  let ord = ref ord in
  let prop t =
    if contains_ho t then QCheck.assume_fail() else
    (* declare symbols *)
    Iter.of_list [t]
      |> Iter.flat_map T.Seq.symbols
      |> ID.Set.of_seq |> ID.Set.to_seq
      |> O.add_seq !ord;
    T.Seq.subterms_depth t
    |> Iter.filter_map (fun (t,i) -> if i>0 then Some t else None)
    |> Iter.for_all
      (fun sub ->
         O.compare !ord t sub = Comparison.Gt)
  in
  QCheck.Test.make ~count ~long_factor ~name arb prop

let test_rpo6 = "ordering.rpo", `Quick, fun () ->
  let ord = O.rpo6 (Precedence.default [a_; b_; c_; f_; g_; h_]) in
  let compare = O.compare ord in

  (* x a < x b *)
  let a = Term.const ~ty a_ in
  let b = Term.const ~ty b_ in
  let x = Term.var (HVar.fresh ~ty:(Type.arrow [ty] ty) ()) in
  Alcotest.(check comp_test) "x a < x b" (compare (Term.app x [a]) (Term.app x [b])) Comparison.Lt ;

  (* f x y is incomparable with  x y *)
  let f = Term.const ~ty:(Type.arrow [(Type.arrow [ty] ty); ty] ty) f_ in
  let x = Term.var (HVar.fresh ~ty:(Type.arrow [ty] ty) ()) in
  let y = Term.var (HVar.fresh ~ty ()) in
  Alcotest.(check comp_test) "f x y <> x y" (compare (Term.app f [x;y]) (Term.app x [y])) Comparison.Incomparable;

  (* g x > f x x *)
  let f = Term.const ~ty:(Type.arrow [ty; ty] ty) f_ in
  let g = Term.const ~ty:(Type.arrow [ty] ty) g_ in
  let x = Term.var (HVar.fresh ~ty ()) in
  Alcotest.(check comp_test) "g x > f x x" (compare (Term.app g [x]) (Term.app f [x; x])) Comparison.Gt;

  (* f (x b) > x a *)
  let f = Term.const ~ty:(Type.arrow [ty] ty) f_ in
  let x = Term.var (HVar.fresh ~ty:(Type.arrow [ty] ty)  ()) in
  let a = Term.const ~ty a_ in
  let b = Term.const ~ty b_ in
  Alcotest.(check comp_test) "f (x b) > x a" (compare (Term.app f [Term.app x [b]]) (Term.app x [a])) Comparison.Gt;

  (* f a a > f b  ( test for length-lexicographic extension ) *)
  let f = Term.const ~ty:(Type.arrow [ty; ty] ty) f_ in
  let a = Term.const ~ty a_ in
  let b = Term.const ~ty b_ in
  Alcotest.(check comp_test) "f a a > f b" (compare (Term.app f [a;a]) (Term.app f [b])) Comparison.Gt


let test_kbo = "ordering.kbo", `Quick, fun () ->
  (* alphabetical precedence, h has weight 2, all other symbols weight 1*)
  let weight id = (if id=h_ then Precedence.Weight.add Precedence.Weight.one Precedence.Weight.one else Precedence.Weight.one) in
  let ord = O.kbo (Precedence.create ~weight Precedence.Constr.alpha [a_; b_; c_; f_; g_; h_]) in
  let compare = O.compare ord in

  (* h (x y) > f y (x a) *)
  let f = Term.const ~ty:(Type.arrow [ty; ty] ty) f_ in
  let h = Term.const ~ty:(Type.arrow [ty] ty) h_ in
  let a = Term.const ~ty a_ in
  let x = Term.var (HVar.fresh ~ty:(Type.arrow [ty] ty) ()) in
  let y = Term.var (HVar.fresh ~ty ()) in
  Alcotest.(check comp_test) "h (x y) > f y (x a)"
    (compare (Term.app h [Term.app x [y]]) (Term.app f [y; Term.app x [a]])) Comparison.Gt;

  (* polymorphic example *)
  let funty_ = (ID.make "funty") in
  let appty = Type.forall_n 2 (Type.arrow [Type.app funty_ [Type.bvar 1; Type.bvar 0]; Type.bvar 1] (Type.bvar 0)) in
  let app_ = ID.make "app" in
  let app = T.const ~ty:appty app_ in
  let add_ = ID.make "add" in
  let add = T.const ~ty:(Type.app funty_ [ty;Type.app funty_ [ty;ty]]) add_ in
  let s_ = ID.make "s" in
  let s = T.const ~ty:(Type.app funty_ [ty;ty]) s_ in
  let k_ = ID.make "k" in
  let k = T.const ~ty k_ in
  let zero_ = ID.make "zero" in
  let zero = T.const ~ty zero_ in
  let ty1 = Term.of_ty ty in
  let ty2 = Term.of_ty (Type.app funty_ [ty; ty]) in
  let ord = O.kbo (Precedence.create ~weight Precedence.Constr.alpha [add_; app_; funty_; k_; s_; zero_]) in
  let x = Term.var (HVar.fresh ~ty ()) in
  let y = Term.var (HVar.fresh ~ty ()) in
  let compare = O.compare ord in
  (*app (app add (app s zero)) k > app (app add zero)(app s k)*)
  Alcotest.(check comp_test)
    "… > …"
    (compare
                  (T.app app [ty1; ty1; T.app app [ty1; ty2; add; (T.app app [ty1; ty1; s; zero])]; k])
                  (T.app app [ty1; ty1; T.app app [ty1; ty2; add; zero]; T.app app [ty1; ty1; s; k]])
               ) Comparison.Gt;
  (*app (app add (app s x)) y > app (app add x)(app s y)*)
  Alcotest.(check comp_test) 
    "… > …"
   (compare
                   (T.app app [ty1; ty1; T.app app [ty1; ty2; add; (T.app app [ty1; ty1; s; x])]; y])
                   (T.app app [ty1; ty1; T.app app [ty1; ty2; add; x]; T.app app [ty1; ty1; s; y]])
                ) Comparison.Gt


let suite =
  [ test_rpo6;
    test_kbo
  ]

let props =
  CCList.flat_map
    (fun o ->
       [ check_ordering_inv_by_subst o;
         check_ordering_trans o;
         check_ordering_swap_args o;
         check_ordering_subterm o;
       ])
    [
      O.kbo (Precedence.default []);
      O.lfhokbo_arg_coeff (Precedence.default []);
      O.rpo6 (Precedence.default []);
    ]

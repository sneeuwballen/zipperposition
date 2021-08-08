
(* This file is free software, part of Zipperposition. See file "license" for more details. *)

(** test orderings *)

open Logtk
open Logtk_arbitrary

module T = Term
module S = Subst.FO
module O = Ordering

let comp_test = Alcotest.testable Comparison.Nonstrict.pp Comparison.Nonstrict.equal

let count = 1_000
let long_factor = 10

let a_ = ID.make "a"
let b_ = ID.make "b"
let c_ = ID.make "c"
let d_ = ID.make "d"
let e_ = ID.make "e"
let f_ = ID.make "f"
let g_ = ID.make "g"
let h_ = ID.make "h"

let ty = Type.term

(* [less_specific cm1 cm2] is true if [cmp2] is compatible with, and possibly
    more accurate, than [cmp1]. For instance, Incomparable/Gt is OK, but
    not Lt/Eq *)
let less_specific cmp1 cmp2 = Comparison.Nonstrict.(match cmp1, cmp2 with
  | Lt, Lt
  | Gt, Gt
  | Eq, Eq
  | Leq, Leq
  | Geq, Geq
  | Leq, Lt
  | Leq, Eq
  | Geq, Gt
  | Geq, Eq
  | Incomparable, _ -> true
  | _, _ -> false
  )

let check_ordering_inv_by_subst ~gen_t ord =
  let name = CCFormat.sprintf "ordering_%s_inv_by_subst" (O.name ord) in
  let pp = QCheck.Print.triple T.to_string T.to_string Subst.to_string in
  (* generate pairs of terms, and grounding substitutions *)
  let gen = QCheck.Gen.(
    (pair gen_t gen_t)
    >>= fun (t1, t2) ->
    let vars = Iter.of_list [t1; t2]
      |> Iter.flat_map T.Seq.vars
      |> T.VarSet.of_iter
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
  *)
  let signature = ref Signature.empty in
  let ord = ref ord in
  let prop (t1, t2, subst) =
    (* declare symbols *)
    Iter.of_list [t1;t2]
      |> Iter.flat_map T.Seq.symbols
      |> ID.Set.of_iter |> ID.Set.to_list
      |> O.add_list ~signature:!signature !ord;
    let t1' = S.apply Subst.Renaming.none subst (t1,0) in
    let t2' = S.apply Subst.Renaming.none subst (t2,0) in
    (* check that instantiating variables preserves ordering *)
    let o1 = O.compare !ord t1 t2 in
    let o2 = O.compare !ord t1' t2' in
    less_specific o1 o2
  in
  QCheck.Test.make ~count ~long_factor ~name gen prop

let check_ordering_trans ~arb_t ord =
  let name = CCFormat.sprintf "ordering_%s_transitive" (O.name ord) in
  let arb = QCheck.triple arb_t arb_t arb_t in
  let ord = ref ord in
  let signature = ref Signature.empty in
  let prop (t1, t2, t3) =
    (* declare symbols *)
    Iter.of_list [t1;t2;t3]
      |> Iter.flat_map T.Seq.symbols
      |> ID.Set.of_iter |> ID.Set.to_list
      |> O.add_list ~signature:!signature !ord;
    (* check that instantiating variables preserves ordering *)
    let o12 = O.compare !ord t1 t2 in
    let o23 = O.compare !ord t2 t3 in
    if o12 = Comparison.Nonstrict.Lt && o23 = Comparison.Nonstrict.Lt
    then
      let o13 = O.compare !ord t1 t3 in
      o13 = Comparison.Nonstrict.Lt
    else QCheck.assume_fail ()
  in
  QCheck.Test.make ~count ~long_factor ~name arb prop

let check_ordering_swap_args ~arb_t ord =
  let name = CCFormat.sprintf "ordering_%s_swap_args" (O.name ord) in
  let arb = QCheck.pair arb_t arb_t in
  let ord = ref ord in
  let signature = ref Signature.empty in
  let prop (t1, t2) =
    (* declare symbols *)
    Iter.of_list [t1;t2]
      |> Iter.flat_map T.Seq.symbols
      |> ID.Set.of_iter |> ID.Set.to_list
      |> O.add_list ~signature:!signature !ord;
    (* check that instantiating variables preserves ordering *)
    let o12 = O.compare !ord t1 t2 in
    let o21 = O.compare !ord t2 t1 in
    let open Comparison in
    begin match o12, o21 with
      | Lt, Gt
      | Gt, Lt
      | Leq, Geq
      | Geq, Leq
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

let check_ordering_subterm ~arb_t ord =
  let name = CCFormat.sprintf "ordering_%s_subterm_property" (O.name ord) in
  let arb = arb_t in
  let ord = ref ord in
  let signature = ref Signature.empty in
  let prop t =
    if contains_ho t then QCheck.assume_fail() else
    (* declare symbols *)
    Iter.of_list [t]
      |> Iter.flat_map T.Seq.symbols
      |> ID.Set.of_iter |> ID.Set.to_list
      |> O.add_list ~signature:!signature !ord;
    T.Seq.subterms_depth t
    |> Iter.filter_map (fun (t,i) -> if i>0 then Some t else None)
    |> Iter.for_all
      (fun sub ->
         O.compare !ord t sub = Comparison.Nonstrict.Gt)
  in
  QCheck.Test.make ~count ~long_factor ~name arb prop

let test_derived_ho_rpo = "ordering.derived_ho_rpo", `Quick, fun () ->
  let ord = O.derived_ho_rpo (Precedence.default [a_; b_; c_; d_; e_; f_; g_; h_]) in
  let compare = O.compare ord in

  (* x a <=>? x b *)
  let a = Term.const ~ty a_ in
  let b = Term.const ~ty b_ in
  let x = Term.var (HVar.fresh ~ty:(Type.arrow [ty] ty) ()) in
  Alcotest.(check comp_test) "x a <=>? x b" Comparison.Nonstrict.Incomparable (compare (Term.app x [a]) (Term.app x [b]));

  (* f x y is incomparable with  x y *)
  let f = Term.const ~ty:(Type.arrow [(Type.arrow [ty] ty); ty] ty) f_ in
  let x = Term.var (HVar.fresh ~ty:(Type.arrow [ty] ty) ()) in
  let y = Term.var (HVar.fresh ~ty ()) in
  Alcotest.(check comp_test) "f x y <=>? x y" Comparison.Nonstrict.Incomparable (compare (Term.app f [x;y]) (Term.app x [y]));

  (* g x > f x x *)
  let f = Term.const ~ty:(Type.arrow [ty; ty] ty) f_ in
  let g = Term.const ~ty:(Type.arrow [ty] ty) g_ in
  let x = Term.var (HVar.fresh ~ty ()) in
  Alcotest.(check comp_test) "g x > f x x" Comparison.Nonstrict.Gt (compare (Term.app g [x]) (Term.app f [x; x]));

  (* f (x b) <=>? x a *)
  let f = Term.const ~ty:(Type.arrow [ty] ty) f_ in
  let x = Term.var (HVar.fresh ~ty:(Type.arrow [ty] ty)  ()) in
  let a = Term.const ~ty a_ in
  let b = Term.const ~ty b_ in
  Alcotest.(check comp_test) "f (x b) <=>? x a" Comparison.Nonstrict.Incomparable (compare (Term.app f [Term.app x [b]]) (Term.app x [a]));

  (* f (x a) > x a *)
  let f = Term.const ~ty:(Type.arrow [ty] ty) f_ in
  let x = Term.var (HVar.fresh ~ty:(Type.arrow [ty] ty)  ()) in
  let a = Term.const ~ty a_ in
  Alcotest.(check comp_test) "f (x a) > x a" Comparison.Nonstrict.Gt (compare (Term.app f [Term.app x [a]]) (Term.app x [a]));

  (* f a a > f b  ( test for length-lexicographic extension ) *)
  let f = Term.const ~ty:(Type.arrow [ty; ty] ty) f_ in
  let a = Term.const ~ty a_ in
  let b = Term.const ~ty b_ in
  Alcotest.(check comp_test) "f a a > f b" Comparison.Nonstrict.Gt (compare (Term.app f [a;a]) (Term.app f [b]))

let test_lambdafree_rpo = "ordering.lambdafree_rpo", `Quick, fun () ->
  let ord = O.lambdafree_rpo (Precedence.default [a_; b_; c_; d_; e_; f_; g_; h_]) in
  let compare = O.compare ord in

  (* x a < x b *)
  let a = Term.const ~ty a_ in
  let b = Term.const ~ty b_ in
  let x = Term.var (HVar.fresh ~ty:(Type.arrow [ty] ty) ()) in
  Alcotest.(check comp_test) "x a < x b" Comparison.Nonstrict.Lt (compare (Term.app x [a]) (Term.app x [b]));

  (* f x y is incomparable with  x y *)
  let f = Term.const ~ty:(Type.arrow [(Type.arrow [ty] ty); ty] ty) f_ in
  let x = Term.var (HVar.fresh ~ty:(Type.arrow [ty] ty) ()) in
  let y = Term.var (HVar.fresh ~ty ()) in
  Alcotest.(check comp_test) "f x y is incomparable with  x y" Comparison.Nonstrict.Incomparable (compare (Term.app f [x;y]) (Term.app x [y]));

  (* g x > f x x *)
  let f = Term.const ~ty:(Type.arrow [ty; ty] ty) f_ in
  let g = Term.const ~ty:(Type.arrow [ty] ty) g_ in
  let x = Term.var (HVar.fresh ~ty ()) in
  Alcotest.(check comp_test) "g x > f x x" Comparison.Nonstrict.Gt (compare (Term.app g [x]) (Term.app f [x; x]));

  (* f (x b) > x a *)
  let f = Term.const ~ty:(Type.arrow [ty] ty) f_ in
  let x = Term.var (HVar.fresh ~ty:(Type.arrow [ty] ty)  ()) in
  let a = Term.const ~ty a_ in
  let b = Term.const ~ty b_ in
  Alcotest.(check comp_test) "f (x b) > x a" Comparison.Nonstrict.Gt (compare (Term.app f [Term.app x [b]]) (Term.app x [a]));

  (* f a a > f b  ( test for length-lexicographic extension ) *)
  let f = Term.const ~ty:(Type.arrow [ty; ty] ty) f_ in
  let a = Term.const ~ty a_ in
  let b = Term.const ~ty b_ in
  Alcotest.(check comp_test) "f a a > f b" Comparison.Nonstrict.Gt (compare (Term.app f [a;a]) (Term.app f [b]))

let test_derived_ho_kbo = "ordering.derived_ho_kbo", `Quick, fun () ->
  (* alphabetical precedence, h has weight 2, all other symbols weight 1 *)
  let weight id = (if id=h_ then Precedence.Weight.add Precedence.Weight.one Precedence.Weight.one else Precedence.Weight.one) in
  let ord = O.derived_ho_kbo ~ignore_quans_under_lam:true
      (Precedence.create ~weight Precedence.Constr.alpha [a_; b_; c_; d_; e_; f_; g_; h_]) in
  let compare = O.compare ord in

  (* b > a *)
  let a = Term.const ~ty a_ in
  let b = Term.const ~ty b_ in
  Alcotest.(check comp_test) "b > a"
    Comparison.Nonstrict.Gt (compare b a);

  (* f b > f a *)
  let a = Term.const ~ty a_ in
  let b = Term.const ~ty b_ in
  let f = Term.const ~ty:(Type.arrow [ty] ty) f_ in
  Alcotest.(check comp_test) "f b > f a"
    Comparison.Nonstrict.Gt (compare (Term.app f [b]) (Term.app f [a]));

  (* f b a > f a b *)
  let a = Term.const ~ty a_ in
  let b = Term.const ~ty b_ in
  let f = Term.const ~ty:(Type.arrow [ty; ty] ty) f_ in
  Alcotest.(check comp_test) "f b a > f a b"
    Comparison.Nonstrict.Gt (compare (Term.app f [b; a]) (Term.app f [a; b]));

  (* z b <=>? z a *)
  let a = Term.const ~ty a_ in
  let b = Term.const ~ty b_ in
  let z = Term.var (HVar.fresh ~ty:(Type.arrow [ty] ty) ()) in
  Alcotest.(check comp_test) "z b <=>? z a"
    Comparison.Nonstrict.Incomparable (compare (Term.app z [b]) (Term.app z [a]));

  (* z b a <=>? z a b *)
  let a = Term.const ~ty a_ in
  let b = Term.const ~ty b_ in
  let z = Term.var (HVar.fresh ~ty:(Type.arrow [ty; ty] ty) ()) in
  Alcotest.(check comp_test) "z b a <=>? z a b"
    Comparison.Nonstrict.Incomparable (compare (Term.app z [b; a]) (Term.app z [a; b]));

  (* c < f (Y X a) b *)
  let f = Term.const ~ty:(Type.arrow [ty; ty] ty) f_ in
  let a = Term.const ~ty a_ in
  let b = Term.const ~ty b_ in
  let c = Term.const ~ty c_ in
  let x = Term.var (HVar.fresh ~ty ()) in
  let y = Term.var (HVar.fresh ~ty:(Type.arrow [ty; ty] ty) ()) in
  Alcotest.(check comp_test) "c < f (Y X a) b"
    Comparison.Nonstrict.Lt (compare c (Term.app f [Term.app y [x; a]; b]));

  (* h (x y) > f a (x y) *)
  let f = Term.const ~ty:(Type.arrow [ty; ty] ty) f_ in
  let h = Term.const ~ty:(Type.arrow [ty] ty) h_ in
  let a = Term.const ~ty a_ in
  let x = Term.var (HVar.fresh ~ty:(Type.arrow [ty] ty) ()) in
  let y = Term.var (HVar.fresh ~ty ()) in
  Alcotest.(check comp_test) "h (x y) > f a (x y)"
    Comparison.Nonstrict.Gt (compare (Term.app h [Term.app x [y]]) (Term.app f [a; Term.app x [y]]));

  (* forall x. x > h a a a *)
  let h = Term.const ~ty:(Type.arrow [ty;ty;ty] ty) h_ in
  let a = Term.const ~ty a_ in
  Alcotest.(check comp_test) "forall x. x > h a a a"
    Comparison.Nonstrict.Gt
    (compare 
      (Term.app_builtin ~ty:Type.prop Builtin.ForallConst [Term.of_ty Type.prop; Term.fun_l [Type.prop] (Term.bvar ~ty:Type.prop 0)]) 
      (Term.app h [a;a;a]));

  (* fun y. forall x. x < h a a a *)
  let h = Term.const ~ty:(Type.arrow [ty;ty;ty] ty) h_ in
  let a = Term.const ~ty a_ in
  Alcotest.(check comp_test) "fun y. forall x. x < h a a a"
    Comparison.Nonstrict.Lt
    (compare 
      (Term.fun_l [ty]
        (Term.app_builtin ~ty:Type.prop Builtin.ForallConst [Term.of_ty Type.prop; Term.fun_l [Type.prop] (Term.bvar ~ty:Type.prop 0)]) 
      )
      (Term.app h [a;a;a]));

  (* fun y. z <=>? z (Variables above and below lambdas need to be treated as if they were different variables) *)
  let z = Term.var (HVar.fresh ~ty ()) in
  Alcotest.(check comp_test) "fun y. z <=>? z"
    Comparison.Nonstrict.Incomparable (compare (Term.fun_l [ty] z) z);

  (* f z > z *)
  let f = Term.const ~ty:(Type.arrow [ty] ty) f_ in
  let z = Term.var (HVar.fresh ~ty ()) in
  Alcotest.(check comp_test) "f z > z"
    Comparison.Nonstrict.Gt (compare (Term.app f [z]) z);

  (* z a <=>? z (Because of fluidity) *)
  let a = Term.const ~ty a_ in
  let z = Term.var (HVar.fresh ~ty:(Type.arrow [ty] ty) ()) in
  Alcotest.(check comp_test) "z a <=>? z"
    Comparison.Nonstrict.Incomparable (compare (Term.app z [a]) z);

  (* lam x. z x a <=>? z (Because of fluidity) *)
  let a = Term.const ~ty a_ in
  let z = Term.var (HVar.fresh ~ty:(Type.arrow [ty; ty] ty) ()) in
  Alcotest.(check comp_test) "lam x. z x a <=>? z"
    Comparison.Nonstrict.Incomparable (compare (Term.fun_l [ty] (Term.app z [Term.bvar ~ty 0; a])) z);

  (* complexity *)
  let rec pow n f x =
    if n = 0 then x else pow (n - 1) f (f x)
  in
  let a = Term.const ~ty a_ in
  let b = Term.const ~ty b_ in
  let f = Term.const ~ty:(Type.arrow [ty] ty) f_ in
  let n = 10000 in
  Alcotest.(check comp_test) "f^n b > f^n a"
    Comparison.Nonstrict.Gt (compare (pow n (fun t -> Term.app f [t]) b) (pow n (fun t -> Term.app f [t]) a));

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
  let ord = O.derived_ho_kbo ~ignore_quans_under_lam:true
      (Precedence.create ~weight Precedence.Constr.alpha [add_; app_; funty_; k_; s_; zero_]) in
  let x = Term.var (HVar.fresh ~ty ()) in
  let y = Term.var (HVar.fresh ~ty ()) in
  let compare = O.compare ord in
  (*app (app add (app s zero)) k > app (app add zero)(app s k)*)
  Alcotest.(check comp_test)
    "… > …"
    Comparison.Nonstrict.Gt
    (compare
                  (T.app app [ty1; ty1; T.app app [ty1; ty2; add; (T.app app [ty1; ty1; s; zero])]; k])
                  (T.app app [ty1; ty1; T.app app [ty1; ty2; add; zero]; T.app app [ty1; ty1; s; k]])
               );
  (*app (app add (app s x)) y > app (app add x)(app s y)*)
  Alcotest.(check comp_test) 
    "… > …"
    Comparison.Nonstrict.Gt
    (compare
                   (T.app app [ty1; ty1; T.app app [ty1; ty2; add; (T.app app [ty1; ty1; s; x])]; y])
                   (T.app app [ty1; ty1; T.app app [ty1; ty2; add; x]; T.app app [ty1; ty1; s; y]])
                )

let test_lambdafree_kbo = "ordering.lambdafree_kbo", `Quick, fun () ->
  (* alphabetical precedence, h has weight 2, all other symbols weight 1*)
  let weight id = (if id=h_ then Precedence.Weight.add Precedence.Weight.one Precedence.Weight.one else Precedence.Weight.one) in
  let ord = O.lambdafree_kbo (Precedence.create ~weight Precedence.Constr.alpha [a_; b_; c_; d_; e_; f_; g_; h_]) in
  let compare = O.compare ord in

  (* h (x y) > f y (x a) *)
  let f = Term.const ~ty:(Type.arrow [ty; ty] ty) f_ in
  let h = Term.const ~ty:(Type.arrow [ty] ty) h_ in
  let a = Term.const ~ty a_ in
  let x = Term.var (HVar.fresh ~ty:(Type.arrow [ty] ty) ()) in
  let y = Term.var (HVar.fresh ~ty ()) in
  Alcotest.(check comp_test) "h (x y) > f y (x a)" Comparison.Nonstrict.Gt (compare (Term.app h [Term.app x [y]]) (Term.app f [y; Term.app x [a]]));

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
  let ord = O.lambdafree_kbo (Precedence.create ~weight Precedence.Constr.alpha [add_; app_; funty_; k_; s_; zero_]) in
  let x = Term.var (HVar.fresh ~ty ()) in
  let y = Term.var (HVar.fresh ~ty ()) in
  let compare = O.compare ord in
  (*app (app add (app s zero)) k > app (app add zero)(app s k)*)
  Alcotest.(check comp_test) "app (app add (app s zero)) k > app (app add zero)(app s k)"
    Comparison.Nonstrict.Gt
    (compare
      (T.app app [ty1; ty1; T.app app [ty1; ty2; add; (T.app app [ty1; ty1; s; zero])]; k])
      (T.app app [ty1; ty1; T.app app [ty1; ty2; add; zero]; T.app app [ty1; ty1; s; k]])
    );
  (*app (app add (app s x)) y > app (app add x)(app s y)*)
  Alcotest.(check comp_test) "app (app add (app s x)) y > app (app add x)(app s y)"
    Comparison.Nonstrict.Gt
    (compare
          (T.app app [ty1; ty1; T.app app [ty1; ty2; add; (T.app app [ty1; ty1; s; x])]; y])
          (T.app app [ty1; ty1; T.app app [ty1; ty2; add; x]; T.app app [ty1; ty1; s; y]])
      )

(* adapted from test_derived_ho_kbo *)
let test_lambda_kbo = "ordering.lambda_kbo", `Quick, fun () ->
  (* alphabetical precedence, h has weight 2, all other symbols weight 1 *)
  let weight id = (if id=h_ then Precedence.Weight.add Precedence.Weight.one Precedence.Weight.one else Precedence.Weight.one) in
  let ord = O.lambda_kbo
      (Precedence.create ~weight Precedence.Constr.alpha [a_; b_; c_; d_; e_; f_; g_; h_]) in
  let compare = O.compare ord in

  (* b > a *)
  let a = Term.const ~ty a_ in
  let b = Term.const ~ty b_ in
  Alcotest.(check comp_test) "b > a"
    Comparison.Nonstrict.Gt (compare b a);

  (* f b > f a *)
  let a = Term.const ~ty a_ in
  let b = Term.const ~ty b_ in
  let f = Term.const ~ty:(Type.arrow [ty] ty) f_ in
  Alcotest.(check comp_test) "f b > f a"
    Comparison.Nonstrict.Gt (compare (Term.app f [b]) (Term.app f [a]));

  (* f b a > f a b *)
  let a = Term.const ~ty a_ in
  let b = Term.const ~ty b_ in
  let f = Term.const ~ty:(Type.arrow [ty; ty] ty) f_ in
  Alcotest.(check comp_test) "f b a > f a b"
    Comparison.Nonstrict.Gt (compare (Term.app f [b; a]) (Term.app f [a; b]));

  (* z b >= z a *)
  let a = Term.const ~ty a_ in
  let b = Term.const ~ty b_ in
  let z = Term.var (HVar.fresh ~ty:(Type.arrow [ty] ty) ()) in
  Alcotest.(check comp_test) "z b >= z a"
    Comparison.Nonstrict.Geq (compare (Term.app z [b]) (Term.app z [a]));

  (* z g <=>? z f (could be improved to >=) *)
  let f = Term.const ~ty:(Type.arrow [ty] ty) f_ in
  let g = Term.const ~ty:(Type.arrow [ty] ty) g_ in
  let z = Term.var (HVar.fresh ~ty:(Type.arrow [Type.arrow [ty] ty] ty) ()) in
  Alcotest.(check comp_test) "z g <=>? z f"
    Comparison.Nonstrict.Incomparable (compare (Term.app z [g]) (Term.app z [f]));

  (* z h <=>? z f (due to weight difference) *)
  let f = Term.const ~ty:(Type.arrow [ty] ty) f_ in
  let h = Term.const ~ty:(Type.arrow [ty] ty) h_ in
  let z = Term.var (HVar.fresh ~ty:(Type.arrow [Type.arrow [ty] ty] ty) ()) in
  Alcotest.(check comp_test) "z h <=>? z f"
    Comparison.Nonstrict.Incomparable (compare (Term.app z [h]) (Term.app z [f]));

  (* z b c e >= z a c d *)
  let a = Term.const ~ty a_ in
  let b = Term.const ~ty b_ in
  let c = Term.const ~ty c_ in
  let d = Term.const ~ty d_ in
  let e = Term.const ~ty e_ in
  let z = Term.var (HVar.fresh ~ty:(Type.arrow [ty; ty; ty] ty) ()) in
  Alcotest.(check comp_test) "z b c e >= z a c d"
    Comparison.Nonstrict.Geq (compare (Term.app z [b; c; e]) (Term.app z [a; c; d]));

  (* z b a <=>? z a b *)
  let a = Term.const ~ty a_ in
  let b = Term.const ~ty b_ in
  let z = Term.var (HVar.fresh ~ty:(Type.arrow [ty; ty] ty) ()) in
  Alcotest.(check comp_test) "z b a <=>? z a b"
    Comparison.Nonstrict.Incomparable (compare (Term.app z [b; a]) (Term.app z [a; b]));

  (* c < f (Y X a) b *)
  let f = Term.const ~ty:(Type.arrow [ty; ty] ty) f_ in
  let a = Term.const ~ty a_ in
  let b = Term.const ~ty b_ in
  let c = Term.const ~ty c_ in
  let x = Term.var (HVar.fresh ~ty ()) in
  let y = Term.var (HVar.fresh ~ty:(Type.arrow [ty; ty] ty) ()) in
  Alcotest.(check comp_test) "c < f (Y X a) b"
    Comparison.Nonstrict.Lt (compare c (Term.app f [Term.app y [x; a]; b]));

  (* h (x y) > f a (x y) *)
  let f = Term.const ~ty:(Type.arrow [ty; ty] ty) f_ in
  let h = Term.const ~ty:(Type.arrow [ty] ty) h_ in
  let a = Term.const ~ty a_ in
  let x = Term.var (HVar.fresh ~ty:(Type.arrow [ty] ty) ()) in
  let y = Term.var (HVar.fresh ~ty ()) in
  Alcotest.(check comp_test) "h (x y) > f a (x y)"
    Comparison.Nonstrict.Gt (compare (Term.app h [Term.app x [y]]) (Term.app f [a; Term.app x [y]]));

  (* forall x. x > h a a a *)
  let h = Term.const ~ty:(Type.arrow [ty;ty;ty] ty) h_ in
  let a = Term.const ~ty a_ in
  Alcotest.(check comp_test) "forall x. x > h a a a"
    Comparison.Nonstrict.Gt
    (compare 
      (Term.app_builtin ~ty:Type.prop Builtin.ForallConst [Term.of_ty Type.prop; Term.fun_l [Type.prop] (Term.bvar ~ty:Type.prop 0)]) 
      (Term.app h [a;a;a]));

  (* fun y. forall x. x > h a a a (cf. derived_ho_kbo) *)
  let h = Term.const ~ty:(Type.arrow [ty;ty;ty] ty) h_ in
  let a = Term.const ~ty a_ in
  Alcotest.(check comp_test) "fun y. forall x. x > h a a a"
    Comparison.Nonstrict.Gt
    (compare 
      (Term.fun_l [ty]
        (Term.app_builtin ~ty:Type.prop Builtin.ForallConst [Term.of_ty Type.prop; Term.fun_l [Type.prop] (Term.bvar ~ty:Type.prop 0)])
      )
      (Term.app h [a;a;a]));

  (* fun y. z > z (cf. derived_ho_kbo) *)
  let z = Term.var (HVar.fresh ~ty ()) in
  Alcotest.(check comp_test) "fun y. z > z"
    Comparison.Nonstrict.Gt (compare (Term.fun_l [ty] z) z);

  (* f z > z *)
  let f = Term.const ~ty:(Type.arrow [ty] ty) f_ in
  let z = Term.var (HVar.fresh ~ty ()) in
  Alcotest.(check comp_test) "f z > z"
    Comparison.Nonstrict.Gt (compare (Term.app f [z]) z);

  (* z a < z *)
  let a = Term.const ~ty a_ in
  let z = Term.var (HVar.fresh ~ty:(Type.arrow [ty] ty) ()) in
  Alcotest.(check comp_test) "z a < z"
    Comparison.Nonstrict.Lt (compare (Term.app z [a]) z);

  (* lam x. z x a < z *)
  let a = Term.const ~ty a_ in
  let z = Term.var (HVar.fresh ~ty:(Type.arrow [ty; ty] ty) ()) in
  Alcotest.(check comp_test) "lam x. z x a < z"
    Comparison.Nonstrict.Lt (compare (Term.fun_l [ty] (Term.app z [Term.bvar ~ty 0; a])) z);

  (* z a <=>? false *)
  let a = Term.const ~ty a_ in
  let z = Term.var (HVar.fresh ~ty:(Type.arrow [ty] ty) ()) in
  Alcotest.(check comp_test) "z a <=>? false"
    Comparison.Nonstrict.Incomparable (compare (Term.app z [a])
      (Term.app_builtin ~ty:Type.prop Builtin.False []));

  (* z a >= true *)
  let a = Term.const ~ty a_ in
  let z = Term.var (HVar.fresh ~ty:(Type.arrow [ty] ty) ()) in
  Alcotest.(check comp_test) "z a >= true"
    Comparison.Nonstrict.Geq (compare (Term.app z [a])
      (Term.app_builtin ~ty:Type.prop Builtin.True []));

  (* y (z a) <=>? z (y a) *)
  let a = Term.const ~ty a_ in
  let y = Term.var (HVar.fresh ~ty:(Type.arrow [ty] ty) ()) in
  let z = Term.var (HVar.fresh ~ty:(Type.arrow [ty] ty) ()) in
  Alcotest.(check comp_test) "y (z a) <=>? z (y a)"
    Comparison.Nonstrict.Incomparable (compare
      (Term.app y [Term.app z [a]])
      (Term.app z [Term.app y [a]]));

  (* f (y (z a)) <=>? z (y a) *)
  let a = Term.const ~ty a_ in
  let f = Term.const ~ty:(Type.arrow [ty] ty) f_ in
  let y = Term.var (HVar.fresh ~ty:(Type.arrow [ty] ty) ()) in
  let z = Term.var (HVar.fresh ~ty:(Type.arrow [ty] ty) ()) in
  Alcotest.(check comp_test) "f (y (z a)) < z (y a)"
    Comparison.Nonstrict.Incomparable (compare
      (Term.app f [Term.app y [Term.app z [a]]])
      (Term.app z [Term.app y [a]]));

  (* complexity *)
  let rec pow n f x =
    if n = 0 then x else pow (n - 1) f (f x)
  in
  let a = Term.const ~ty a_ in
  let b = Term.const ~ty b_ in
  let f = Term.const ~ty:(Type.arrow [ty] ty) f_ in
  let n = 10000 in
  Alcotest.(check comp_test) "f^n b > f^n a"
    Comparison.Nonstrict.Gt (compare (pow n (fun t -> Term.app f [t]) b) (pow n (fun t -> Term.app f [t]) a));

  (* maximal sides of a literal *)

  (* a = b *)
  let a = Term.const ~ty a_ in
  let b = Term.const ~ty b_ in
  let a_eq_b = Literal.mk_eq a b in
  let b_eq_a = Literal.mk_eq b a in
  Alcotest.(check bool) "b is max of a = b"
    true (CCList.equal T.equal (Literal.Comp.max_terms ~ord a_eq_b) [b]);
  Alcotest.(check bool) "b is max of b = a"
    true (CCList.equal T.equal (Literal.Comp.max_terms ~ord b_eq_a) [b]);

  (* z a = z b *)
  let a = Term.const ~ty a_ in
  let b = Term.const ~ty b_ in
  let z = Term.var (HVar.fresh ~ty:(Type.arrow [ty] ty) ()) in
  let za = Term.app z [a] in
  let zb = Term.app z [b] in
  let za_eq_zb = Literal.mk_eq za zb in
  let zb_eq_za = Literal.mk_eq zb za in
  Alcotest.(check bool) "z b is max of z a = z b"
    true (CCList.equal T.equal (Literal.Comp.max_terms ~ord za_eq_zb) [zb]);
  Alcotest.(check bool) "z b is max of z b = z a"
    true (CCList.equal T.equal (Literal.Comp.max_terms ~ord zb_eq_za) [zb]);

  (* maximal literals *)

  (* (f b (z b) = b) > (f b (z a) = a) *)
  let a = Term.const ~ty a_ in
  let b = Term.const ~ty b_ in
  let f = Term.const ~ty:(Type.arrow [ty; ty] ty) f_ in
  let z = Term.var (HVar.fresh ~ty:(Type.arrow [ty] ty) ()) in
  let fbzb_eq_b = Literal.mk_eq (Term.app f [b; Term.app z [b]]) b in
  let fbza_eq_a = Literal.mk_eq (Term.app f [b; Term.app z [a]]) a in
  Alcotest.(check comp_test) "(f b (z b) = b) > (f b (z a) = a)"
    Comparison.Nonstrict.Gt (Literal.Comp.compare ~ord fbzb_eq_b fbza_eq_a);

  (* (f b (z b) = f b (z a)) > (f b (z a) = a) *)
  let a = Term.const ~ty a_ in
  let b = Term.const ~ty b_ in
  let f = Term.const ~ty:(Type.arrow [ty; ty] ty) f_ in
  let z = Term.var (HVar.fresh ~ty:(Type.arrow [ty] ty) ()) in
  let fbzb_eq_fbza = Literal.mk_eq (Term.app f [b; Term.app z [b]]) (Term.app f [b; Term.app z [a]]) in
  let fbza_eq_a = Literal.mk_eq (Term.app f [b; Term.app z [a]]) a in
  Alcotest.(check comp_test) "(f b (z b) = f b (z a)) > (f b (z a) = a)"
    Comparison.Nonstrict.Gt (Literal.Comp.compare ~ord fbzb_eq_fbza fbza_eq_a);

  (* (f b (z b) = y b) >= (f b (z a) = y a) *)
  let a = Term.const ~ty a_ in
  let b = Term.const ~ty b_ in
  let f = Term.const ~ty:(Type.arrow [ty; ty] ty) f_ in
  let y = Term.var (HVar.fresh ~ty:(Type.arrow [ty] ty) ()) in
  let z = Term.var (HVar.fresh ~ty:(Type.arrow [ty] ty) ()) in
  let fbzb_eq_yb = Literal.mk_eq (Term.app f [b; Term.app z [b]]) (Term.app y [b]) in
  let fbza_eq_ya = Literal.mk_eq (Term.app f [b; Term.app z [a]]) (Term.app y [a]) in
  Alcotest.(check comp_test) "(f b (z b) = y b) >= (f b (z a) = y a)"
    Comparison.Nonstrict.Geq (Literal.Comp.compare ~ord fbzb_eq_yb fbza_eq_ya);

  (* (f b (z b) = z b) >= (f b (z a) = z a) *)
  let a = Term.const ~ty a_ in
  let b = Term.const ~ty b_ in
  let f = Term.const ~ty:(Type.arrow [ty; ty] ty) f_ in
  let z = Term.var (HVar.fresh ~ty:(Type.arrow [ty] ty) ()) in
  let fbzb_eq_zb = Literal.mk_eq (Term.app f [b; Term.app z [b]]) (Term.app z [b]) in
  let fbza_eq_za = Literal.mk_eq (Term.app f [b; Term.app z [a]]) (Term.app z [a]) in
  Alcotest.(check comp_test) "(f b (z b) = z b) >= (f b (z a) = z a)"
    Comparison.Nonstrict.Geq (Literal.Comp.compare ~ord fbzb_eq_zb fbza_eq_za);

  (* (f b (z a) = b) <=>? (f b (z b) = a) *)
  let a = Term.const ~ty a_ in
  let b = Term.const ~ty b_ in
  let f = Term.const ~ty:(Type.arrow [ty; ty] ty) f_ in
  let z = Term.var (HVar.fresh ~ty:(Type.arrow [ty] ty) ()) in
  let fbza_eq_b = Literal.mk_eq (Term.app f [b; Term.app z [a]]) b in
  let fbzb_eq_a = Literal.mk_eq (Term.app f [b; Term.app z [b]]) a in
  Alcotest.(check comp_test) "f b (z a) = b <=>? f b (z b) = a"
    Comparison.Nonstrict.Incomparable (Literal.Comp.compare ~ord fbza_eq_b fbzb_eq_a);

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
  let ord = O.lambda_kbo
      (Precedence.create ~weight Precedence.Constr.alpha [add_; app_; funty_; k_; s_; zero_]) in
  let x = Term.var (HVar.fresh ~ty ()) in
  let y = Term.var (HVar.fresh ~ty ()) in
  let compare = O.compare ord in
  (*app (app add (app s zero)) k > app (app add zero)(app s k)*)
  Alcotest.(check comp_test)
    "… > …"
    Comparison.Nonstrict.Gt
    (compare
                  (T.app app [ty1; ty1; T.app app [ty1; ty2; add; (T.app app [ty1; ty1; s; zero])]; k])
                  (T.app app [ty1; ty1; T.app app [ty1; ty2; add; zero]; T.app app [ty1; ty1; s; k]])
               );
  (*app (app add (app s x)) y > app (app add x)(app s y)*)
  Alcotest.(check comp_test) 
    "… > …"
    Comparison.Nonstrict.Gt
    (compare
                   (T.app app [ty1; ty1; T.app app [ty1; ty2; add; (T.app app [ty1; ty1; s; x])]; y])
                   (T.app app [ty1; ty1; T.app app [ty1; ty2; add; x]; T.app app [ty1; ty1; s; y]])
                )

let suite =
  [ test_derived_ho_rpo;
    test_derived_ho_kbo;
    test_lambdafree_rpo;
    test_lambdafree_kbo;
    test_lambda_kbo;
  ]

let props =
  CCList.flat_map
    (fun o ->
       [ check_ordering_inv_by_subst ~gen_t:ArTerm.default_ho_g o;
         check_ordering_trans ~arb_t:ArTerm.default_ho o;
         check_ordering_swap_args ~arb_t:ArTerm.default_ho o;
         check_ordering_subterm ~arb_t:ArTerm.default_ho o;
       ])
    [
      O.derived_ho_kbo ~ignore_quans_under_lam:true (Precedence.default []);
      O.derived_ho_rpo (Precedence.default []);
      O.lambda_kbo (Precedence.default []);
    ]
  @
  CCList.flat_map
    (fun o ->
       [ check_ordering_inv_by_subst ~gen_t:ArTerm.default_lfho_g o;
         check_ordering_trans ~arb_t:ArTerm.default_lfho o;
         check_ordering_swap_args ~arb_t:ArTerm.default_lfho o;
         check_ordering_subterm ~arb_t:ArTerm.default_lfho o;
       ])
    [
      O.lambdafree_kbo (Precedence.default []);
      O.lambdafree_rpo (Precedence.default []);
      O.epo (Precedence.default []);
    ]

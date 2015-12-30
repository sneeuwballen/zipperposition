
(* This file is free software, part of Zipperposition. See file "license" for more details. *)

(** test orderings *)

open Libzipperposition
open Libzipperposition_arbitrary
open OUnit

module T = FOTerm
module S = Substs.FO
module O = Ordering

let suite =
  "test_ordering" >:::
    []

open QCheck

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

let check_ordering_inv_by_subst ord =
  let name = CCFormat.sprintf "ordering_%s_inv_by_subst" (O.name ord) in
  let pp = PP.triple T.to_string T.to_string Substs.to_string in
  (* generate pairs of terms, and grounding substitutions *)
  let gen = Arbitrary.((pair ArTerm.default ArTerm.default) >>= fun (t1, t2) ->
    let vars = Sequence.of_list [t1; t2]
      |> Sequence.flat_map T.Seq.vars
      |> T.VarSet.of_seq
    in
    (* grounding substitution *)
    let subst st = T.VarSet.fold
      (fun v subst ->
        let v = (v : Type.t HVar.t :> InnerTerm.t HVar.t) in
        S.bind subst (v,1) (ArTerm.ground st,0))
      vars Substs.empty in
    triple (return t1) (return t2) subst)
  in
  let size (t1, t2, s) =
    T.size t1 + T.size t2 +
      (Substs.fold (fun n _ (t,_) -> n + T.size (T.of_term_unsafe t)) 0 s)
  in
  (* do type inference on the fly
  let tyctx = TypeInference.Ctx.create () in
  let signature = ref Signature.empty in
  *)
  let ord = ref ord in
  let prop (t1, t2, subst) =
    let t1' = S.apply_no_renaming subst (t1,0) in
    let t2' = S.apply_no_renaming subst (t2,0) in
    (* FIXME use a fixed signature?
    ignore (TypeInference.FO.infer tyctx t1 0);
    ignore (TypeInference.FO.infer tyctx t2 0);
    ignore (TypeInference.FO.infer tyctx t1' 0);
    ignore (TypeInference.FO.infer tyctx t2' 0);
    signature := TypeInference.Ctx.to_signature tyctx;
    ord := O.add_signature !ord !signature;
    *)
    (* check that instantiating variables preserves ordering *)
    let o1 = O.compare !ord t1 t2 in
    let o2 = O.compare !ord t1' t2' in
    more_specific o1 o2
  in
  mk_test ~n:1000 ~name ~pp ~size gen prop

let props =
  [ check_ordering_inv_by_subst (O.kbo (Precedence.default []))
  ; check_ordering_inv_by_subst (O.rpo6 (Precedence.default []))
  ]


(* This file is free software, part of Zipperposition. See file "license" for more details. *)

(** test orderings *)

open Libzipperposition
open Libzipperposition_arbitrary

module T = FOTerm
module S = Substs.FO
module O = Ordering

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
  let pp = QCheck.Print.triple T.to_string T.to_string Substs.to_string in
  (* generate pairs of terms, and grounding substitutions *)
  let gen = QCheck.Gen.(
    (pair ArTerm.default_g ArTerm.default_g)
    >>= fun (t1, t2) ->
    let vars = Sequence.of_list [t1; t2]
      |> Sequence.flat_map T.Seq.vars
      |> T.VarSet.of_seq
    in
    (* grounding substitution *)
    let subst st = T.VarSet.fold
      (fun v subst ->
        let v = (v : Type.t HVar.t :> InnerTerm.t HVar.t) in
        S.bind subst (v,1) (ArTerm.ground_g st,0))
      vars Substs.empty in
    triple (return t1) (return t2) subst)
  in
  let size (t1, t2, s) =
    T.size t1 + T.size t2 +
      (Substs.fold (fun n _ (t,_) -> n + T.size (T.of_term_unsafe t)) 0 s)
  in
  let gen = QCheck.make ~print:pp ~small:size gen in
  (* do type inference on the fly
  let tyctx = TypeInference.Ctx.create () in
  let signature = ref Signature.empty in
  *)
  let ord = ref ord in
  let prop (t1, t2, subst) =
    (* declare symbols *)
    Sequence.of_list [t1;t2]
      |> Sequence.flat_map T.Seq.symbols
      |> ID.Set.of_seq |> ID.Set.to_seq
      |> O.add_seq !ord;
    let t1' = S.apply_no_renaming subst (t1,0) in
    let t2' = S.apply_no_renaming subst (t2,0) in
    (* check that instantiating variables preserves ordering *)
    let o1 = O.compare !ord t1 t2 in
    let o2 = O.compare !ord t1' t2' in
    more_specific o1 o2
  in
  QCheck.Test.make ~count:1000 ~name gen prop

let props =
  [ check_ordering_inv_by_subst (O.kbo (Precedence.default []))
  ; check_ordering_inv_by_subst (O.rpo6 (Precedence.default []))
  ]


(*
Zipperposition: a functional superposition prover for prototyping
Copyright (c) 2013, Simon Cruanes
All rights reserved.

Redistribution and use in source and binary forms, with or without
modification, are permitted provided that the following conditions are met:

Redistributions of source code must retain the above copyright notice, this
list of conditions and the following disclaimer.  Redistributions in binary
form must reproduce the above copyright notice, this list of conditions and the
following disclaimer in the documentation and/or other materials provided with
the distribution.

THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS "AS IS" AND
ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED
WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE ARE
DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT HOLDER OR CONTRIBUTORS BE LIABLE
FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL
DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR
SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER
CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY,
OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE
OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
*)

(** {1 Tests for Monome} *)

open Logtk
open Libzipperposition
open QCheck

module T = FOTerm
module S = Substs.FO
module M = Monome

(* m1 + m2 == m1 - (- m2) *)
let check_add_diff ar name =
  let gen = Arbitrary.pair ar ar in
  let prop (m1,m2) =
    M.eq (M.sum m1 m2) (M.difference m1 (M.uminus m2))
  in
  let pp = PP.pair M.to_string M.to_string in
  let name = "monome_add_diff_uminus_" ^ name in
  mk_test ~name ~pp gen prop

(* deserialise (serialise m) = m *)
let check_bij =
  let gen = M.arbitrary_int in
  let prop m =
    let bij = M.bij in
    let m = M.normalize m in
    let s = Bij.TrBencode.to_string ~bij m in
    let m' = Bij.TrBencode.of_string ~bij s in
    M.eq m m'
  in
  mk_test ~name:"monome_bij" ~pp:M.to_string gen prop

let check_has_instances_rat =
  let gen = M.arbitrary_rat in
  let prop m = M.has_instances m in
  mk_test ~name:"monome_rat_has_instances" ~pp:M.to_string gen prop

(* m has instances (for int) => m*c has instances too *)
let check_has_instances_int_prod =
  let gen = Arbitrary.(pair M.arbitrary_int (lift Symbol.mk_int (2 -- 5))) in
  let prop (m,c) =
    Prop.assume (M.has_instances m);
    M.has_instances (M.product m c)
  in
  let pp = PP.(pair M.to_string Symbol.to_string) in
  let name = "monome_int_has_instances_kept_by_product" in
  mk_test ~pp ~name ~n:1000 gen prop

let check_has_instances_int_normalize =
  let gen = M.arbitrary_int in
  let prop m =
    (M.has_instances m) = (M.has_instances (M.normalize m))
  in
  let name = "monome_int_has_instances_inv_by_normalization" in
  mk_test ~name ~pp:M.to_string gen prop

let check_normalization_idempotent =
  let gen = M.arbitrary in
  let prop m =
    M.eq (M.normalize m) (M.normalize (M.normalize m))
  in
  let name = "monome_normalization_idempotent" in
  mk_test ~name ~pp:M.to_string gen prop

(* check  that applying empty substitution gets the same monome *)
let check_apply_empty_subst =
  let gen = M.arbitrary in
  let prop m =
    let renaming = S.Renaming.dummy in
    M.eq m (M.apply_subst ~renaming S.empty m 0)
  in
  let name = "monome_apply_empty_subst_is_eq" in
  mk_test ~name ~pp:M.to_string gen prop

(* check comparison if we add an offset to a monome *)
let check_comparison_offset =
  let gen = Arbitrary.(pair M.arbitrary_int (lift Symbol.mk_int (5 -- 10))) in
  let prop (m, n) =
    let m' = M.add_const m n in
    M.comparison m m' = Comparison.Lt
  in
  let name = "monome_add_const_is_bigger" in
  let pp = PP.pair M.to_string Symbol.to_string in
  mk_test ~name ~pp ~n:1000 gen prop

let check_comparison_antisymmetric =
  let gen = Arbitrary.(
    among [Type.int; Type.rat] >>= fun ty ->
    pair (M.arbitrary_ty ty) (M.arbitrary_ty ty))
  in
  let prop (m1, m2) =
    let o = M.comparison m1 m2 in
    let o' = M.comparison m2 m1 in
    o = Comparison.opp o'
  in
  let pp = PP.pair M.to_string M.to_string in
  let name = "check_comparison_antisymmetric" in
  mk_test ~name ~pp gen prop

(* check that comparison is compatible with substitutions *)
let check_comparison_compatible_subst =
  let gen = Arbitrary.(
    pair M.arbitrary_int M.arbitrary_int >>= fun (m1,m2) ->
    let vars = Util.list_uniq T.eq (M.vars m1 @ M.vars m2) in
    (* grounding substitution *)
    let subst st = List.fold_left
      (fun subst v -> S.bind subst v 0 (T.arbitrary_ground st) 0) S.empty vars in
    triple (return m1) (return m2) subst
  ) in
  let prop (m1, m2, subst) =
    let o = M.comparison m1 m2 in
    let renaming = S.Renaming.create 5 in
    let m1' = M.apply_subst ~renaming subst m1 0 in
    let m2' = M.apply_subst ~renaming subst m2 0 in
    let o' = M.comparison m1' m2' in
    match o, o' with
    | Comparison.Gt, (Comparison.Lt | Comparison.Eq | Comparison.Incomparable)
    | Comparison.Lt, (Comparison.Gt | Comparison.Eq | Comparison.Incomparable)
    | Comparison.Eq, (Comparison.Gt | Comparison.Lt | Comparison.Incomparable) -> false
    | _ -> true
  in
  let name = "monome_comparison_compatible_with_subst" in
  let pp = PP.triple M.to_string M.to_string Substs.FO.to_string in
  mk_test ~name ~pp ~n:1000 gen prop

(* check that Solve.diophant2 actually finds valid solutions *)
let check_diophant2 =
  let mkbig = Big_int.big_int_of_int in
  let bgstr = Big_int.string_of_big_int in
  let ppbi buf i = Buffer.add_string buf (bgstr i) in
  let gen = Arbitrary.(
    (* obtain a triple of non null bigints *)
    let gen =
      lift3 (fun x y z -> x,y,z) (~-20 --30) (~-15 -- 20) (~-20 -- 30) >>= fun (a,b,c) ->
      if a * b = 0 then return None else return (Some (a,b,c))
    in
    retry gen >>= fun (a,b,c) ->
    (* find 5 variables to try solutions *)
    list_repeat 5 (lift mkbig (~-20 -- 20)) >>= fun l ->
    return (mkbig a, mkbig b, mkbig c, l)
  ) in
  (* check that solutions are indeed solutions *)
  let prop (a,b,const,l) =
    Util.debug 5 "try to solve %a x + %a y = %a" ppbi a ppbi b ppbi const;
    try
      let u, v, gcd = M.Solve.diophant2 a b const in
      let res = List.for_all
        (fun k ->
          (* x = u + k b  and y = v - k a *)
          let x = Big_int.add_big_int u (Big_int.mult_big_int k b) in
          let y = Big_int.sub_big_int v (Big_int.mult_big_int k a) in
          (* check that a x + b y = const *)
          let const' = Big_int.add_big_int
            (Big_int.mult_big_int a x)
            (Big_int.mult_big_int b y) in
          Big_int.eq_big_int const const')
        l
      in
      res
    with (Failure _) as e ->
      Prop.assume false; false
  in
  let name = "monome_diophant2" in
  let pp = PP.(quad bgstr bgstr bgstr (list bgstr)) in
  mk_test ~name ~pp ~n:100_000 gen prop

let props =
  [ check_add_diff M.arbitrary_int "int"
  ; check_add_diff M.arbitrary_int "rat"
  ; check_bij
  ; check_has_instances_rat
  ; check_has_instances_int_prod
  ; check_has_instances_int_normalize
  ; check_normalization_idempotent
  ; check_comparison_offset
  ; check_comparison_antisymmetric
  ; check_apply_empty_subst
  ; check_comparison_compatible_subst
  ; check_diophant2
  ]

(*
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

(** test orderings *)

open Logtk
open Logtk_arbitrary
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
  let name = Util.sprintf "ordering_%s_inv_by_subst" (O.name ord) in
  let pp = PP.triple T.to_string T.to_string S.to_string in
  (* generate pairs of terms, and grounding substitutions *)
  let gen = Arbitrary.((pair ArTerm.default ArTerm.default) >>= fun (t1, t2) ->
    let vars = T.vars_list [t1; t2] in
    (* grounding substitution *)
    let subst st = List.fold_left
      (fun subst v -> S.bind subst v 1 (ArTerm.ground st) 0) S.empty vars in
    triple (return t1) (return t2) subst)
  in
  let size (t1, t2, s) =
    T.size t1 + T.size t2 + (S.fold s 0 (fun n _ _ t _ -> n + T.size t))
  in
  (* do type inference on the fly 
  let tyctx = TypeInference.Ctx.create () in
  *)
  let signature = ref Signature.empty in
  let ord = ref ord in
  let prop (t1, t2, subst) =
    let t1' = S.apply_no_renaming subst t1 0 in
    let t2' = S.apply_no_renaming subst t2 0 in
    (* FIXME use a fixed signature?
    ignore (TypeInference.FO.infer tyctx t1 0);
    ignore (TypeInference.FO.infer tyctx t2 0);
    ignore (TypeInference.FO.infer tyctx t1' 0);
    ignore (TypeInference.FO.infer tyctx t2' 0);
    signature := TypeInference.Ctx.to_signature tyctx;
    *)
    ord := O.add_signature !ord !signature;
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

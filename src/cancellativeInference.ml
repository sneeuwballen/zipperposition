
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

(** {1 Cancellative Inferences} *)

open Logtk

module T = FOTerm
module AT = ArithTerm
module Lit = Literal
module I = ProofState.CancellativeIndex
module C = Clause
module S = Symbol
module M = Monome

module Canon = ArithLit.Canonical
module Foc = ArithLit.Focused

let stat_canc_sup = Util.mk_stat "canc.superposition"

(* do cancellative superposition *)
let _do_canc ~ctx active idx_a lit_a s_a passive idx_p lit_p s_p subst acc =
  let ord = Ctx.ord ctx in
  (* TODO: check also that lit_a.term maximal in lit_a,
    and lit_p.term maximal in lit_p. For this, need AC-compatible order *)
  if C.is_maxlit active idx_a subst s_a
  && C.is_maxlit passive idx_p subst s_p
  then begin
    (* get both lit with same coeff for unified term *)
    let lit_a, lit_p = Foc.scale lit_a lit_p in
    assert (S.Arith.sign lit_a.Foc.coeff > 0);
    assert (S.Arith.sign lit_p.Foc.coeff > 0);
    let renaming = Ctx.renaming_clear ctx in
    (* other literals *)
    let lits_a = Util.array_except_idx active.C.hclits idx_a in
    let lits_a = Literal.apply_subst_list ~renaming ~ord subst lits_a s_a in
    let lits_p = Util.array_except_idx passive.C.hclits idx_p in
    let lits_p = Literal.apply_subst_list ~renaming ~ord subst lits_p s_p in
    (* new literal: lit_a="t+m1=m2", lit_p="t'+m1' R m2'" for some
      relation R. Now let's replace t' by m2-m1 in lit', ie,
      build m = "m1'-m2'+(m2-m1) R 0". *)
    let m_p = M.difference lit_p.Foc.same_side lit_p.Foc.other_side in
    let m_p = M.apply_subst ~renaming subst m_p s_p in
    let m_a = M.difference lit_a.Foc.other_side lit_a.Foc.same_side in
    let m_a = M.apply_subst ~renaming subst m_a s_a in
    let m = M.sum m_a m_p in
    let lit = Canon.of_monome lit_p.Foc.op m in
    let lit = Canon.to_lit ~ord lit in
    let all_lits = lit :: lits_a @ lits_p in
    (* build clause *)
    let proof cc = Proof.mk_c_step cc ~rule:"canc_sup" [active.C.hcproof; passive.C.hcproof] in
    let new_c = C.create ~parents:[active;passive] ~ctx all_lits proof in
    Util.debug 5 "cancellative superposition of %a and %a gives %a"
      C.pp active C.pp passive C.pp new_c;
    Util.incr_stat stat_canc_sup;
    new_c :: acc
  end else
    acc

let canc_sup_active (state:ProofState.ActiveSet.t) c =
  let ctx = state#ctx in
  let ord = Ctx.ord ctx in
  let eligible = C.Eligible.param c in
  let res = ArithLit.Arr.fold_focused ~eligible ~ord c.C.hclits []
    (fun acc i lit ->
      match lit.Foc.op with
      | ArithLit.Eq ->
        let t = lit.Foc.term in
        Util.debug 5 "attempt to active superpose %a in %a[%d]" T.pp t C.pp c i;
        I.retrieve_unifiables state#idx_canc 0 t 1 acc
          (fun acc t' (passive,j,lit') subst ->
            _do_canc ~ctx c i lit 1 passive j lit' 0 subst acc
          )
      | _ -> acc)
  in
  res

let canc_sup_passive state c =
  let ctx = state#ctx in
  let ord = Ctx.ord ctx in
  let eligible = C.Eligible.res c in
  let res = ArithLit.Arr.fold_focused ~eligible ~ord c.C.hclits []
    (fun acc i lit ->
      let t = lit.Foc.term in
      Util.debug 5 "attempt to passive superpose %a in %a[%d]" T.pp t C.pp c i;
      I.retrieve_unifiables state#idx_canc 1 t 0 acc
        (fun acc t' (active,j,lit') subst ->
          _do_canc ~ctx active j lit' 1 c i lit 0 subst acc
        ))
  in
  res

let canc_equality_resolution c = []

let canc_equality_factoring c =  []

let canc_eq_chaining_active state c = []
  
let canc_eq_chaining_passive state c = []
  
let canc_ineq_chaining_left state c = []
  
let canc_ineq_chaining_right state c = []
  
let canc_reflexivity_res c = []
  
let canc_ineq_factoring c = []

let is_tautology c = false

(** {2 Setup} *)

let setup_penv ~ctx ~penv =
  Util.debug 2 "cancellative inf: setup penv";
  ()

let setup_env ~env =
  Util.debug 2 "cancellative inf: setup env";
  Env.add_binary_inf ~env "canc_sup_active" canc_sup_active;
  Env.add_binary_inf ~env "canc_sup_passive" canc_sup_passive;
  Env.add_unary_inf ~env "canc_eq_resolution" canc_equality_resolution;
  Env.add_unary_inf ~env "canc_eq_factoring" canc_equality_factoring;
  Env.add_binary_inf ~env "canc_ineq_chaining_left" canc_ineq_chaining_left;
  Env.add_binary_inf ~env "canc_ineq_chaining_right" canc_ineq_chaining_right;
  Env.add_unary_inf ~env "canc_reflexivity_res" canc_reflexivity_res;
  Env.add_unary_inf ~env "canc_ineq_factoring" canc_ineq_factoring;
  Env.add_is_trivial ~env is_tautology;
  ()
  

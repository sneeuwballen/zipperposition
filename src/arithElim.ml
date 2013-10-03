
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

(** {1 Elimination of Arithmetic} *)

open Logtk

module S = Symbol
module T = Term
module F = Formula
module C = Clause
module PF = PFormula
module Lit = Literal
module Lits = Literal.Arr

(** {2 Inference Rules} *)

let rewrite_lit ~ctx lit =
  let signature = Ctx.signature ~ctx in
  let lit' = match lit with
  | Lit.Prop (p, sign) ->
    let p' = Arith.T.simplify ~signature p in
    begin match p'.T.term, sign with
    | T.Node (S.Const("$less",_), [l;r]), false ->
      (* not (l < r) ---> r <= l *)
      Lit.mk_true (Arith.T.mk_lesseq r l)
    | T.Node (S.Const("$lesseq",_), [l;r]), false ->
      (* not (l <= r) ---> r < l *)
      Lit.mk_true (Arith.T.mk_less r l)
    | _ -> Lit.mk_prop p' sign
    end
  | Lit.Equation (l, r, sign, _) ->
    let l' = Arith.T.simplify ~signature l in
    let r' = Arith.T.simplify ~signature r in
    begin match sign with
    | true when Arith.T.is_arith_const l'
      && Arith.T.is_arith_const r' && not (T.eq l' r') ->
      (* i = j, when i and j are distinct numbers ---> false *)
      Lit.mk_false T.true_term
    | false when Arith.T.is_arith_const l'
      && Arith.T.is_arith_const r' && not (T.eq l' r') ->
      (* i != j. when i and j are distinct numbers ---> true *)
      Lit.mk_true T.true_term
    | _ ->
      (* just keep the literal *)
      Lit.mk_lit ~ord:(Ctx.ord ctx) l' r' sign 
    end
  | Lit.True
  | Lit.False -> lit
  in
  let signature = Ctx.signature ctx in
  if Arith.Lit.is_trivial ~signature lit'
    then Lit.mk_tauto
  else if Arith.Lit.has_instances ~signature lit'
    then lit'
    else Lit.mk_absurd

let eliminate_arith c =
  let ctx = c.C.hcctx in
  let signature = Ctx.signature ctx in
  let eligible = C.Eligible.max c in
  (* eliminate i-th literal with [subst] *)
  let eliminate_lit i subst =
    let lits' = Util.array_except_idx c.C.hclits i in
    let renaming = Ctx.renaming_clear ~ctx in
    let ord = Ctx.ord ctx in
    let lits' = Lit.apply_subst_list ~ord ~renaming subst lits' 0 in
    let proof cc = Proof.mk_c_step ~theories:["arith";"equality"]
      ~rule:"arith_elim" cc [c.C.hcproof] in
    let new_c = C.create ~parents:[c] ~ctx lits' proof in
    Util.debug 3 "eliminate %a in %a (with %a) into %a" Lit.pp c.C.hclits.(i)
      C.pp c Substs.pp subst C.pp new_c;
    new_c
  in
  (* try to eliminate every arith literals *)
  Lits.fold_lits ~eligible c.C.hclits []
    (fun acc lit i ->
      let ord_lits = Arith.Lit.extract ~signature lit in
      (* we can eliminate variables that are not shielded in other literals *)
      let elim_var x = not (Arith.Lits.shielded ~filter:(fun i' _ -> i <> i') c.C.hclits x) in
      let substs = Util.list_flatmap
        (Arith.Lit.eliminate ~elim_var ~signature) ord_lits
      in
      List.fold_left
        (fun acc subst -> eliminate_lit i subst :: acc)
        acc substs)

let factor_arith c =
  let ctx = c.C.hcctx in
  let ord = Ctx.ord ctx in
  let signature = Ctx.signature ctx in
  let eligible = C.Eligible.max c in
  (* instantiate the clause with subst *)
  let mk_instance subst =
    let renaming = Ctx.renaming_clear ~ctx in
    let lits' = Lits.apply_subst ~ord ~renaming subst c.C.hclits 0 in
    let proof cc = Proof.mk_c_step ~theories:["arith";"equality"]
      ~rule:"factor" cc [c.C.hcproof] in
    let new_c = C.create_a ~parents:[c] ~ctx lits' proof in
    Util.debug 3 "factor %a (with %a) into %a" C.pp c Substs.pp subst C.pp new_c;
    new_c
  in
  (* try to factor arith literals *)
  Lits.fold_lits ~eligible c.C.hclits []
    (fun acc lit i ->
      let ord_lits = Arith.Lit.extract ~signature lit in
      let substs = Util.list_flatmap Arith.Lit.factor ord_lits in
      List.fold_left
        (fun acc subst -> mk_instance subst :: acc)
        acc substs)

let pivot_arith c =
  let ctx = c.C.hcctx in
  let eligible = C.Eligible.max c in
  let lits'_list = Arith.Lits.pivot ~ord:(Ctx.ord ctx)
    ~signature:(Ctx.signature ctx) ~eligible c.C.hclits
  in
  Util.list_fmap
    (fun lits' ->
      if Lits.eq_com c.C.hclits lits'
        then None
        else begin
          (* build new clause *)
          let proof cc = Proof.mk_c_step ~theories:["equality";"arith"]
            ~rule:"arith_pivot" cc [c.C.hcproof] in
          let new_c = C.create_a ~parents:[c] ~ctx lits' proof in
          Util.debug 3 "arith_pivot of %a: %a" C.pp c C.pp new_c;
          Some new_c
        end)
    lits'_list

let purify_arith c =
  let ctx = c.C.hcctx in
  let eligible = C.Eligible.(max c) in
  let lits' = Arith.Lits.purify ~ord:(Ctx.ord ctx)
    ~signature:(Ctx.signature ctx) ~eligible c.C.hclits
  in
  if Lits.eq_com c.C.hclits lits'
    then []
    else begin
      let proof cc = Proof.mk_c_step ~rule:"purify" cc [c.C.hcproof] in
      let new_c = C.create_a ~ctx ~parents:[c] lits' proof in
      [new_c]
    end

let axioms =
  (* parse a pformula *)
  let pform ~name s =
    let f = Parse_tptp.parse_formula Lex_tptp.token (Lexing.from_string s) in
    let proof = Proof.mk_f_axiom f ~file:"/dev/arith" ~name in
    let pf = PF.create f proof in
    pf
  in
  [ pform ~name:"sum_assoc" "![X:A,Y:A,Z:A]: $sum($sum(X,Y),Z) = $sum(X,$sum(Y,Z))"
  ; pform ~name:"sum_com" "![X:A,Y:A]: $sum(X,Y) = $sum(Y,X)"
  ; pform ~name:"product_assoc"
    "![X:A,Y:A,Z:A]: $product($product(X,Y),Z) = $product(X,$product(Y,Z))"
  ; pform ~name:"product_com" "![X:A,Y:A]: $product(X,Y) = $product(Y,X)"
  ]

(** {2 Setup} *)

let setup_penv ~penv =
  (* rule for formula simplification *)
  let simplify_rule set =
    let signature = PF.Set.signature set in
    fun set pf ->
      let f' = Arith.F.simplify ~signature pf.PF.form in
      if F.eq pf.PF.form f'
        then PEnv.DoNothing
        else
          let proof = Proof.mk_f_step f' ~rule:"arith_simplify" [pf.PF.proof] in
          let pf' = PF.create f' proof in
          PEnv.SimplifyInto pf'
  in
  (* signature of arith symbols *)
  let base = Signature.Arith.signature in
  PEnv.add_base_sig ~penv base;
  PEnv.add_axioms ~penv (Sequence.of_list axioms);
  PEnv.add_operation_rule ~penv ~prio:2 simplify_rule;
  PEnv.add_constr ~penv (Precedence.min_constraint (Signature.to_symbols base));
  ()

let setup_env ~env =
  Env.add_lit_rule ~env "arith_rw" rewrite_lit;
  Env.add_unary_inf ~env "arith_factor" factor_arith;
  Env.add_unary_inf ~env "arith_pivot" pivot_arith;
  Env.add_unary_inf ~env "arith_purify" purify_arith;
  Env.add_unary_inf ~env "arith_elim" eliminate_arith;
  (* be sure that the ordering is present in the context *)
  Ctx.add_order ~ctx:(Env.ctx env) ~less:S.Arith.less ~lesseq:S.Arith.lesseq;
  (* we are (until proved otherwise) incomplete *)
  Ctx.lost_completeness ~ctx:(Env.ctx env);
  ()

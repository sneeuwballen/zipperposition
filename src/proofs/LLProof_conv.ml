(* This file is free software, part of Zipperposition. See file "license" for more details. *)

(** {1 Convert Proof.t to LLProof.t — Clause-Based} *)

open Logtk

let section = LLProof.section
let errorf msg = Util.errorf ~where:"llproof_conv" msg

(** Build the full instantiation for a parent, like the encoder's [emit_subst].
    For each variable of the parent clause, apply the renaming and substitution
    to get the image, and include non-identity mappings. *)
let full_inst (proj : Subst.Projection.t) (clause : Literal.t array) :
    LLProof.inst =
  let scope = Subst.Projection.scope proj in
  let renaming = Subst.Projection.renaming proj in
  let subst = Subst.Projection.subst proj in
  (* Collect all variables from the parent clause *)
  let parent_vars = Literals.vars clause in
  List.filter_map
    (fun v ->
      let t = Subst.FO.apply renaming subst (Term.var v, scope) in
      match Term.view t with
      | Term.Var v2 when HVar.equal Type.equal v v2 -> None
      | _ -> Some (v, t))
    parent_vars

let rec conv_proof tbl get_clause (p : Proof.t) : LLProof.t =
  match Proof.S.Tbl.get tbl p with
  | Some r -> r
  | None ->
    let res = conv_step tbl get_clause p in
    Proof.S.Tbl.add tbl p res;
    res

and conv_step tbl get_clause p =
  Util.debugf ~section 5 "(@[llproof.conv.step@ %a@])" (fun k ->
      k Proof.S.pp_notrec1 p);
  let step = Proof.S.step p in
  let kind = Proof.Step.kind step in
  let concl = get_clause p in
  match kind with
  | Proof.Inference (rule, tags) | Proof.Simplification (rule, tags) ->
    let parents =
      List.map (conv_parent tbl get_clause) (Proof.Step.parents step)
    in
    LLProof.inference ~tags concl (Proof.Rule.name rule) parents
  | Proof.Esa _rule ->
    let parents =
      List.map
        (function
          | Proof.P_of p -> LLProof.p_of (conv_proof tbl get_clause p)
          | Proof.P_subst (p, _subst) ->
            (* ESA steps don't carry instantiations in LLProof;
               drop the substitution and keep the proof reference *)
            LLProof.p_of (conv_proof tbl get_clause p))
        (Proof.Step.parents step)
    in
    let ps = List.map (fun par -> par.LLProof.p_proof) parents in
    let rule_name =
      match Proof.Step.rule step with
      | Some r -> Proof.Rule.name r
      | None -> "esa"
    in
    LLProof.mk_ concl (LLProof.Esa (rule_name, ps))
  | Proof.Trivial -> LLProof.trivial concl
  | Proof.By_def id -> LLProof.by_def id concl
  | Proof.Define (id, _) -> LLProof.define id concl
  | Proof.Intro (_, Proof.R_assert) -> LLProof.assert_ concl
  | Proof.Intro (_, (Proof.R_goal | Proof.R_lemma)) -> LLProof.goal concl
  | Proof.Intro (_, (Proof.R_def | Proof.R_decl)) -> LLProof.trivial concl

and conv_parent tbl get_clause (parent : Proof.Parent.t) : LLProof.parent =
  match parent with
  | Proof.P_of p -> LLProof.p_of (conv_proof tbl get_clause p)
  | Proof.P_subst (p, subst) ->
    let p_llproof = conv_proof tbl get_clause p in
    let parent_clause = get_clause p in
    let inst = full_inst subst parent_clause in
    LLProof.p_inst p_llproof inst

let conv ~get_clause (p : Proof.t) : LLProof.t =
  let@ _sp = Trace.with_span ~__FILE__ ~__LINE__ "llprover.proof.conv" in
  let tbl = Proof.S.Tbl.create 32 in
  conv_proof tbl get_clause p

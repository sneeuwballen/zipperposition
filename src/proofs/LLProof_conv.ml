
(* This file is free software, part of Zipperposition. See file "license" for more details. *)

open Logtk

module T = TypedSTerm

type ll_subst = (T.t,T.t) Var.Subst.t

let section = LLProof.section
let errorf msg = Util.errorf ~where:"llproof_conv" msg

let conv_subst ~ctx (p:Subst.Projection.t) : ll_subst =
  List.fold_left
    (fun lsubst (v,t) ->
       let v = Type.cast_var_unsafe v in
       let prefix = if Type.is_tType (HVar.ty v) then "A" else "X" in
       let v = Term.Conv.var_to_simple_var ~prefix ctx v in
       let t = Term.Conv.to_simple_term ctx (Term.of_term_unsafe t) in
       assert (not (Var.Subst.mem lsubst v));
       Var.Subst.add lsubst v t)
    Var.Subst.empty
    (Subst.Projection.bindings p)

type state = {
  ctx: Term.Conv.ctx;
  tbl: LLProof.t Proof.S.Tbl.t;
}

let open_forall = T.unfold_binder Binder.Forall

let rec conv_proof st p: LLProof.t =
  begin match Proof.S.Tbl.get st.tbl p with
    | Some r -> r
    | None ->
      let res = conv_step st p in
      Proof.S.Tbl.add st.tbl p res;
      res
  end

and conv_step st p =
  Util.debugf ~section 5 "(@[llproof.conv_step@ %a@])"
    (fun k->k Proof.S.pp_notrec1 p);
  let res = Proof.Result.to_form ~ctx:st.ctx (Proof.S.result p) in
  let vars, _ = open_forall res in
  let intros =
    List.mapi (fun i v -> T.const ~ty:(Var.ty v) (ID.makef "sk_%d" i)) vars
  in
  let res = match Proof.Step.kind @@ Proof.S.step p with
    | Proof.Inference (rule,c,tags)
    | Proof.Simplification (rule,c,tags) ->
      let parents =
        List.map (conv_parent st p res intros) (Proof.Step.parents @@ Proof.S.step p)
      in
      LLProof.inference c ~intros ~tags
        (T.rename_all_vars res) (Proof.Rule.name rule) parents
    | Proof.Esa (rule,c) ->
      let l =
        List.map
          (function
            | Proof.P_of p -> conv_proof st p
            | Proof.P_subst _ -> assert false)
          (Proof.Step.parents @@ Proof.S.step p)
      in
      LLProof.esa c (T.rename_all_vars res) (Proof.Rule.name rule) l
    | Proof.Trivial -> LLProof.trivial res
    | Proof.By_def id -> LLProof.by_def id res
    | Proof.Define (id,_) -> LLProof.define id res
    | Proof.Intro (_,Proof.R_assert) -> LLProof.assert_ res
    | Proof.Intro (_,Proof.R_goal) -> LLProof.goal res
    | Proof.Intro (_,(Proof.R_lemma|Proof.R_def|Proof.R_decl)) ->
      LLProof.trivial res
  in
  Util.debugf ~section 4 "(@[llproof.conv_step.->@ :from %a@ :to %a@])"
    (fun k->k Proof.S.pp_notrec1 p LLProof.pp res);
  res

(* convert parent of the given result formula. Also make instantiations
   explicit. *)
and conv_parent st step res intros (parent:Proof.Parent.t): LLProof.parent =
  Util.debugf ~section 5 "(@[llproof.conv_parent@ %a@])"
    (fun k->k Proof.pp_parent parent);
  let prev_proof, p_instantiated_res = match parent with
    | Proof.P_of p ->
      let p_res = Proof.Result.to_form ~ctx:st.ctx (Proof.S.result p) in
      let p = conv_proof st p in
      p, p_res
    | Proof.P_subst (p,subst) ->
      (* perform instantiation *)
      assert (not (Subst.Projection.is_empty subst));
      (* instantiated result of [p] *)
      let p_instantiated_res, inst_subst =
        Proof.Result.to_form_subst ~ctx:st.ctx subst (Proof.S.result p)
      in
      let p_res = Proof.Result.to_form ~ctx:st.ctx (Proof.S.result p) in
      (* convert [p] itself *)
      let p = conv_proof st p in
      (* find instantiation for [p] *)
      let inst : LLProof.inst =
        let vars_p, _ = T.unfold_binder Binder.forall p_res in
        List.map
          (fun v ->
             begin match Var.Subst.find inst_subst v with
               | Some t -> t
               | None ->
                 errorf "cannot find variable `%a`@ in inst-subst %a"
                   Var.pp_fullc v (Var.Subst.pp T.pp) inst_subst
             end)
          vars_p
      in
      LLProof.instantiate p_instantiated_res p inst, p_instantiated_res
  in
  (* now open foralls in [p_instantiated_res]
     and find which variable of [intros] they rename into *)
  let inst_intros : LLProof.inst =
    (* rename variables of result of inference *)
    let vars_res, _ = T.unfold_binder Binder.forall res in
    if List.length intros <> List.length vars_res then (
      errorf "length mismatch, cannot do intros@ :res %a@ :with [@[%a@]]"
        T.pp res (Util.pp_list ~sep:"," T.pp) intros
    );
    let intro_subst =
      Var.Subst.of_list (List.combine vars_res intros)
    in
    let vars_instantiated, _ = T.unfold_binder Binder.forall p_instantiated_res in
    List.map
      (fun v ->
         begin match Var.Subst.find intro_subst v with
           | Some v2 -> v2
           | None ->
             errorf "(@[<hv2>cannot find intros-inst for `%a`@ \
                     :subst {%a}@ :form `%a`@ :res %a@ :parent %a@ :in-step %a@])"
               Var.pp v (Var.Subst.pp T.pp) intro_subst
               T.pp p_instantiated_res T.pp res
               Proof.pp_parent parent
               Proof.S.pp_notrec1 step
         end)
      vars_instantiated
  in
  LLProof.p_inst prev_proof inst_intros

let conv (p:Proof.t) : LLProof.t =
  let st = {
    ctx = Term.Conv.create();
    tbl = Proof.S.Tbl.create 32;
  } in
  conv_proof st p

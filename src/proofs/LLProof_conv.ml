
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
  Util.debugf ~section 5 "(@[llproof.conv.step@ %a@])"
    (fun k->k Proof.S.pp_notrec1 p);
  let res = Proof.Result.to_form ~ctx:st.ctx (Proof.S.result p) in
  let vars, _ = open_forall res in
  (* introduce local symbols for making proof checking locally ground.
     Some variables are typed using other variables, so we
     need to substitute eagerly *)
  let intros =
    let l =
      List.mapi (fun i v -> v, T.const ~ty:(Var.ty v) (ID.makef "sk_%d" i)) vars
    in
    let subst = Var.Subst.of_list l in
    List.map (fun (_,c) -> T.Subst.eval subst c) l
  in
  (* convert result *)
  let res = match Proof.Step.kind @@ Proof.S.step p with
    | Proof.Inference (rule,tags)
    | Proof.Simplification (rule,tags) ->
      let local_intros = ref Var.Subst.empty in
      let parents =
        List.map (conv_parent st res intros local_intros tags)
          (Proof.Step.parents @@ Proof.S.step p)
      in
      let local_intros = Var.Subst.to_list !local_intros |> List.rev_map snd in
      LLProof.inference ~intros ~local_intros ~tags
        (T.rename_all_vars res) (Proof.Rule.name rule) parents
    | Proof.Esa rule ->
      let l =
        List.map
          (function
            | Proof.P_of p -> conv_proof st p
            | Proof.P_subst _ -> assert false)
          (Proof.Step.parents @@ Proof.S.step p)
      in
      LLProof.esa (T.rename_all_vars res) (Proof.Rule.name rule) l
    | Proof.Trivial -> LLProof.trivial res
    | Proof.By_def id -> LLProof.by_def id res
    | Proof.Define (id,_) -> LLProof.define id res
    | Proof.Intro (_,Proof.R_assert) -> LLProof.assert_ res
    | Proof.Intro (_,(Proof.R_goal|Proof.R_lemma)) -> LLProof.goal res
    | Proof.Intro (_,(Proof.R_def|Proof.R_decl)) ->
      LLProof.trivial res
  in
  Util.debugf ~section 4 "(@[llproof.conv.step.->@ :from %a@ :to %a@])"
    (fun k->k Proof.S.pp_notrec1 p LLProof.pp res);
  res

(* convert parent of the given result formula. Also make instantiations
   explicit. *)
and conv_parent
    st step_res intros local_intros tags
    (parent:Proof.Parent.t) : LLProof.parent =
  Util.debugf ~section 5 "(@[llproof.conv_parent@ %a@])"
    (fun k->k Proof.pp_parent parent);
  (* rename variables of result of inference *)
  let vars_step_res, _ = open_forall step_res in
  if List.length intros <> List.length vars_step_res then (
    errorf "length mismatch, cannot do intros@ :res %a@ :with [@[%a@]]"
      T.pp step_res (Util.pp_list ~sep:"," T.pp) intros
  );
  let intro_subst =
    Var.Subst.of_list (List.combine vars_step_res intros)
  in
  (* build an instantiation step, if needed *)
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
      (* find instantiation for [p] by looking at variables of [p_res] *)
      let inst : LLProof.inst =
        let vars_p, _ = open_forall p_res in
        List.map
          (fun v ->
             begin match Var.Subst.find inst_subst v with
               | Some t -> t
               | None ->
                 errorf "cannot find variable `%a`@ \
                         in inst-subst {%a}@ :inst-res %a@ :res %a@ \
                         :parent %a@ :intros [@[%a@]]"
                   Var.pp_fullc v (Var.Subst.pp T.pp) inst_subst
                   T.pp p_instantiated_res T.pp step_res Proof.pp_parent parent
                   (Util.pp_list ~sep:"," T.pp) intros
             end)
          vars_p
      in
      LLProof.instantiate ~tags p_instantiated_res p inst, p_instantiated_res
  in
  (* now open foralls in [p_instantiated_res]
     and find which variable of [intros] they rename into *)
  let inst_intros : LLProof.inst =
    let vars_instantiated, _ = T.unfold_binder Binder.forall p_instantiated_res in
    List.map
      (fun v ->
         begin match Var.Subst.find intro_subst v with
           | Some t -> t
           | None ->
             begin match Var.Subst.find !local_intros v with
               | Some t -> t
               | None ->
                 (* introduce local_intro *)
                 let c =
                   ID.makef "sk_%d"
                     (List.length intros + Var.Subst.size !local_intros)
                   |> T.const ~ty:(Var.ty v |> T.Subst.eval intro_subst)
                 in
                 local_intros := Var.Subst.add !local_intros v c;
                 Util.debugf ~section 5
                   "(@[llproof.conv.add_local_intro@ %a := %a@ :p-instantiated `%a`@])"
                   (fun k->k Var.pp_fullc v T.pp c T.pp p_instantiated_res);
                 c
             end
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

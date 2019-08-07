
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

let axiom kind v t const = 
  let t1 = T.Form.forall v t in
  let t2 = T.Subst.eval (Var.Subst.singleton v const) t in
  let t = match kind with
    | `Instance -> T.Form.imply t1 t2 
    | `Choice -> T.Form.imply t2 t1 
  in
  LLProof.trivial t

let create_axioms kind s intro_subst = 
  T.Seq.subterms s
  |> Iter.filter_map (fun t -> match T.view t with
    | T.Bind((Binder.Forall|Binder.Exists as b), v, t) ->
      begin match Var.Subst.find intro_subst v with
        | Some sk -> 
          let t = if b = Binder.Exists then T.Form.not_ t else t in
          Some (LLProof.p_of (axiom kind v t sk))
        | None -> None
      end
    | _ -> None) 
  |> Iter.to_list 

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
  (* convert result *)
  let res = match Proof.Step.kind @@ Proof.S.step p with
    | Proof.Esa (rule,tags) when CCList.memq Builtin.Tag.T_conv tags ->
      (* Omit term conversion steps in LLProof *)
      let parents = Proof.Step.parents (Proof.S.step p) in
      assert (List.length parents == 1);
      conv_proof st (Proof.Parent.proof (List.hd parents))
    | Proof.Inference (rule,tags)
    | Proof.Simplification (rule,tags)
    | Proof.Esa (rule,tags) ->
      let is_cnf = CCList.memq Builtin.Tag.T_cnf tags in
      let vars, _ = open_forall res in
      (* introduce local symbols for making proof checking locally ground.
        Some variables are typed using other variables, so we
        need to substitute eagerly *)
      let l =
        List.mapi (fun i v -> v, T.const ~ty:(Var.ty v) (ID.makef "sk_%d" i)) vars
      in
      let intro_subst = Var.Subst.of_list l in
      let intros =
        if is_cnf
        then None
        else Some (List.map (fun (_,c) -> T.Subst.eval intro_subst c) l)
      in
      let local_intros = ref Var.Subst.empty in
      let parents =
        List.map (conv_parent st res intro_subst local_intros tags)
          (Proof.Step.parents @@ Proof.S.step p)
      in
      (* For CNF-transformations, add instance and choice axioms: *)
      let parents = parents @ 
        if is_cnf 
        then
          CCList.flat_map (fun p -> 
            create_axioms `Instance (LLProof.concl p.LLProof.p_proof) intro_subst) parents
          @ create_axioms `Choice res intro_subst
        else []
      in
      let local_intros = Var.Subst.to_list !local_intros |> List.rev_map snd in
      (* TODO: What is this for?  let res = T.rename_all_vars res in *)
      LLProof.inference ~intros ~local_intros ~tags
        res (Proof.Rule.name rule) parents
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
    st step_res intro_subst local_intros tags
    (parent:Proof.Parent.t) : LLProof.parent =
  Util.debugf ~section 5 "(@[llproof.conv_parent@ %a@])"
    (fun k->k Proof.pp_parent parent);
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
                         :parent %a@ :intro_subst [@[%a@]]"
                   Var.pp_fullc v (Var.Subst.pp T.pp) inst_subst
                   T.pp p_instantiated_res T.pp step_res Proof.pp_parent parent
                   (Var.Subst.pp T.pp) intro_subst
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
                     (Var.Subst.size intro_subst + Var.Subst.size !local_intros)
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

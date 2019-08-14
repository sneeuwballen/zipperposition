
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

let find_skolem subst v =     
  match Var.Subst.find !subst v with
    | Some sk -> 
        (* the skolem may have arguments that need to be instantiated: *)
        let sk = T.Subst.eval !subst sk in
        sk
    | None -> 
      (* introduce skolem *)
      let sk =
        ID.makef "sk_%d"
          (Var.Subst.size !subst) 
        |> T.const ~ty:(Var.ty v |> T.Subst.eval !subst)
      in
      let sk = T.Subst.eval !subst sk in
      subst := Var.Subst.add !subst v sk;
      sk

let create_axiom t kind v sk =
  (* Create choice/inst axiom *)
  assert (T.equal (T.ty_exn t) (T.prop));
  let t1 = match kind with
    | `Instance -> T.Form.forall v t 
    | `Choice -> T.Form.exists v t 
  in
  let t2 = T.Subst.eval ~rename_binders:false (Var.Subst.singleton v sk) t in
  let t = T.Form.imply t1 t2 in
  assert (T.closed t);
  let proof = (LLProof.p_of (LLProof.trivial t)) in
  proof, t2

let iter_boolean_skeleton t ~on_exists ~on_forall (k : 'a -> unit) =
  let rec iter ~pos t =
    let n = if pos then (fun t -> t) else (fun t -> T.Form.not_ t) in
    match T.view t with 
      | Bind (Binder.Forall|Binder.Exists as b, v, t) when (b = Binder.Forall) = pos ->
        iter ~pos (n (on_forall v (n t) k))
      | Bind (Binder.Forall|Binder.Exists as b, v, t) when (b = Binder.Exists) = pos -> 
        iter ~pos (n (on_exists v (n t) k))
      | AppBuiltin (Builtin.And,l)
      | AppBuiltin (Builtin.Or,l) -> List.iter (iter ~pos) l
      | AppBuiltin (Builtin.Not,l) -> List.iter (iter ~pos:(not pos)) l
      | AppBuiltin (Builtin.Imply,[a;b]) -> iter ~pos:(not pos) a; iter ~pos b
      | AppBuiltin (Builtin.Eq,l) 
      | AppBuiltin (Builtin.Equiv,l) 
      | AppBuiltin (Builtin.Xor,l)
      | AppBuiltin (Builtin.Neq,l) -> List.iter (iter ~pos) l; List.iter (iter ~pos:(not pos)) l
      | _ -> ()
  in
  iter ~pos:true t

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
  (* introduce local symbols for making proof checking locally ground.
        Some variables are typed using other variables, so we
        need to substitute eagerly *)
  let mk_inference rule tags ~intros add_parents =
    let intro_list = 
      let vars, _ = open_forall res in
      List.mapi (fun i v -> v, T.const ~ty:(Var.ty v) (ID.makef "sk_%d" i)) vars 
    in
    let intro_subst = Var.Subst.of_list intro_list 
    in
    let local_intros = ref Var.Subst.empty in
    let parents =
      List.map (conv_parent st res intro_subst ~intros local_intros tags)
        (Proof.Step.parents @@ Proof.S.step p)
    in
    let parents = parents @ add_parents parents in
    let local_intros = ref Var.Subst.empty in (* TODO: remove *)
    let local_intros = Var.Subst.to_list !local_intros |> List.rev_map snd in
    let intros = if intros 
      then Some (List.map (fun (_,c) -> T.Subst.eval intro_subst c) intro_list)
      else None 
    in
    (* TODO: What is this for?  let res = T.rename_all_vars res in *)
    LLProof.inference ~intros ~local_intros ~tags
      res (Proof.Rule.name rule) parents
  in
  (* convert result *)
  let res = match Proof.Step.kind @@ Proof.S.step p with
    | Proof.Esa (rule,tags,skolems) when CCList.memq Builtin.Tag.T_conv tags || CCList.memq Builtin.Tag.T_neg tags ->
      let add_parents _ = [] in
      mk_inference rule tags ~intros:false add_parents
    | Proof.Esa (rule,tags,skolems) ->
      (* For CNF-transformations, add instance and choice axioms: *)
      let add_parents parents =
        let subst = skolems |> 
          List.fold_left (fun subst ((sk, ty), (var, args)) -> 
            Var.Subst.add subst var (T.app_infer (T.const ~ty sk) args)
          ) Var.Subst.empty
          |> ref
        in
        let from_concl = (iter_boolean_skeleton (T.Form.not_ res)
          ~on_forall:(fun _ _ _ -> assert false)
          ~on_exists:(fun v t k -> let ax, t' = create_axiom t `Choice v (find_skolem subst v) in k ax; t') 
          |> Iter.to_list) in
        let from_premises = CCList.flat_map (fun p -> 
          (iter_boolean_skeleton (LLProof.concl p.LLProof.p_proof)
            ~on_forall:(fun v t k -> let ax, t' = create_axiom t `Instance v (find_skolem subst v) in k ax; t') 
            ~on_exists:(fun v t k -> let ax, t' = create_axiom t `Choice v (find_skolem subst v) in k ax; t') 
          |> Iter.to_list)) parents in
        from_concl @ from_premises
      in
      mk_inference rule tags ~intros:false add_parents
    | Proof.Inference (rule,tags)
    | Proof.Simplification (rule,tags) ->
      let add_parents _ = [] in
      mk_inference rule tags ~intros:true add_parents
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
    st step_res intro_subst ~intros local_intros tags
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
    if intros
    then
      let vars_instantiated, _ = T.unfold_binder Binder.forall p_instantiated_res in
      let l = List.map
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
      l
      else []
  in
  LLProof.p_inst prev_proof inst_intros (*TODO: remove lacal_intros completely *)

let conv (p:Proof.t) : LLProof.t =
  let st = {
    ctx = Term.Conv.create();
    tbl = Proof.S.Tbl.create 32;
  } in
  conv_proof st p

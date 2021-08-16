open Logtk
module T = Term

let section = Util.Section.make ~parent:Const.section "live_lifting"

let k_live_lifting = Flex_state.create_key ()
let k_post_cnf_lifting = Flex_state.create_key ()

module type S = sig
  module Env : Env.S
  module C : module type of Env.C

  (** {5 Registration} *)
  val setup : unit -> unit
  val lift_lambdas : Env.C.t -> Env.C.t list
end


module Make(E : Env.S) : S with module Env = E = struct
  module Env = E
  module C = Env.C
  module Ctx = Env.Ctx
  module Idx = Fingerprint.Make(struct 
    type t = C.t * T.t
    let (<?>) = (CCOrd.Infix.(<?>)) 
    let compare (a,b) (c,d) =
      (C.compare a c) <?> (T.compare, b, d)
   end)


  let clausify_def c : C.t list =
    match C.lits c with 
    | [| Literal.Equation(l,r,_) |] 
        when Type.is_prop (T.ty l) && not (T.equal l r) &&
             ((not (T.equal r T.true_) && not (T.equal r T.false_))
             || T.is_formula l || T.is_formula r) ->
      let f = Literals.Conv.to_tst (C.lits c) in
      let proof = Proof.Step.simp ~rule:(Proof.Rule.mk "clausify_def") ~tags:[Proof.Tag.T_ho] [C.proof_parent c] in
      let trail = C.trail c and penalty = C.penalty c in
      let stmt = Statement.assert_ ~proof f in
      let cnf_vec = Cnf.convert @@ CCVector.to_iter @@ Cnf.cnf_of ~ctx:(Ctx.sk_ctx ()) stmt in
      CCVector.iter (fun cl -> 
          Statement.Seq.ty_decls cl
          |> Iter.iter (fun (id,ty) -> 
            Ctx.declare id ty; 
            ID.set_payload id (ID.Attr_skolem ID.K_after_cnf)
          )) cnf_vec;

      CCVector.map (C.of_statement ~convert_defs:true) cnf_vec
      |> CCVector.to_list 
      |> CCList.flatten
      |> List.map (fun c -> 
          C.create ~penalty  ~trail (CCArray.to_list (C.lits c)) proof)
    | _ -> [c]

  let setup () = ()
  
  let sc t = (t, 0)

  let idx = ref (Idx.empty ())

  let loose_bound_to_fvars ~counter t =
    let rec aux ~depth ~lb_map ~subst t =
      match T.view t with
      | DB i ->
        if i < depth then t, lb_map, subst
        else (
          let i = i - depth in
          match Util.Int_map.get i lb_map  with 
          | Some var -> 
            assert(not @@ Scoped.equal T.equal (sc var) (Subst.FO.deref subst (sc var)));
            var, lb_map, subst
          | None  ->
            let fvar = HVar.fresh_cnt ~counter ~ty:(T.ty t) () in
            let t' = T.var fvar in
            let lb_map = Util.Int_map.add i t' lb_map in 
            let unshifted = T.bvar ~ty:(T.ty t) i in
            let subst = Subst.FO.bind' subst (sc fvar) (sc unshifted) in
            t', lb_map, subst
        )
      | Const _ | Var _ -> t, lb_map, subst
      | T.Fun _ ->
        let prefs, body = T.open_fun t in
        let depth_inc = List.length prefs in
        let body', lb_map, subst = aux ~depth:(depth+depth_inc) ~lb_map ~subst body in
        T.fun_l prefs body', lb_map, subst
      | AppBuiltin(b, l) -> 
        let l', lb_map, subst = aux_l ~depth ~lb_map ~subst l in
        T.app_builtin ~ty:(T.ty t) b l', lb_map, subst
      | App(hd, l ) ->
        let hd', lb_map, subst = aux ~depth ~lb_map ~subst hd in
        let l', lb_map, subst = aux_l ~depth ~lb_map ~subst l in
        T.app hd' l', lb_map, subst
    and aux_l ~depth ~lb_map ~subst = function 
      | [] -> [], lb_map, subst
      | x :: xs -> 
        let x', lb_map, subst = aux ~depth ~lb_map ~subst x in
        let xs', lb_map, subst = aux_l ~depth ~lb_map ~subst xs in
        x'::xs', lb_map, subst in
    aux t ~depth:0 ~lb_map:Util.Int_map.empty ~subst:Subst.empty
    

let fully_apply ~counter s t  =
  assert(Type.equal (T.ty s) (T.ty t));
  if not (Type.is_fun (T.ty t)) then  (s,t)
  else (
    let tys,_ = Type.open_fun (T.ty t) in
    let fresh_vars = List.map (fun ty -> T.var (HVar.fresh_cnt ~counter ~ty ())) tys in
    Lambda.whnf @@ T.app s fresh_vars, Lambda.whnf @@ T.app t fresh_vars
  )

let lift_lambdas_t ~parent ~counter t  =
  let rec aux t =
    match T.view t with
    | T.Fun _ ->
      Util.debugf ~section 1 "before lifting:@[%a@]@." (fun k -> k T.pp t);
      let pref_vars, body = T.open_fun t in
      let body', (reused_defs,new_defs), declared_syms = aux body in
      let body_closed = T.fun_l pref_vars body' in
      Util.debugf ~section 1 "after lifting:@[%a@]@." (fun k -> k T.pp body_closed);
      let generalization = 
        Idx.retrieve_generalizations (!idx, 1) (body_closed, 0)
        |> Iter.head in
      begin match generalization with 
      | Some (_,(def, lhs),subst) ->
        let lits = C.lits def in
        assert(Array.length lits = 1);
        let lhs' = Lambda.eta_reduce @@ Lambda.snf @@ Subst.FO.apply Subst.Renaming.none subst (lhs, 1) in
        lhs', (def :: reused_defs, new_defs), declared_syms
      | None -> 
        let free_vars = T.vars body' in
        let unbound, _, subst = loose_bound_to_fvars ~counter body_closed in
        let new_free_vars = T.VarSet.diff (T.vars unbound) (free_vars) in
        let all_vars = T.VarSet.to_list free_vars @ T.VarSet.to_list new_free_vars in
        let (id, ty), new_ll_sym = 
          T.mk_fresh_skolem ~prefix:"l_lift" all_vars (T.ty t) in
        let lhs = new_ll_sym and rhs = unbound in
        let lhs_applied, rhs_applied = fully_apply ~counter lhs rhs in
        let lits = [Literal.mk_eq lhs_applied rhs_applied] in
        let proof = Proof.Step.define_internal id [C.proof_parent parent] in
        let lift_def = C.create ~penalty:1 ~trail:Trail.empty lits proof in
        let repl = Subst.FO.apply Subst.Renaming.none subst (sc lhs) in

        let new_def =
          Idx.retrieve_specializations (!idx, 1) (rhs, 0)
          |> Iter.fold (fun acc (_,(def,lhs_spec),subst) -> 
            let subst_has_lams subst = 
              Subst.codomain subst
              |> Iter.exists (fun (t,_) -> 
                  Iter.exists T.is_fun (T.Seq.subterms (T.of_term_unsafe t))) in

            if not (subst_has_lams subst) then (
              let renaming = Subst.Renaming.create () in
              let lhs, rhs = 
                Subst.FO.apply renaming subst (lhs, 0), Subst.FO.apply renaming subst (lhs_spec, 1) in
              let lits = [Literal.mk_eq lhs rhs] in
              let proof = Proof.Step.define_internal id [C.proof_parent parent; C.proof_parent def] in
              let lift_rel = C.create ~penalty:1 ~trail:Trail.empty lits proof in
              lift_rel :: acc)
            else acc
          ) [lift_def] in
        

        idx := Idx.add !idx rhs (lift_def, lhs);
        repl, (reused_defs, (new_def @ new_defs)), ((id,ty) :: declared_syms)
        end
    | T.Var _ | T.Const _  | T.DB _ -> t, ([],[]), []
    | T.App(hd, l) -> 
      assert(not (T.is_fun hd));
      let l', new_defs, declared_syms = aux_l l in
      (if T.same_l l l' then t else (T.app hd l')), new_defs, declared_syms
    | T.AppBuiltin((Builtin.ExistsConst|Builtin.ForallConst) as b, [_;q_body])
        when T.is_fun q_body ->
      let vars, body = T.open_fun q_body in
      let body', new_defs, declared_syms = aux body in
      let mk_q b = 
        if Builtin.equal b ExistsConst then T.Form.exists else T.Form.forall in
      (if T.equal body body' then t else mk_q b (T.fun_l vars body')), 
        new_defs, declared_syms
    | T.AppBuiltin(b, l) ->
      let l', new_defs, declared_syms = aux_l l in
      (if T.same_l l l' then t else T.app_builtin ~ty:(T.ty t) b l'), 
        new_defs, declared_syms
  and aux_l = function 
    | [] -> ([], ([],[]), [])
    | x :: xs ->
      let x', (x_reused_defs, x_new_defs), declared_syms = aux x in
      let xs', (xs_reused_defs, xs_new_defs), declared_symss = aux_l xs in
      x' :: xs', (x_reused_defs @ xs_reused_defs, x_new_defs @ xs_new_defs), declared_syms @ declared_symss in
  Util.debugf ~section 1 "lifting @[%a@]@." (fun k -> k T.pp t);
  let res, defs, declared_syms =  aux (Lambda.snf @@ t) in
  Ctx.declare_syms declared_syms;
  (res,defs)

  let lift_lambdas cl =
    let counter = 
      ref ((C.Seq.vars cl |> Iter.map HVar.id 
            |> Iter.max |> CCOpt.get_or ~default:0) + 1) in
    let lits, (reused_defs, new_defs) = 
      C.lits cl
      |> CCArray.to_list
      |> (fun l -> List.fold_right (fun lit (acc, (reused_defs, new_defs)) -> 
          match lit with 
          | Literal.Equation(lhs, rhs, sign) ->
            let lhs', (reused_lhs, new_lhs) = lift_lambdas_t ~parent:cl ~counter lhs in
            let rhs', (reused_rhs, new_rhs) = lift_lambdas_t ~parent:cl ~counter rhs in
            let reused = reused_lhs @ reused_rhs and new_ = new_rhs @ new_lhs in
            let lit' = Literal.mk_lit lhs' rhs' sign in
            lit'::acc, (reused@reused_defs, new_@new_defs)
          | _ -> lit::acc, (reused_defs, new_defs)
       ) l ([],([],[]))) in

    if Literals.equal (Array.of_list lits) (C.lits cl)  then []
    else (
      let proof = 
        Proof.Step.simp 
          ~tags:[Proof.Tag.T_ho] ~rule:(Proof.Rule.mk "lambda_lifting")
          (List.map C.proof_parent (cl :: reused_defs @ new_defs)) in
      let lifted =
        C.create ~penalty:(C.penalty cl) ~trail:(C.trail cl) lits proof in
      
      lifted :: new_defs
  )

  let lift_lambdas_simp cl =
    let res = lift_lambdas cl in
    if CCList.is_empty res then (
      Util.debugf ~section 1 "Nothing to do for @[%a@]@." (fun k -> k C.pp cl);
      None)
    else (
      Util.debugf ~section 1 "lifting(@[%a@])@. = @[%a@]" (fun k -> k C.pp cl (CCList.pp C.pp) res);
      Some (CCList.flat_map clausify_def res))

  
  let lift_lambdas_cnf st =
    Env.cr_return @@ CCList.flat_map (fun c -> 
      CCOpt.get_or ~default:[c] (lift_lambdas_simp c)
    )(E.C.of_statement st)

  let setup () =
    if Env.flex_get k_live_lifting then (
      Env.add_multi_simpl_rule ~priority:5 lift_lambdas_simp
    );
    if Env.flex_get k_post_cnf_lifting then (
      Env.add_clause_conversion lift_lambdas_cnf
    );
end

let _live_lifting = ref false
let _post_cnf_lifting = ref false

let extension =
  let register env =
    let module E = (val env : Env.S) in
    let module ET = Make(E) in

    E.flex_add k_live_lifting !_live_lifting;
    E.flex_add k_post_cnf_lifting !_post_cnf_lifting;
    ET.setup ()
  in
  { Extensions.default with
    Extensions.name = "lift_lambdas";
    prio=1;
    env_actions=[register];
  }

let () =
  Options.add_opts
    [ "--live-lambda-lifting", Arg.Bool ((:=) _live_lifting), 
      " enable/disable lambda lifting as simplifying inference";
      "--post-cnf-lambda-lifting", Arg.Bool ((:=) _post_cnf_lifting),
      "enable/disable post-cnf lambda lifting";
    ];
  Params.add_to_modes ["ho-complete-basic";
                       "ho-pragmatic";
                       "lambda-free-intensional";
                       "lambda-free-purify-intensional";
                       "lambda-free-extensional";
                       "ho-comb-complete";
                       "lambda-free-purify-extensional";
                       "fo-complete-basic"] (fun () ->
      _live_lifting := false
  );

  Extensions.register extension;

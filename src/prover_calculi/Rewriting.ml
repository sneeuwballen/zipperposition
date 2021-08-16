
(* This file is free software, part of Zipperposition. See file "license" for more details. *)

(** {1 Rewriting} *)

open Logtk
open Libzipperposition

module T = Term
module RW = Rewrite
module P = Position

let section = RW.section

let stat_narrowing_lit = Util.mk_stat "narrow.lit_steps"
let stat_narrowing_term = Util.mk_stat "narrow.term_steps"
let stat_ctx_narrowing = Util.mk_stat "narrow.ctx_narrow_steps"

let prof_narrowing_term = ZProf.make "narrow.term"
let prof_narrowing_lit = ZProf.make "narrow.lit"
let prof_ctx_narrowing = ZProf.make "narrow.ctx_narrow"

let max_steps = 500
let rewrite_before_cnf = ref false


module Key = struct
  let has_rw = Flex_state.create_key()
  let ctx_narrow = Flex_state.create_key()
  let narrow = Flex_state.create_key()
end

let simpl_term t =
  let t', rules = RW.Term.normalize_term ~max_steps t in
  if T.equal t t' then (
    assert (RW.Term.Rule_inst_set.is_empty rules);
    None
  ) else (
    let proof =
      RW.Rule.set_as_proof_parents rules
    in

    Util.debugf ~section 3
      "@[<2>@{<green> simpl rewrite@} `@[%a@]`@ :into `@[%a@]`@ :using %a@]"
      (fun k->k T.pp t T.pp t' RW.Term.Rule_inst_set.pp rules);
    Some (t',proof)
  )

module Make(E : Env_intf.S) = struct
  module Env = E
  module C = E.C

  (* simplification rule *)
  (* perform term narrowing in [c] *)
  let narrow_term_passive_ c: C.t list =
    let eligible = C.Eligible.(res c) in
    let sc_rule = 1 in
    let sc_c = 0 in
    Literals.fold_terms ~vars:false ~subterms:true ~ty_args:false ~ord:(C.Ctx.ord())
      ~which:`All ~eligible (C.lits c)
    |> Iter.flat_map
      (fun (u_p, passive_pos) ->
         RW.Term.narrow_term ~scope_rules:sc_rule (u_p,sc_c)
         |> Iter.filter_map
           (fun (rule,us) ->
              let renaming = Subst.Renaming.create() in
              let subst = Unif_subst.subst us in
              let c_guard = Literal.of_unif_subst renaming us in
              (* side literals *)
              let lits_passive = C.lits c in
              let lits' =
                Literals.apply_subst renaming subst (lits_passive,sc_c) in
              (* substitute in rule *)
              let rhs =
                Subst.FO.apply renaming subst (RW.Term.Rule.rhs rule, sc_rule)
              in
              (* literal in which narrowing took place: replace lhs by rhs *)
              Literals.Pos.replace lits' ~at:passive_pos ~by:rhs;
              (* make new clause *)
              Util.incr_stat stat_narrowing_term;
              let proof =
                Proof.Step.inference
                  [C.proof_parent_subst renaming (c,sc_c) subst;
                   Proof.Parent.from_subst renaming
                     (RW.Rule.as_proof (RW.T_rule rule),sc_rule) subst]
                  ~rule:(Proof.Rule.mk "narrow") in
              let c' =
                C.create ~trail:(C.trail c) ~penalty:(C.penalty c)
                  (c_guard @ CCArray.to_list lits') proof
              in
              
              Util.debugf ~section 3
                "@[<2>term narrowing:@ from `@[%a@]`@ to `@[%a@]`@ \
                 using rule `%a`@ and subst @[%a@]@]"
                (fun k->k C.pp c C.pp c' RW.Term.Rule.pp rule Unif_subst.pp us);
              Some c'
           )
      )
    |> Iter.to_rev_list

  let narrow_term_passive = ZProf.with_prof prof_narrowing_term narrow_term_passive_

  (* XXX: for now, we only do one step, and let Env.multi_simplify
     manage the fixpoint *)
  let simpl_clause c =
    let lits = C.lits c in
    match RW.Lit.normalize_clause lits with
    | None -> None
    | Some (clauses,r,subst,sc_r,renaming,tags) ->
      let proof =
        Proof.Step.simp ~rule:(Proof.Rule.mk "rw_clause") ~tags
          [C.proof_parent_subst renaming (c,0) subst;
           RW.Rule.lit_as_proof_parent_subst renaming subst (r,sc_r)]
      in
      let clauses =
        List.map
          (fun c' -> C.create_a ~trail:(C.trail c) ~penalty:(C.penalty c) c' proof)
          clauses
      in
      Util.debugf ~section 2
        "@[<2>@{<green>rewrite@} `@[%a@]`@ into `@[<v>%a@]`@]"
        (fun k->k C.pp c (Util.pp_list C.pp) clauses);
      Some clauses

  (* narrowing on literals of given clause, using lits rewrite rules *)
  let narrow_lits_ c =
    let eligible = C.Eligible.res c in
    let lits = C.lits c in
    Literals.fold_lits ~eligible lits
    |> Iter.fold
      (fun acc (lit,i) ->
         RW.Lit.narrow_lit ~scope_rules:1 (lit,0)
         |> Iter.fold
           (fun acc (rule,us,tags) ->
              let subst = Unif_subst.subst us in
              let renaming = Subst.Renaming.create () in
              let c_guard = Literal.of_unif_subst renaming us in
              let proof =
                Proof.Step.inference
                  [C.proof_parent_subst renaming (c,0) subst;
                   Proof.Parent.from_subst renaming
                     (RW.Rule.as_proof (RW.L_rule rule),1) subst]
                  ~rule:(Proof.Rule.mk "narrow_clause") ~tags in
              let lits' = CCArray.except_idx lits i in
              (* create new clauses that correspond to replacing [lit]
                 by [rule.rhs] *)
              let clauses =
                List.map
                  (fun c' ->
                     let new_lits =
                       c_guard
                       @ Literal.apply_subst_list renaming subst (lits',0)
                       @ Literal.apply_subst_list renaming subst (c',1)
                     in
                     C.create ~trail:(C.trail c) ~penalty:(C.penalty c) new_lits proof)
                  (RW.Lit.Rule.rhs rule)
              in
              Util.debugf ~section 3
                "@[<2>narrowing of `@[%a@]`@ using `@[%a@]`@ with @[%a@]@ yields @[%a@]@]"
                (fun k->k C.pp c RW.Lit.Rule.pp rule Unif_subst.pp us
                    CCFormat.(list (hovbox C.pp)) clauses);
              Util.incr_stat stat_narrowing_lit;
              List.rev_append clauses acc)
           acc)
      []

  let narrow_lits lits =
    ZProf.with_prof prof_narrowing_lit narrow_lits_ lits

  (* find positions in rules' LHS *)
  let ctx_narrow_find (s,sc_a) sc_p : (RW.Rule.t * Position.t * Unif_subst.t) Iter.t =
    let find_term (r:RW.Term.rule) =
      let t = RW.Term.Rule.lhs r in
      T.all_positions ~vars:false ~pos:P.stop ~ty_args:false t
      |> Iter.filter (fun (_,p) -> not (P.equal p P.stop)) (* not root *)
      |> Iter.filter
        (fun (t,_) -> match T.Classic.view t with
           | T.Classic.App (id,_) -> not (Ind_ty.is_constructor id)
           | T.Classic.Var _ | T.Classic.DB _
           | T.Classic.AppBuiltin (_,_) | T.Classic.NonFO -> false)
      |> Iter.filter_map
        (fun (t,p) ->
           try
             let subst = Unif.FO.unify_full (s,sc_a) (t,sc_p) in
             Some (RW.T_rule r, p, subst)
           with Unif.Fail -> None)
    and find_lit (r:RW.Lit.rule) =
      let lit = RW.Lit.Rule.lhs r in
      Literal.fold_terms lit
        ~position:P.stop ~vars:false ~ty_args:false
        ~which:`All ~ord:(E.Ctx.ord()) ~subterms:true
      |> Iter.filter_map
        (fun (t,p) -> match p with
           | P.Left P.Stop -> None (* not root *)
           | _ ->
             try
               let subst = Unif.FO.unify_full (s,sc_a) (t,sc_p) in
               Some (RW.L_rule r, p, subst)
             with Unif.Fail -> None)
    in
    Rewrite.all_rules
    |> Iter.flat_map
      (function
        | RW.T_rule r -> find_term r
        | RW.L_rule r -> find_lit r)

  (* do narrowing with [s=t], a literal in [c], and add results to [acc] *)
  let ctx_narrow_with ~ord s t s_pos c acc : C.t list =
    let sc_a = 1 and sc_p = 0 in
    (* do narrowing inside this rule? *)
    let do_narrowing rule rule_pos (us:Unif_subst.t) =
      let rule_clauses = match rule with
        | RW.T_rule r -> [ [| RW.Term.Rule.as_lit r |] ]
        | RW.L_rule r -> RW.Lit.Rule.as_clauses r
      in
      let renaming = Subst.Renaming.create() in
      let subst = Unif_subst.subst us in
      let c_guard = Literal.of_unif_subst renaming us in
      let s' = Subst.FO.apply renaming subst (s,sc_a) in
      let t' = Subst.FO.apply renaming subst (t,sc_a) in
      if Ordering.compare ord s' t' <> Comparison.Lt then (
        Util.incr_stat stat_ctx_narrowing;
        rule_clauses
        |> List.map
          (fun rule_clause ->
             (* instantiate rule and replace [s'] by [t'] now *)
             let new_lits =
               Literals.apply_subst renaming subst (rule_clause,sc_p)
               |> Literals.map (T.replace ~old:s' ~by:t')
               |> Array.to_list
             in
             (* also instantiate context literals in [c] *)
             let idx_active = match s_pos with
               | P.Arg (n,_) -> n | _ -> assert false
             in
             let ctx =
               Literal.apply_subst_list renaming subst
                 (CCArray.except_idx (C.lits c) idx_active, sc_a)
             in
             (* build new clause *)
             let proof =
               Proof.Step.inference
                 ~rule:(Proof.Rule.mk "contextual_narrowing")
                 [C.proof_parent_subst renaming (c,sc_a) subst;
                  Proof.Parent.from_subst renaming
                    (RW.Rule.as_proof rule,sc_p) subst]
             in
             (* add some penalty on every inference *)
             let penalty = Array.length (C.lits c) + C.penalty c in
             let new_c =
               C.create (c_guard @ new_lits @ ctx) proof
                 ~trail:(C.trail c) ~penalty
             in
             Util.debugf ~section 4
               "(@[<2>ctx_narrow@ :rule %a[%d]@ :clause %a[%d]@ :pos %a@ :subst %a@ :yield %a@])"
               (fun k->k RW.Rule.pp rule sc_p C.pp c sc_a P.pp rule_pos Subst.pp subst C.pp new_c);
             new_c)
        |> CCOpt.return
      ) else None
    in
    ctx_narrow_find (s,sc_a) sc_p
    |> Iter.fold
      (fun acc (rule,rule_pos,subst) ->
         match do_narrowing rule rule_pos subst with
         | None -> acc
         | Some cs -> cs @ acc)
      acc

  let contextual_narrowing_ c : C.t list =
    (* no literal can be eligible for paramodulation if some are selected.
       This checks if inferences with i-th literal are needed? *)
    let eligible = C.Eligible.param c in
    let ord = E.Ctx.ord() in
    (* do the inferences where clause is active; for this,
       we try to rewrite conditionally other clauses using
       non-minimal sides of every positive literal *)
    let new_clauses =
      Literals.fold_eqn ~sign:true ~ord ~both:true ~eligible (C.lits c)
      |> Iter.fold
        (fun acc (s, t, _, s_pos) ->
           (* rewrite clauses using s *)
           ctx_narrow_with ~ord s t s_pos c acc)
        []
    in
    new_clauses

  let contextual_narrowing c =
    ZProf.with_prof prof_ctx_narrowing contextual_narrowing_ c

  let setup ?(ctx_narrow=true) ~narrowing ~has_rw () =
    Util.debug ~section 1 "register Rewriting to Env...";
    E.add_rewrite_rule "rewrite_defs" simpl_term;
    if narrowing then (
      E.add_binary_inf "narrow_term_defs" narrow_term_passive;
      E.add_unary_inf "narrow_lit_defs" narrow_lits);
    if ctx_narrow then (
      E.add_binary_inf "ctx_narrow" contextual_narrowing;
    );
    if has_rw then  E.Ctx.lost_completeness ();
    E.add_multi_simpl_rule ~priority:5 simpl_clause;
    ()
end

let ctx_narrow_ = ref true
let narrowing = ref true

let post_cnf stmts st =
  CCVector.iter Statement.scan_stmt_for_defined_cst 
    (if not !rewrite_before_cnf then stmts
     else (
       CCVector.filter (fun st -> match Statement.view st with
           | Statement.Rewrite _ -> false
           | _ -> true) stmts));
  (* check if there are rewrite rules *)
  let has_rw =
    CCVector.to_iter stmts
    |> Iter.exists
      (fun st -> match Statement.view st with
         | Statement.Rewrite _
         | Statement.Def _ -> true
         | _ -> false)  in
  st
  |> Flex_state.add Key.has_rw has_rw

(* let post_typing stmts state = 
*)

let rewrite_tst_stmt stmt = 
  let aux f =
    let ctx = Type.Conv.create () in
    let t = Term.Conv.of_simple_term_exn ctx f in
    let snf = Lambda.snf in
    CCOpt.map (fun (t',p) ->  (Term.Conv.to_simple_term ctx (snf t'), p)) (simpl_term t) in

  let aux_l fs =
    let ts = List.map aux fs in
    if List.for_all CCOpt.is_none ts then None
    else (
      let proof = ref [] in
      let combined = CCList.combine fs ts in
      let res = 
        List.map (fun (f,res) -> 
            let f', p_list = CCOpt.get_or ~default:(f,[]) res in
            proof := p_list @ !proof;
            f') combined in
      Some (res, !proof)) in

  let mk_proof ~stmt_parents f_opt orig =
    CCOpt.map (fun (f', parent_list) -> 
        let rule = Proof.Rule.mk "definition expansion" in
        f', Proof.S.mk_f_simp ~rule orig (parent_list @ stmt_parents)) f_opt in

  let stmt_parents = [Proof.Parent.from @@ Statement.as_proof_i stmt] in
  match Statement.view stmt with
  | Assert f -> 
    (match mk_proof ~stmt_parents (aux f) f with
     | Some (f', proof) -> Statement.assert_ ~proof:(Proof.S.step proof) f'
     | None -> stmt)
  | Lemma fs -> 
    begin match aux_l fs with 
      | Some (fs', parents) ->
        let rule = Proof.Rule.mk "definition expansion" in
        let fs_parents = (List.map (fun f -> Proof.Parent.from (Proof.S.mk_f_esa ~rule f stmt_parents)) fs)
                         @ parents in
        let proof = Proof.Step.simp ~rule fs_parents in
        Statement.lemma ~proof fs'
      | None -> stmt end
  | Goal g ->
    (match mk_proof ~stmt_parents (aux g) g with
     | Some (g', proof) -> Statement.goal ~proof:(Proof.S.step proof) g'
     | None -> stmt)
  | NegatedGoal (skolems, ngs) -> 
    begin match aux_l ngs with 
      | Some (ng', parents) ->
        let rule = Proof.Rule.mk "definition expansion" in
        let ng_parents = (List.map (fun f -> Proof.Parent.from (Proof.S.mk_f_esa ~rule f stmt_parents)) ngs)
                         @ parents in
        let proof = Proof.Step.simp ~rule ng_parents in
        Statement.neg_goal ~skolems ~proof ng'
      | None -> stmt end
  | _ -> stmt

let unfold_def_before_cnf stmts =
  if !rewrite_before_cnf then (
    let cnt = ref 0 in
    CCVector.map (fun stmt ->
        incr cnt;
        try  
          rewrite_tst_stmt stmt
        with Type.Conv.Error t ->
          Util.debugf ~section 1 "@[%a@] cannot be converted because it uses unsupported features (such as ite, let and others)" 
            (fun k -> k Statement.pp_input stmt);
          stmt
      ) stmts
  ) else stmts

let post_tying stmts st =
  if !rewrite_before_cnf then (
    CCVector.iter Statement.scan_tst_rewrite stmts;
    let has_rw =
      CCVector.to_iter stmts
      |> Iter.exists
        (fun st -> match Statement.view st with
           | Statement.Rewrite _ -> true
           | _ -> false)  in
    Flex_state.add Key.has_rw has_rw st
  ) else st

(* add a term simplification that normalizes terms w.r.t the set of rules *)
let normalize_simpl (module E : Env_intf.S) =
  let module M = Make(E) in
  let has_rw = E.flex_get Key.has_rw in
  E.flex_add Key.ctx_narrow !ctx_narrow_;
  E.flex_add Key.narrow !narrowing;

  M.setup ~has_rw ~narrowing:!narrowing 
                  ~ctx_narrow:!ctx_narrow_ ()

let extension =
  let open Extensions in
  { default with
    name = "rewriting";
    post_typing_actions=[post_tying];
    post_cnf_actions=[post_cnf];
    env_actions=[normalize_simpl];
  }

let () = Options.add_opts
    [ "--rw-ctx-narrow", Arg.Set ctx_narrow_, " enable contextual narrowing";
      "--no-rw-ctx-narrow", Arg.Clear ctx_narrow_, " disable contextual narrowing";
      "--rewrite-before-cnf", Arg.Bool (fun v -> rewrite_before_cnf := v), " enable/disable rewriting before CNF"
    ];
    Params.add_to_modes 
    [ "ho-complete-basic"
    ; "ho-pragmatic"
    ; "ho-competitive"
    ; "fo-complete-basic"
    ; "lambda-free-intensional"
    ; "lambda-free-extensional"
    ; "ho-comb-complete"
    ; "lambda-free-purify-intensional"
    ; "lambda-free-purify-extensional"] 
    (fun () ->
      narrowing := false;
      ctx_narrow_ := false;
    );


(* This file is free software, part of Zipperposition. See file "license" for more details. *)

(** {1 boolean subterms} *)

open Logtk

module BV = CCBV
module T = Term
module RW = Rewrite

let section = Util.Section.make ~parent:Const.section "ho"

let stat_unif = Util.mk_stat "ho.unif.steps"
let stat_unif_syntastic = Util.mk_stat "ho.unif_syntactic.steps"
let stat_ext_neg = Util.mk_stat "ho.extensionality-.steps"
let stat_complete_eq = Util.mk_stat "ho.complete_eq.steps"
let stat_factor = Util.mk_stat "ho.factor.steps"

let prof_unif = Util.mk_profiler "ho.unif"
let prof_unif_syn = Util.mk_profiler "ho.unif_syntactic"

module type S = sig
  module Env : Env.S
  module C : module type of Env.C

  (** {6 Registration} *)

  val setup : unit -> unit
  (** Register rules in the environment *)
end

module Make(E : Env.S) : S with module Env = E = struct
  module Env = E
  module C = Env.C
  module Ctx = Env.Ctx

  let lit_is_unshielded_ho_unif (c:C.t)(i:int)(lit:Literal.t): bool = match lit with
    | Literal.HO_constraint (t, u) ->
      let other_lits = lazy (CCArray.except_idx (C.lits c) i |> Array.of_list) in
      begin match T.as_var (T.head_term t), T.as_var (T.head_term u) with
        | Some v, _ when not (Purify.is_shielded v (Lazy.force other_lits)) -> true
        | _, Some v when not (Purify.is_shielded v (Lazy.force other_lits)) -> true
        | _ -> false
      end
    | _ -> false

  (* flex-rigid (dis)equation? *)
  let is_flex_rigid t u =
    (T.is_var @@ T.head_term t) <> (T.is_var @@ T.head_term u)

  (* rigid/rigid constraint? *)
  let is_rigid_rigid t u =
    not (T.is_var (T.head_term t)) && not (T.is_var (T.head_term u))

  (* flex/flexconstraint? *)
  let is_flex_flex t u =
    T.is_var (T.head_term t) && T.is_var (T.head_term u)

  (* maximum number of consecutive steps in HO unif *)
  let max_unif_depth = 10

  type ho_constraint = T.t * T.t

  (* one possible solution for HO unif *)
  type unif_sol = {
    unif_lits: Literal.t list;
    unif_flex_rigid: ho_constraint list;
    unif_flex_flex: ho_constraint list;
    unif_rigid_rigid: ho_constraint list;
    unif_proof: Proof.Parent.t;
    unif_penalty: int;
    unif_depth: int; (* how many steps were done so far? *)
  }

  (* builder for {!unif_sol} *)
  let mk_unif ~lits ~cstr ~proof ~penalty ~depth : unif_sol =
    let unif_rigid_rigid, others =
      List.partition (CCFun.uncurry is_rigid_rigid) cstr in
    let unif_flex_rigid, others =
      List.partition (CCFun.uncurry is_flex_rigid) others in
    assert (List.for_all (CCFun.uncurry is_flex_flex) others);
    { unif_lits=lits;
      unif_rigid_rigid; unif_flex_rigid; unif_flex_flex=others;
      unif_proof=proof; unif_penalty=penalty; unif_depth=depth;
    }

  let unif_cstr (u:unif_sol): ho_constraint list =
    u.unif_flex_flex
    |> List.rev_append u.unif_flex_rigid
    |> List.rev_append u.unif_rigid_rigid

  (* TODO: use this for batch unification *)

  let ho_unif_lits
      (c:C.t)
      (lits:Literals.t)
      (cstr:ho_constraint list)
    : unif_sol list =
    (* queue of problems to process *)
    let q : unif_sol Queue.t = Queue.create() in
    let pb0 =
      mk_unif
        ~lits:(Array.to_list lits) ~cstr
        ~proof:(C.proof_parent c) ~penalty:(C.penalty c) ~depth:0
    in
    Queue.push pb0 q;
    (* final solutions *)
    let sols : unif_sol list ref = ref [] in
    while not (Queue.is_empty q) do
      let pb = Queue.pop q in
      if pb.unif_depth >= max_unif_depth then CCList.Ref.push sols pb
      else begin match pb.unif_rigid_rigid, pb.unif_flex_rigid with
        | [], [] -> CCList.Ref.push sols pb
        | (t,u)::tail, _ ->
          (* do syntactic unif *)
          assert (Type.equal (T.ty t) (T.ty u));
          let hd_t, args_t = T.as_app t in
          let hd_u, args_u = T.as_app u in
          assert (not (T.is_var hd_t) && not (T.is_var hd_u));
          if T.equal hd_t hd_u then (
            assert (List.length args_t = List.length args_u);
            (* unify arguments pairwise. no depth increase needed *)
            let pb' =
              mk_unif
                ~lits:pb.unif_lits
                ~cstr:(List.combine args_t args_u @
                   unif_cstr {pb with unif_rigid_rigid=tail})
                ~proof:pb.unif_proof ~penalty:pb.unif_penalty
                ~depth:pb.unif_depth
            in
            Queue.push pb' q
          )
        | [], (t,u)::tail ->
          (* try to solve [t=u] *)
          assert (not (is_rigid_rigid t u));
          HO_unif.unif_step (Ctx.combinators (), 1) ((t,u),0)
          |> List.iter
            (fun (subst,subst_penalty) ->
               let renaming = Ctx.renaming_clear () in
               let proof = Proof.Parent.add_subst (pb.unif_proof,0) subst in
               let t =
                 Subst.FO.apply ~renaming subst (t,0) |> RW.Term.normalize_term_fst
               and u =
                 Subst.FO.apply ~renaming subst (u,0) |> RW.Term.normalize_term_fst
               in
               let lits =
                 Literal.apply_subst_list ~renaming subst (pb.unif_lits,0)
               and cstr =
                 (t,u) :: unif_cstr {pb with unif_flex_rigid=tail}
               in
               let penalty = pb.unif_penalty + subst_penalty in
               let depth = pb.unif_depth + 1 in
               let pb' = mk_unif ~lits ~cstr ~penalty ~proof ~depth in
               Queue.push pb' q)
      end
    done;
    Util.add_stat stat_unif (List.length !sols);
    !sols

  (* HO unif rule, applies to literals [F t != u] or [P t] or [¬ P t] *)
  let ho_unif_ (c:C.t) : C.t list =
    (* try doing HO unif using combinators for [l != r] *)
    let try_unif_combinator_ l r pos =
      let pos = Literals.Pos.idx pos in
      if BV.get (C.eligible_res_no_subst c) pos then (
        Util.debugf ~section 5 "(@[try_ho_unif@ :lit %a@ :idx %d@])"
          (fun k->k Literal.pp (C.lits c).(pos) pos);
        HO_unif.unif_step (Ctx.combinators (), 1) ((l,r),0)
        |> List.rev_map
          (fun (subst,subst_penalty) ->
             Util.incr_stat stat_unif;
             let renaming = Ctx.renaming_clear () in
             let rule = Proof.Rule.mk "ho_unif" in
             let proof = Proof.Step.inference ~rule
                 [C.proof_parent_subst (c,0) subst] in
             let new_lits = Literals.apply_subst ~renaming subst (C.lits c,0) in
             let trail = C.trail c in
             let penalty = C.penalty c + subst_penalty in
             let new_c = C.create_a ~trail ~penalty new_lits proof in
             Util.debugf ~section 3
               "(@[<hv2>ho_unif@ :on @[%a@]@ :yields @[%a@]@])"
               (fun k->k C.pp c C.pp new_c);
             new_c)
      ) else []
    in
    (* try negative HO unif lits that are also eligible for resolution *)
    let eligible = C.Eligible.(lit_is_unshielded_ho_unif c ** res c) in
    let new_clauses =
      Literals.fold_ho_constraint (C.lits c) ~eligible
      |> Sequence.flat_map_l
        (fun (l, r, pos) -> try_unif_combinator_ l r pos)
      |> Sequence.to_rev_list
    in
    new_clauses

  let ho_unif c = Util.with_prof prof_unif ho_unif_ c

  let mk_parameter =
    let n = ref 0 in
    fun ty ->
      let i = CCRef.incr_then_get n in
      let id = ID.makef "#k%d" i in
      ID.set_payload id (ID.Attr_parameter i);
      T.const id ~ty

  (* negative extensionality rule:
     [f != g] where [f : a -> b] becomes [f k != g k] for a fresh parameter [k] *)
  let ext_neg (lit:Literal.t): Literal.t option = match lit with
    | Literal.Equation (f, g, false)
      when Type.is_fun (T.ty f) &&
           not (T.is_var f) &&
           not (T.is_var g) &&
           not (T.equal f g) ->
      let n_ty_params, ty_args, _ = Type.open_poly_fun (T.ty f) in
      assert (n_ty_params=0);
      let params = List.map mk_parameter ty_args in
      let new_lit =
        Literal.mk_neq
          (T.app f params)
          (T.app g params)
      in
      Util.incr_stat stat_ext_neg;
      Util.debugf ~section 4
        "(@[ho_ext_neg@ :old `%a`@ :new `%a`@])"
        (fun k->k Literal.pp lit Literal.pp new_lit);
      Some new_lit
    | _ -> None

  (* complete [f = g] into [f x1…xn = g x1…xn] *)
  let complete_eq_args (c:C.t) : C.t list =
    let var_offset = C.Seq.vars c |> Type.Seq.max_var |> succ in
    let new_c =
      C.lits c
      |> Sequence.of_array |> Sequence.zip_i |> Sequence.zip
      |> Sequence.filter_map
        (fun (lit_idx,lit) -> match lit with
          | Literal.Equation (t, u, true) when Type.is_fun (T.ty t) ->
            let n_ty_args, ty_args, _ = Type.open_poly_fun (T.ty t) in
            assert (n_ty_args = 0);
            let vars =
              List.mapi
                (fun i ty -> HVar.make ~ty (i+var_offset) |> T.var)
                ty_args
            in
            let new_lit = Literal.mk_eq (T.app t vars) (T.app u vars) in
            let new_lits = new_lit :: CCArray.except_idx (C.lits c) lit_idx in
            let proof =
              Proof.Step.inference [C.proof_parent c]
                ~rule:(Proof.Rule.mk "ho_complete_eq")
            in
            let new_c =
              C.create new_lits proof ~penalty:(C.penalty c) ~trail:(C.trail c)
            in
            Some new_c
          | _ -> None)
      |> Sequence.to_rev_list
    in
    if new_c<>[] then (
      Util.add_stat stat_complete_eq (List.length new_c);
      Util.debugf ~section 4
        "(@[complete-eq@ :clause %a@ :yields (@[<hv>%a@])@])"
        (fun k->k C.pp c (Util.pp_list ~sep:" " C.pp) new_c);
    );
    new_c

  (* try to eliminate a predicate variable in one fell swoop *)
  let elim_pred_variable (c:C.t) : C.t list =
    (* find unshielded predicate vars *)
    let find_vars(): _ HVar.t Sequence.t =
      Purify.unshielded_vars (C.lits c)
      |> Sequence.of_list
      |> Sequence.filter
        (fun v -> Type.is_prop @@ Type.returns @@ HVar.ty v)
    (* find all constraints on [v], also returns the remaining literals.
       returns None if some constraints contains [v] itself. *)
    and gather_lits v : (Literal.t list * (T.t list * bool) list) option =
      try
        Array.fold_left
          (fun (others,set) lit ->
             begin match lit with
               | Literal.Prop (t, sign) ->
                 let f, args = T.as_app t in
                 begin match T.view f with
                   | T.Var q when HVar.equal Type.equal v q ->
                     (* found an occurrence *)
                     if List.exists (T.var_occurs ~var:v) args then (
                       raise Exit; (* [P … t[v] …] is out of scope *)
                     );
                     others, (args, sign) :: set
                   | _ -> lit :: others, set
                 end
               | _ -> lit :: others, set
             end)
          ([], [])
          (C.lits c)
        |> CCOpt.return
      with Exit -> None
    in
    (* try to eliminate [v], if it doesn't occur in its own arguments *)
    let try_elim_var v: _ option =
      (* gather constraints on [v] *)
      begin match gather_lits v with
        | None
        | Some (_, []) -> None
        | Some (other_lits, constr_l) ->
          (* gather positive/negative args *)
          let pos_args, neg_args =
            CCList.partition_map
              (fun (args,sign) -> if sign then `Left args else `Right args)
              constr_l
          in
          (* build substitution used for this inference *)
          let subst =
            let some_tup = match pos_args, neg_args with
              | tup :: _, _ | _, tup :: _ -> tup
              | [], [] -> assert false
            in
            let offset = C.Seq.vars c |> T.Seq.max_var |> succ in
            let vars =
              List.mapi (fun i t -> HVar.make ~ty:(T.ty t) (i+offset)) some_tup
            in
            let vars_t = List.map T.var vars in
            let body =
              neg_args
              |> List.map
                (fun tup ->
                   assert (List.length tup = List.length vars);
                   List.map2 T.Form.eq vars_t tup |> T.Form.and_l)
              |> T.Form.or_l
            in
            Util.debugf ~section 5
              "(@[elim-pred-with@ (@[@<1>λ @[%a@].@ %a@])@])"
              (fun k->k (Util.pp_list Type.pp_typed_var) vars T.pp body);
            let t =
              HO_unif.Combinators.conv_lambda
                (Ctx.combinators()) vars body
            in
            Subst.FO.of_list [((v:>InnerTerm.t HVar.t),0), (t,0)]
          in
        (* build new clause *)
        let renaming = Ctx.renaming_clear () in
        let new_lits =
          let l1 = Literal.apply_subst_list ~renaming subst (other_lits,0) in
          let l2 =
            CCList.product
              (fun args_pos args_neg ->
                 let args_pos = Subst.FO.apply_l ~renaming subst (args_pos,0) in
                 let args_neg = Subst.FO.apply_l ~renaming subst (args_neg,0) in
                 List.map2 Literal.mk_eq args_pos args_neg)
              pos_args
              neg_args
            |> List.flatten
          in
          l1 @ l2
        in
        let proof =
          Proof.Step.inference ~rule:(Proof.Rule.mk "ho_elim_pred")
            [ C.proof_parent_subst (c,0) subst ]
        in
        let new_c =
          C.create new_lits proof
            ~penalty:(C.penalty c) ~trail:(C.trail c)
        in
        Util.debugf ~section 3
          "(@[<2>elim_pred_var %a@ :clause %a@ :yields %a@])"
          (fun k->k T.pp_var v C.pp c C.pp new_c);
        Some new_c
      end
    in
    begin
      find_vars()
      |> Sequence.filter_map try_elim_var
      |> Sequence.to_rev_list
    end

  (* smart [t ≠ₒ u] that turns [(¬ F a) ≠ t] into [F a ≠ ¬t] *)
  let mk_prop_unif_constraint t u : Literal.t =
    assert (Type.is_prop @@ T.ty t);
    begin match T.view t, T.view u with
      | T.AppBuiltin (Builtin.Not, [t']), _
        when Term.is_ho_pred t' && not (Term.is_ho_pred u) ->
        Literal.mk_ho_constraint t' (T.Form.not_ u)
      | _, T.AppBuiltin (Builtin.Not, [u'])
        when Term.is_ho_pred u' && not (Term.is_ho_pred t) ->
        Literal.mk_ho_constraint (T.Form.not_ t) u'
      | _ -> Literal.mk_ho_constraint t u
    end

  (* factor together literals.
     [C ∨ l ∨ l'] becomes  [C ∨ l ∨ l≠l'] where at least one of [l,l'] is HO *)
  let factor_rule (c:C.t): C.t list =
    let res =
      C.lits c
      |> Sequence.of_array_i
      |> Sequence.diagonal
      |> Sequence.filter_map
        (fun ((i1,lit1),(i2,lit2)) ->
           let is_ho_lit1 = Literal.is_ho_predicate lit1 in
           let is_ho_lit2 = Literal.is_ho_predicate lit2 in
           begin match Literal.to_ho_term lit1, Literal.to_ho_term lit2 with
             | Some t1, Some t2 when is_ho_lit1 || is_ho_lit2  ->
               (* we shall do factoring! *)
               let constr = mk_prop_unif_constraint t1 t2 in
               (* now, which literal do we remove? if one of them
                  is first-order, then keep it (will restrict unifications) *)
               let remove_idx =
                 if is_ho_lit1 && not is_ho_lit2 then i1 else i2
               in
               Some (remove_idx, constr)
             | _ -> None
           end)
      |> Sequence.map
        (fun (remove_idx,constr) ->
           let new_lits = Array.copy (C.lits c) in
           new_lits.(remove_idx) <- constr;
           let proof =
             Proof.Step.inference ~rule:(Proof.Rule.mk "ho.factor")
               [C.proof_parent c]
           in
           let new_c =
             C.create_a new_lits proof
               ~penalty:(C.penalty c+1) ~trail:(C.trail c)
           in
           Util.incr_stat stat_factor;
           Util.debugf ~section 4
             "(@[<hv2>ho_factor@ :clause %a@ :idx %d :yields %a@])"
             (fun k->k C.pp c remove_idx C.pp new_c);
           new_c)
      |> Sequence.to_rev_list
    in
    res

  (* ensure that combinators are defined functions *)
  let declare_combinators() =
    let module RW = Rewrite in
    let c = Ctx.combinators () in
    List.iter
      (fun (id,ty) -> Ctx.declare id ty)
      (HO_unif.Combinators.decls c);
    List.iter
      (fun (r,_) ->
         let id = RW.Term.Rule.head_id r in
         RW.Defined_cst.declare_or_add id (Rewrite.T_rule r))
      (HO_unif.Combinators.rules c);
    ()

  let setup () =
    Util.debug ~section 1 "setup HO rules";
    Env.Ctx.lost_completeness();
    (* force rules *)
    let () = ignore (HO_unif.Combinators.rules @@ Ctx.combinators ()) in
    declare_combinators ();
    Env.add_unary_inf "ho_unif" ho_unif;
    Env.add_unary_inf "ho_complete_eq" complete_eq_args;
    Env.add_unary_inf "ho_elim_pred_var" elim_pred_variable;
    Env.add_unary_inf "ho_factor" factor_rule;
    Env.add_lit_rule "ho_ext_neg" ext_neg;
    ()
end

let k_some_ho : bool Flex_state.key = Flex_state.create_key()

let st_contains_ho (st:(_,_,_) Statement.t): bool =
  (* is there a HO variable? *)
  let has_ho_var () =
    Statement.Seq.terms st
    |> Sequence.flat_map T.Seq.vars
    |> Sequence.exists
      (fun v ->
         let n_ty_vars, args, _ = Type.open_poly_fun (HVar.ty v) in
         n_ty_vars > 0 || args<>[])
  (* is there a HO symbol? *)
  and has_ho_sym () =
    Statement.Seq.ty_decls st
    |> Sequence.exists (fun (_,ty) -> Type.order ty > 1)
  in
  has_ho_sym () || has_ho_var ()

let extension =
  let register env =
    let module E = (val env : Env.S) in
    if E.flex_get k_some_ho then (
      let module ET = Make(E) in
      ET.setup ()
    )
  (* check if there are HO variables *)
  and check_ho vec state =
    let is_ho =
      CCVector.to_seq vec
      |> Sequence.exists st_contains_ho
    in
    if is_ho then (
      Util.debug ~section 2 "problem is HO"
    );
    Flex_state.add k_some_ho is_ho state
  in
  { Extensions.default with
      Extensions.name = "ho";
      post_cnf_actions=[check_ho];
      env_actions=[register];
  }

let () = Extensions.register extension


(* This file is free software, part of Zipperposition. See file "license" for more details. *)

open Logtk

module Lits = Literals
module T = FOTerm

type term = T.t

let section = Ind_ty.section
let stat_acyclicity = Util.mk_stat "ind_ty.acyclicity_steps"
let stat_disjointness = Util.mk_stat "ind_ty.disjointness_steps"
let stat_injectivity = Util.mk_stat "ind_ty.injectivity_steps"
let stat_exhaustiveness = Util.mk_stat "ind_ty.exhaustiveness_steps"

(** {1 Deal with Inductive Types} *)
module Make(Env : Env_intf.S) = struct
  module C = Env.C

  let as_cstor_app (t:term): (ID.t * term list) option =
    begin match T.view t with
      | T.App (f, l) ->
        begin match T.view f with
          | T.Const id when Ind_ty.is_constructor id -> Some (id,l)
          | _ -> None
        end
      | T.Const id when Ind_ty.is_constructor id -> Some (id, [])
      | T.Const _
      | T.Var _
      | T.DB _
      | T.AppBuiltin _ -> None
    end

  let is_cstor_app t = CCOpt.is_some (as_cstor_app t)

  (* traverse all the sub-terms under at least one constructor *)
  let walk_cstor_args (t:term): term Sequence.t =
    let rec aux t = match as_cstor_app t with
      | Some (_, l) ->
        Sequence.of_list l |> Sequence.flat_map aux
      | None -> Sequence.empty
    in
    aux t

  let acyclicity lit: [`Absurd | `Trivial | `Neither] =
    let occurs_in_ t ~sub =
      walk_cstor_args t |> Sequence.exists (T.equal sub)
    in
    begin match lit with
      | Literal.Equation (l, r, b) ->
        if
          ( Ind_ty.is_inductive_type (T.ty l) && occurs_in_ ~sub:l r )
          ||
          ( Ind_ty.is_inductive_type (T.ty r) && occurs_in_ ~sub:r l )
        then if b then `Absurd else `Trivial else `Neither
      | _ -> `Neither
    end

  let acyclicity_trivial c: bool =
    let res =
      C.Seq.lits c
      |> Sequence.exists
        (fun lit -> match acyclicity lit with
           | `Neither
           | `Absurd -> false
           | `Trivial -> true)
    in
    if res then (
      Util.incr_stat stat_acyclicity;
      Util.debugf ~section 3 "@[<2>acyclicity:@ `@[%a@]` is trivial@]"
        (fun k->k C.pp c);
    );
    res

  let acyclicity_simplify c: C.t SimplM.t =
    let lits' =
      C.Seq.lits c
      |> Sequence.filter
        (fun lit -> match acyclicity lit with
           | `Neither
           | `Trivial -> true
           | `Absurd -> false (* remove lit *)
        )
      |> Sequence.to_array
    in
    if Array.length lits' = Array.length (C.lits c)
    then SimplM.return_same c
    else (
      let proof =
        ProofStep.mk_inference ~rule:(ProofStep.mk_rule "acyclicity") [C.proof c] in
      let c' = C.create_a ~trail:(C.trail c) lits' proof in
      Util.incr_stat stat_acyclicity;
      Util.debugf ~section 3
        "@[<2>acyclicity:@ simplify `@[%a@]`@ into `@[%a@]`@]" (fun k->k C.pp c C.pp c');
      SimplM.return_new c'
    )

  let acyclicity_inf (c:C.t): C.t list =
    (* unify [sub] with cstor-prefixed subterms of [t] *)
    let unify_sub t ~sub =
      walk_cstor_args t
      |> Sequence.filter_map
        (fun t' ->
           try Some (Unif.FO.unification (t',0) (sub,0))
           with Unif.Fail -> None)
    in
    (* try to kill a [t=u] if there is [sigma] s.t. acyclicity applies
       to [t\sigma = u\sigma] *)
    let kill_lit lit: Subst.t Sequence.t =
      begin match lit with
        | Literal.Equation (l, r, true) ->
          begin match as_cstor_app l, as_cstor_app r with
            | Some _, None -> unify_sub l ~sub:r
            | None, Some _ -> unify_sub r ~sub:l
            | Some _, Some _
            | None, None -> Sequence.empty
          end
        | _ -> Sequence.empty
      end
    in
    begin
      Sequence.of_array_i (C.lits c)
      |> Sequence.flat_map
        (fun (i, lit) ->
           kill_lit lit |> Sequence.map (fun subst -> i, subst))
      |> Sequence.map
        (fun (i,subst) ->
           (* delete i-th literal and build new clause *)
           let new_lits = CCArray.except_idx (C.lits c) i in
           let renaming = Env.Ctx.renaming_clear () in
           let new_lits = Literal.apply_subst_list ~renaming subst (new_lits,0) in
           let proof =
             ProofStep.mk_inference [C.proof c]
               ~rule:(ProofStep.mk_rule "acyclicity")
           in
           let new_c =
             C.create
               ~trail:(C.trail c)
               new_lits proof
           in
           Util.incr_stat stat_acyclicity;
           Util.debugf ~section 3
             "@[<2>acyclicity@ :from `@[%a@]`@ :into `@[%a@]`@ :subst %a@]"
             (fun k->k C.pp c C.pp new_c Subst.pp subst);
           new_c)

      |> Sequence.to_rev_list
    end

  (* find, in [c], a literal which a (dis)equation of given sign
     between two constructors *)
  let find_cstor_pair ~sign ~eligible c =
    Lits.fold_lits ~eligible (C.lits c)
    |> Sequence.find
      (fun (lit, i) -> match lit with
         | Literal.Equation (l, r, sign') when sign=sign' ->
           begin match T.Classic.view l, T.Classic.view r with
             | T.Classic.App (s1, l1), T.Classic.App (s2, l2)
               when Ind_ty.is_constructor s1
                 && Ind_ty.is_constructor s2
               -> Some (i,s1,l1,s2,l2)
             | _ -> None
           end
         | _ -> None)

  (* if c is `f(t1,...,tn) = f(t1',...,tn') or d`, with f inductive cstor, then
      replace c with `And_i (ti = ti' or d)` *)
  let injectivity_destruct_pos c =
    let eligible = C.Eligible.(filter Literal.is_eq) in
    match find_cstor_pair ~sign:true ~eligible c with
      | Some (idx,s1,l1,s2,l2) when ID.equal s1 s2 ->
        (* same constructor: simplify *)
        assert (List.length l1 = List.length l2);
        let other_lits = CCArray.except_idx (C.lits c) idx in
        let new_lits =
          List.combine l1 l2
          |> CCList.filter_map
            (fun (t1,t2) ->
               if T.equal t1 t2 then None else Some (Literal.mk_eq t1 t2))
        in
        let rule = ProofStep.mk_rule ~comment:["induction"] "injectivity_destruct+" in
        let proof = ProofStep.mk_inference ~rule [C.proof c] in
        (* make one clause per [new_lits] *)
        let clauses =
          List.map
            (fun lit ->
               C.create ~trail:(C.trail c) (lit :: other_lits) proof)
            new_lits
        in
        Util.incr_stat stat_injectivity;
        Util.debugf ~section 3 "@[<hv2>injectivity:@ simplify @[%a@]@ into @[<v>%a@]@]"
          (fun k->k C.pp c (CCFormat.list C.pp) clauses);
        Some clauses
      | Some _
      | None -> None

  (* if c is  f(t1,...,tn) != f(t1',...,tn') or d, with f inductive cstor, then
      replace c with    t1 != t1' or ... or tn != tn' or d *)
  let injectivity_destruct_neg c =
    let eligible = C.Eligible.(filter Literal.is_neq) in
    match find_cstor_pair ~sign:false ~eligible c with
      | Some (idx,s1,l1,s2,l2) when ID.equal s1 s2 ->
        (* same constructor: simplify *)
        let lits = CCArray.except_idx (C.lits c) idx in
        assert (List.length l1 = List.length l2);
        (* add [ti != ti'] for arguments that are not actually types;
           types should ALWAYS be equal anyway *)
        let new_lits =
          List.combine l1 l2
          |> CCList.filter_map
            (fun (t1,t2) ->
               let ty = T.ty t1 in
               if Type.is_tType ty then None else Some (Literal.mk_neq t1 t2))
        in
        let rule = ProofStep.mk_rule ~comment:["(ind_types)"] "injectivity_destruct-" in
        let proof = ProofStep.mk_inference ~rule [C.proof c] in
        let c' = C.create ~trail:(C.trail c) (new_lits @ lits) proof in
        Util.incr_stat stat_injectivity;
        Util.debugf ~section 3 "@[<hv2>injectivity:@ simplify @[%a@]@ into @[%a@]@]"
          (fun k->k C.pp c C.pp c');
        SimplM.return_new c'
      | Some _
      | None -> SimplM.return_same c

  (* rule on literals that are trivial or absurd depending on toplevel
     constructor *)
  let disjointness lit = match lit with
    | Literal.Equation (l,r,sign) ->
      begin match T.Classic.view l, T.Classic.view r with
        | T.Classic.App (s1, _), T.Classic.App (s2, _)
          when Ind_ty.is_constructor s1
            && Ind_ty.is_constructor s2
            && not (ID.equal s1 s2)
          ->
          (* s1(...) = s2(...) is absurd,
             s1(...) != s2(...) is obvious *)
          Util.incr_stat stat_disjointness;
          if sign
          then Some Literal.mk_absurd
          else Some Literal.mk_tauto
        | _ -> None
      end
    | _ -> None

  (* all ground terms for which we already applied the exhaustiveness inf *)
  let exhaustiveness_tbl_ : unit T.Tbl.t = T.Tbl.create 128

  let exhaustiveness (c:C.t): C.t list =
    let is_param t = match T.view t with
      | T.Const id -> Ind_cst.id_is_cst id
      | _ -> false
    in
    let mk_sub_skolem (t:term) (ty:Type.t): ID.t =
      if Ind_ty.is_inductive_type ty then (
        (* declare a constant, with a depth that (if any)
           is bigger than [t]'s depth.
           This way, the case will be smaller than the constant. *)
        let depth = match T.view t with
          | T.Const id ->
            Ind_cst.id_as_cst id |> CCOpt.map Ind_cst.depth |> CCOpt.map succ
          | _ -> None
        in
        Ind_cst.make ~is_sub:false ?depth ty |> Ind_cst.id
      ) else Ind_cst.make_skolem ty
    in
    (* how to build exhaustiveness axiom for a term [t] *)
    let make_axiom (t:term): C.t =
      assert (T.is_ground t);
      let ity, ty_params = match Ind_ty.as_inductive_type (T.ty t) with
        | Some t -> t
        | None -> assert false
      in
      assert (List.for_all Type.is_ground ty_params);
      let rhs_l =
        ity.Ind_ty.ty_constructors
        |> List.map
          (fun { Ind_ty.cstor_name; cstor_ty } ->
             let n_args, _, _ = Type.open_poly_fun cstor_ty in
             assert (n_args = List.length ty_params);
             let cstor_ty_args, ret =
               Type.apply cstor_ty ty_params |> Type.open_fun
             in
             assert (Type.equal ret (T.ty t));
             (* build new constants to pass to the cstor *)
             let args =
               List.map
                 (fun ty ->
                    let c = mk_sub_skolem t ty in
                    Env.Ctx.declare c ty;
                    T.const ~ty c)
                 cstor_ty_args
             in
             T.app_full
               (T.const ~ty:cstor_ty cstor_name)
               ty_params
               args)
      in
      let lits = List.map (Literal.mk_eq t) rhs_l in
      (* XXX: could derive this from the [data] that defines [ity]… *)
      let proof = ProofStep.mk_trivial in
      let new_c = C.create ~trail:Trail.empty lits proof in
      Util.incr_stat stat_exhaustiveness;
      Util.debugf ~section 3
        "(@[<2>exhaustiveness axiom@ :for `@[%a:%a@]`@ :clause %a@])"
        (fun k->k T.pp t Type.pp (T.ty t) C.pp new_c);
      new_c
    in
    (* find terms to instantiate exhaustiveness for, and do it *)
    begin
      let eligible = C.Eligible.(res c ** neg) in
      C.lits c
      |> Sequence.of_array_i
      |> Sequence.filter_map
        (fun (i,lit) -> if eligible i lit then Some lit else None)
      |> Sequence.flat_map_l
        (function
          | Literal.Equation (l, r, false)
            when Ind_ty.is_inductive_type (T.ty l) ->
            let ity, _ = Ind_ty.as_inductive_type (T.ty l) |> CCOpt.get_exn in
            let non_rec = not (Ind_ty.is_recursive ity) in
            (* [l != r] with some specific criteria.
               We always do it for ground terms of
               non-recursive inductive types, or for ground terms
               that are not parameters (i.e. not inductive constants).
               For [a != b] where both are parameters, it is required
               to instantiate them only if the type is non-recursive.
            *)
            (* FIXME: actually we'd need a notion of "infinite type" rather
               than just recursive/non-recursive… *)
            if
              (non_rec || not (is_param l && is_param r)) &&
              T.is_ground l && T.is_ground r
            then [l; r]
            else if
              T.is_ground l &&
              (non_rec || not (is_param l)) &&
               is_cstor_app r
            then [l]
            else if
              T.is_ground r &&
              (non_rec || not (is_param r)) &&
              is_cstor_app l
            then [r]
            else []
          | _ -> [])
      (* remove cstor-headed terms *)
      |> Sequence.filter
        (fun t ->
           not (is_cstor_app t) &&
           not (T.Tbl.mem exhaustiveness_tbl_ t))
      |> T.Set.of_seq
      |> T.Set.to_list
      |> List.rev_map
        (fun t ->
           T.Tbl.add exhaustiveness_tbl_ t ();
           let ax = make_axiom t in
           ax)
    end

  let setup() =
    Util.debug ~section 2 "setup inductive types calculus";
    Env.add_is_trivial acyclicity_trivial;
    Env.add_simplify acyclicity_simplify;
    Env.add_simplify injectivity_destruct_neg;
    Env.add_multi_simpl_rule injectivity_destruct_pos;
    Env.add_lit_rule "ind_types.disjointness" disjointness;
    Env.add_unary_inf "ind_types.acyclicity" acyclicity_inf;
    Env.add_unary_inf "ind_types.exhaustiveness" exhaustiveness;
    ()
end

let env_act (module E : Env_intf.S) =
  let module M = Make(E) in
  M.setup ()

let extension =
  let open Extensions in
  { default with
      name="ind_types";
      env_actions=[env_act]
  }

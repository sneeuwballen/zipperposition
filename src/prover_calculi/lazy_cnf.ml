open Logtk
open Libzipperposition

module L = Literal
module Ls = Literals
module T = Term

let enabled = ref false

let k_lazy_cnf_kind = Flex_state.create_key ()
let k_renaming_threshold = Flex_state.create_key ()
let k_rename_eq = Flex_state.create_key ()
let k_scoping = Flex_state.create_key ()
let k_solve_formulas = Flex_state.create_key ()
let k_skolem_mode = Flex_state.create_key ()


let section = Util.Section.make ~parent:Const.section "lazy_cnf"

module type S = sig
  module Env : Env.S
  module C : module type of Env.C with type t = Env.C.t

  (** {5 Registration} *)

  val setup : unit -> unit
  (** Register rules in the environment *)

  val update_form_counter: action:[< `Decrease | `Increase ] -> C.t -> unit
  val solve_bool_formulas: C.t -> C.t CCList.t option


end

module Make(E : Env.S) : S with module Env = E = struct
  module Env = E
  module C = Env.C
  module Combs = Combinators.Make(Env)
  
  let _form_counter = Term.Tbl.create 256

  module Idx = Fingerprint.Make(struct 
    type t = T.t * ((C.t * bool) list ref)
    let compare (a1,_) (a2,_) = (T.compare a1 a2)
   end)

  let sign_present sign = function
    | [(c,sign1)] -> sign1 = sign
    | [(c1, sign1);(c2, sign2)] ->
      if sign1 = sign2 then invalid_arg "signs must be different!";
      true
    | _ -> invalid_arg "only one or two element lists"


  let _skolem_idx = ref @@ Idx.empty ()
  let _renaming_idx = ref @@ Idx.empty ()
  let _renamer_symbols = ref @@ ID.Set.empty

  let solve_bool_formulas c =
    let module PUnif = 
      PUnif.Make(struct 
        let st = 
          Env.flex_state ()
          |> Flex_state.add PragUnifParams.k_fixpoint_decider true
          |> Flex_state.add PragUnifParams.k_pattern_decider true
          |> Flex_state.add PragUnifParams.k_solid_decider true
          |> Flex_state.add PragUnifParams.k_max_inferences 1
          |> Flex_state.add PragUnifParams.k_max_depth 4
          |> Flex_state.add PragUnifParams.k_max_app_projections 2
          |> Flex_state.add PragUnifParams.k_max_rigid_imitations 2
          |> Flex_state.add PragUnifParams.k_max_elims 0
          |> Flex_state.add PragUnifParams.k_max_identifications 0
        end) in
    
    let normalize_not t =
      let rec aux t = 
        match T.view t with
        | T.AppBuiltin(Not, [f]) ->
          begin match T.view f with
          | T.AppBuiltin(Not, [g]) -> aux g
          | T.AppBuiltin( ((Eq|Equiv) as b), l ) ->
            let flipped = 
              if b = Builtin.Eq then Builtin.Neq else Builtin.Xor in
            T.app_builtin flipped l ~ty:(T.ty f)
          | T.AppBuiltin( ((Neq|Xor) as b), l ) ->
            let flipped = 
              if b = Builtin.Neq then Builtin.Eq else Builtin.Equiv in
            T.app_builtin flipped l ~ty:(T.ty f)
          | _ -> t end
        | _ -> t in
      aux t in

    let find_resolvable_form lit =
      let is_var_headed t = T.is_var (T.head_term t) in

      (* Rewrite positive equations of the form F s = t, where t is not
         var-headed into F s != (not t).  *)
      let find_pos_var_headed_eq l r = 
        if is_var_headed l && not (is_var_headed r) then (Some(l, T.Form.not_ r)) 
        else if is_var_headed r && not (is_var_headed l) then (Some(r, T.Form.not_ l))
        else None in

      match (Literal.View.as_eqn lit) with 
      | Some (l,r,sign) ->
        if not (T.is_true_or_false r) && Type.is_prop (T.ty l) then (
          if not sign then Some (l,r)
          else find_pos_var_headed_eq l r)
        else if T.is_true_or_false r then (
          let neg = if sign then CCFun.id else T.Form.not_ in
          match T.view (normalize_not (neg l)) with 
          | T.AppBuiltin((Neq|Xor), ([f;g]|[_;f;g])) when Type.is_prop (T.ty f) ->
            assert(Type.equal (T.ty f) (T.ty g));
            Some (f,g)
          | T.AppBuiltin((Eq|Equiv), ([f;g]|[_;f;g])) when Type.is_prop (T.ty f) ->
            assert(Type.equal (T.ty f) (T.ty g));
            find_pos_var_headed_eq f g
          | _ -> None
        ) else None
      | None -> None in
    
    let unif_alg l r =
      if not (Env.flex_get Combinators.k_enable_combinators) then (
        PUnif.unify_scoped (l,0) (r,0)
        |> OSeq.filter_map CCFun.id
        |> OSeq.nth 0
      ) else Unif_subst.of_subst @@ Unif.FO.unify_syn (l,0) (r,0) in
    
    Util.debugf ~section 2 "bool solving @[%a@]@."(fun k -> k C.pp c);

    C.lits c
    |> CCArray.mapi (fun i lit ->
      match find_resolvable_form lit with 
      | None -> 
        None
      | Some (l,r) ->
        let module US = Unif_subst in
        try
          Util.debugf ~section 2 "trying lit @[%d:%a@]@."(fun k -> k i Literal.pp lit);
          Util.debugf ~section 2 "unif problem: @[%a=?=%a@]@."(fun k -> k T.pp l T.pp r);
          let subst = unif_alg l r in
          assert(not @@ Unif_subst.has_constr subst);
          let renaming = Subst.Renaming.create () in
          let new_lits =
            CCArray.except_idx (C.lits c) i
            |> CCArray.of_list
            |> (fun l -> 
                Literals.apply_subst renaming (US.subst subst) (l,0))
            |> CCArray.to_list in
          let proof = 
            Proof.Step.simp ~tags:[Proof.Tag.T_ho]
              ~rule:(Proof.Rule.mk "solve_formulas")
              [C.proof_parent_subst renaming (c,0) (US.subst subst) ] in
          let res = C.create ~penalty:(C.penalty c) ~trail:(C.trail c) new_lits proof in
          Util.debugf ~section 2 "solved by @[%a@]@."(fun k ->  k C.pp res);
          Some res
        with _ -> 
          Util.debugf ~section 2 "failed @." (fun k -> k);
          None)
      |> CCArray.filter_map CCFun.id
      |> CCArray.to_list
      |> (fun l -> if CCList.is_empty l then None else Some l)



  (* Two-literal clause of which one is a renaming literal
     and the other one is a formula *)
  let is_renaming_clause c =
    let is_renaming_lit = function
      | L.Equation (lhs, rhs, _) when T.equal T.true_ rhs ->
        let hd = T.head_term lhs in
        begin match T.head hd with
        | Some id  -> ID.Set.mem id !_renamer_symbols
        | None -> false end
      | _ -> false in
    let is_formula_lit = function 
      | L.Equation (lhs, rhs, _) ->
        if T.equal T.true_ rhs then T.is_appbuiltin lhs
        else Type.is_prop (T.ty lhs) && not (T.equal T.true_ rhs)
      | _ -> false in

    match C.lits c with
    | [| a; b |] ->
      CCArray.length (CCArray.filter is_renaming_lit (C.lits c)) = 1 &&
      CCArray.length (CCArray.filter is_formula_lit (C.lits c)) = 1
    | _ -> false

  let update_form_counter ~action c =
    if not (is_renaming_clause c) then (
      Ls.fold_eqn 
        ~both:false ~ord:(E.Ctx.ord ()) ~eligible:(C.Eligible.always) 
        (C.lits c)
      |> Iter.iter (fun (lhs,rhs,_,_) ->
        let terms = 
          if T.equal T.true_ rhs && T.is_appbuiltin lhs  then [lhs]
          else if Type.is_prop (T.ty lhs) && not (T.equal T.true_ rhs) &&
                  (Term.is_appbuiltin lhs || T.is_appbuiltin rhs) then
            [T.Form.equiv lhs rhs]
          else [] in
        List.iter (fun t -> 
          match action with
            | `Increase -> Term.Tbl.incr _form_counter t
            | `Decrease -> Term.Tbl.decr _form_counter t
        ) terms))

  let fold_lits c = 
    Ls.fold_eqn_simple (C.lits c)

  let proof ~constructor ~name ~parents c =
    constructor ~rule:(Proof.Rule.mk name)
      (List.map C.proof_parent parents)

  let mk_renaming_clause parent ~renamer ~form sign =
    let proof = Proof.Step.define_internal 
      (T.head_exn renamer) [C.proof_parent parent] in

    let res = 
      if sign then (
        C.create ~penalty:1 ~trail:Trail.empty 
          [L.mk_false renamer; L.mk_true form] proof
      ) else (C.create ~penalty:1 ~trail:Trail.empty 
          [L.mk_true renamer; L.mk_false form] proof
      ) in
    res

  let rename ~c form sign =
    assert(Type.is_prop (T.ty form));
    if is_renaming_clause c || not (T.is_appbuiltin form) || T.is_true_or_false form then None
    else (
      let gen = Iter.head @@ 
        Idx.retrieve_generalizations (!_renaming_idx, 0) (form, 1) in
      match gen with 
      | Some (orig, (renamer, defined_as), subst) ->
        let renamer_sub = 
          Subst.FO.apply Subst.Renaming.none subst (renamer,0) in

        let renamer_sub, new_defs, parents = 
          if sign_present sign !defined_as then (
            (renamer_sub, [], !defined_as)
          ) else (
            let def = mk_renaming_clause c ~renamer ~form:orig sign in
            defined_as := (def,sign) :: !defined_as;
            (renamer_sub, [def], !defined_as)) in

          Some(renamer_sub, new_defs,
              CCList.filter_map (fun (c, sign') ->
                if sign != sign' then None else Some c) parents)
      | None ->
        (* maybe we need to define it if it appears too many times *)
        let num_occurrences = Term.Tbl.get_or _form_counter form ~default:0 in
        if num_occurrences >= Env.flex_get k_renaming_threshold (*&&
           CCArray.length (C.lits c) > 1*)  then (
          Term.Tbl.remove _form_counter form;

          let free_vars = T.vars form |> T.VarSet.to_list in
          let (id, ty), renamer =
            T.mk_fresh_skolem ~prefix:"form" free_vars Type.prop in
          E.Ctx.declare id ty;
          let def = mk_renaming_clause c ~renamer ~form sign in
          _renaming_idx := Idx.add !_renaming_idx form (renamer, ref [(def,sign)]);
          _renamer_symbols := ID.Set.add id !_renamer_symbols;
          Some(renamer, [def], [def])
        ) else None
    )

  let rename_eq ~c lhs rhs sign =
    assert(Type.equal (T.ty lhs) (T.ty rhs));
    assert(Type.is_prop (T.ty lhs));
    if Env.flex_get k_rename_eq then rename ~c (T.Form.equiv lhs rhs) sign
    else None

  let mk_and ~proof_cons ~rule_name and_args c ?(parents=[c]) lit_idx =
    let lits = CCArray.except_idx (C.lits c) lit_idx in
    let proof = proof ~constructor:proof_cons ~parents ~name:rule_name c  in
    List.map (fun t -> 
      C.create ~penalty:(C.penalty c) ~trail:(C.trail c) 
        (L.mk_true t :: lits) proof) and_args
  
  let mk_or ~proof_cons ~rule_name or_args c ?(parents=[c]) lit_idx =
    let lits = 
      (List.map L.mk_true or_args) @
      (CCArray.except_idx (C.lits c) lit_idx) in
    let proof = proof ~constructor:proof_cons ~parents ~name:rule_name c in
    [C.create ~penalty:(C.penalty c) ~trail:(C.trail c) lits proof]

  let cnf_scope_form form =
    let kind = Env.flex_get k_scoping in
    let open CCOpt in

    let rec maxiscoping_eligible l =
      let get_quant t = 
        let t = Combs.expand t in
        match T.view t with
        | T.AppBuiltin((ForallConst|ExistsConst) as b, [x]) ->
          let ty, body = T.open_fun x in
          assert(List.length ty = 1);
          Some (b, List.hd ty, [body])
        | _ -> None in

      match l with
        | [] -> assert false;
        | [x] -> get_quant x
        | x :: xs -> 
          get_quant x
          >>= (fun (b,ty,body) -> 
            maxiscoping_eligible xs
            >>= (fun (b', ty', bodies) ->
              if Builtin.equal b b' && Type.equal ty ty' then (
                Some(b, ty, (List.hd body)::bodies)
              ) else None)) in

    let miniscope hd f =
      let distribute_quant hd ty bodies =
        let quant_hd = 
          if Builtin.equal hd Or then T.Form.exists else T.Form.forall in
        let outer_hd =
          if Builtin.equal hd Or then T.Form.or_l else T.Form.and_l in
        
        outer_hd (List.map (fun t -> quant_hd (T.fun_ ty t) ) bodies) in 

      let f = Combs.expand f in
      if T.is_fun f then (
        let ty, body = T.open_fun f in
        assert(List.length ty = 1);
        match T.view body with
        | T.AppBuiltin(Or, l) when Builtin.equal hd ExistsConst ->
          Some (distribute_quant Or (List.hd ty) l)
        | T.AppBuiltin(And, l) when Builtin.equal hd ForallConst ->
          Some (distribute_quant And (List.hd ty) l)
        | _ -> None
      ) else None in

    match T.view form with 
    | T.AppBuiltin(And, ((_ :: _) as l)) when kind = `Maxi ->
      begin match maxiscoping_eligible l with
      | Some (ForallConst, ty, bodies) ->
        Some (T.Form.forall (T.fun_ ty (T.Form.and_l bodies)))
      | _ -> None end
    | T.AppBuiltin(Or, ((_ :: _) as l)) when kind = `Maxi ->
      begin match maxiscoping_eligible l with
      | Some (ExistsConst, ty, bodies) ->
        Some (T.Form.exists (T.fun_ ty (T.Form.or_l bodies)))
      | _ -> None end
    | T.AppBuiltin(((ExistsConst|ForallConst) as b), [f]) when kind = `Mini ->
      miniscope b f
    | _ -> None

  let choice_tbl = Type.Tbl.create 32
  let lazy_clausify_driver ?(ignore_eq=false) ~proof_cons c =
    let return l =
      Iter.of_list l, `Stop in
    
    let continue =
      Iter.empty, `Continue in

    let eligible_for_ignore_eq ~ignore_eq lhs rhs = 
      ignore_eq && not (T.is_true_or_false lhs) && not (T.is_true_or_false rhs) in

    let get_skolem ~free_vars ~ret_ty ~mode f = 
      match mode with 
      | `Skolem ->
        let gen = Iter.head @@ 
        Idx.retrieve_generalizations (!_skolem_idx, 0) (f, 1) in
        begin match gen with 
        | Some (orig, (skolem, _), subst) ->
          Subst.FO.apply Subst.Renaming.none subst (skolem,0) 
        | None ->
          let free_vars_l = 
              T.VarSet.to_list (T.VarSet.of_iter free_vars) in
          let (id,ty), t = T.mk_fresh_skolem ~prefix:"sk" free_vars_l ret_ty in
          E.Ctx.declare id ty;
          Signal.send Env.on_pred_skolem_introduction (c, t);
          _skolem_idx := Idx.add !_skolem_idx f (t, ref[]);
          t end
      | `Choice ->
        let hd = Type.Tbl.get_or_add choice_tbl ~f:(fun ty ->
          let arg_tys, ret_ty_form = Type.open_fun ty in
          assert(List.length arg_tys == 1);
          assert(Type.is_prop ret_ty_form);
          assert(Type.equal (List.hd arg_tys) ret_ty);
          let (hd_id, hd_ty), res =
            Term.mk_fresh_skolem ~prefix:"$_choose" [] (Type.arrow [ty] (List.hd arg_tys)) in
          E.Ctx.declare hd_id hd_ty;
          res
        ) ~k:(T.ty f) in
        T.app hd [f] in

    Util.debugf ~section 2 "lazy_cnf(@[%a@])@." (fun k -> k C.pp c);

    let init = 
      if Env.flex_get k_solve_formulas
      then Iter.of_list (CCOpt.get_or ~default:[] (solve_bool_formulas c))
      else Iter.empty 
    in

    fold_lits c
    |> Iter.fold_while ( fun _ (lhs, rhs, sign, pos) ->
      let i,_ = Ls.Pos.cut pos in
      if T.equal rhs T.true_ then (
        Util.debugf ~section 2 "  subformula:%d:@[%a@]" (fun k -> k i L.pp (C.lits c).(i) );
        begin match T.view lhs with 
        | T.AppBuiltin(And, l) when List.length l >= 2 ->
          let rule_name = "lazy_cnf_and" in
          if sign then return @@ mk_and ~proof_cons l c i ~rule_name
          else return @@ mk_or ~proof_cons (List.map T.Form.not_ l) c i ~rule_name
        | T.AppBuiltin(Or, l) when List.length l >= 2 ->
          let rule_name = "lazy_cnf_or" in
          if sign then return @@ mk_or ~proof_cons l c i ~rule_name
          else return @@ mk_and ~proof_cons (List.map T.Form.not_ l) c i ~rule_name
        | T.AppBuiltin(Imply, [a;b]) ->
          let rule_name = "lazy_cnf_imply" in
          if sign then return @@ mk_or ~proof_cons [T.Form.not_ a; b] c i ~rule_name
          else return @@ mk_and ~proof_cons [a; T.Form.not_ b] c i ~rule_name
        | T.AppBuiltin((Equiv|Xor) as hd, [a;b]) ->
          let hd = if sign then hd else (if hd = Equiv then Xor else Equiv) in
          if eligible_for_ignore_eq ~ignore_eq a b then continue
          else (
            let rule_name = 
              CCFormat.sprintf "lazy_cnf_%s" 
                (if hd = Equiv then "equiv" else "xor") in
            if hd = Equiv then (
              return @@ (
                mk_or ~proof_cons ~rule_name [T.Form.not_ a; b] c i 
                  @ mk_or ~proof_cons ~rule_name [a; T.Form.not_ b] c i)
            ) else (
              return @@ (
                mk_or ~proof_cons ~rule_name [T.Form.not_ a; T.Form.not_ b] c i 
                @ mk_or ~proof_cons ~rule_name [a; b] c i
                )))
        | T.AppBuiltin((ForallConst|ExistsConst) as hd, [f]) ->
          let free_vars = T.Seq.vars f in
          let var_id = T.Seq.max_var (C.Seq.vars c) + 1 in
          let f = Combs.expand f in
          let var_tys, body =  T.open_fun f in
          assert(List.length var_tys = 1);
          let var_ty = List.hd var_tys in
          let hd, f =
            if sign then hd,f
            else ((if hd=ForallConst then ExistsConst else ForallConst),
                  T.fun_ var_ty (T.Form.not_ body)) in
          let rule_name = 
            CCFormat.sprintf "lazy_cnf_%s" 
              (if hd = ForallConst then "forall" else "exists") in
          let subst_term =
            if hd = ForallConst then (
              T.var @@ HVar.make ~ty:var_ty var_id
            ) else (
              get_skolem ~free_vars ~ret_ty:var_ty ~mode:(Env.flex_get k_skolem_mode) f
            ) in
          let res = Lambda.eta_reduce @@ Lambda.snf @@ T.app f [subst_term] in
          assert(Type.is_prop (T.ty res));
          let res_cl = mk_or ~proof_cons ~rule_name [res] c i in
          if Type.returns_prop var_ty && hd == ForallConst then (
            assert (List.length res_cl == 1);
            assert (T.is_var subst_term);
            Signal.send Env.on_pred_var_elimination (List.hd res_cl, subst_term)
          );
          return res_cl
        | T.AppBuiltin(Not, _) -> assert false
        | _ -> continue end
      ) else if Type.is_prop (T.ty lhs) && not (T.equal T.true_ rhs) then (
          let rule_name = 
            CCFormat.sprintf "lazy_cnf_%s" (if sign then "equiv" else "xor") in
          
          Util.debugf ~section 2 "  subeq:%d:@[%a %s= %a@]" (fun k -> k i T.pp lhs (if sign then "" else "~") T.pp rhs );
          if eligible_for_ignore_eq ~ignore_eq lhs rhs then continue
          else if sign then (
            return @@ (
              mk_or ~proof_cons ~rule_name [T.Form.not_ lhs; rhs] c i 
              @ mk_or ~proof_cons ~rule_name [lhs; T.Form.not_ rhs] c i)
          ) else (
            return @@ (
                mk_or ~proof_cons ~rule_name [T.Form.not_ lhs; T.Form.not_ rhs] c i 
                @ mk_or ~proof_cons ~rule_name [lhs; rhs] c i ))
      ) else continue) (init)

  let rename_subformulas c =
    Util.debugf ~section 2 "lazy-cnf-rename(@[%a@])@." (fun k -> k C.pp c);
    fold_lits c
    |> Iter.fold_while (fun _ (lhs,rhs,sign,pos) -> 
      let i,_ = Ls.Pos.cut pos in
      let proof_cons = Proof.Step.simp ~infos:[] ~tags:[Proof.Tag.T_live_cnf; Proof.Tag.T_dont_increase_depth] in
      if T.equal rhs T.true_ && T.is_appbuiltin lhs then (
        match rename ~c lhs sign with
        | Some (renamer, new_defs, parents) ->
          let rule_name = "renaming" in
          let renamer = (if sign then CCFun.id else T.Form.not_) renamer in
          let renamed = mk_or ~proof_cons ~rule_name [renamer] c ~parents:(c :: parents) i in
          let res = renamed @ new_defs in
          Util.debugf ~section 2 "  @[renamed subformula %d:(@[%a@])=@. @[%a@]@]@." 
            (fun k -> k i C.pp c (CCList.pp C.pp) renamed);
          Util.debugf ~section 2 "  new defs:@[%a@]@." 
            (fun k -> k (CCList.pp C.pp) new_defs);
          Some res, `Stop
        | None -> None, `Continue
      ) else if Type.is_prop (T.ty lhs) && not (T.equal rhs T.true_) &&
              (T.is_appbuiltin lhs || T.is_appbuiltin rhs) then (
          match rename_eq ~c lhs rhs sign with
          | Some (renamer, new_defs, parents) ->
            let rule_name = "renaming" in
            let renamer = (if sign then CCFun.id else T.Form.not_) renamer in
            let renamed = mk_or ~proof_cons ~rule_name [renamer] c ~parents:(c :: parents) i in
            let res = renamed @ new_defs in
            Util.debugf ~section 2 "  @[renamed eq %d(@[%a@]) into @[%a@]@]@." 
            (fun k -> k i L.pp (C.lits c).(i) (CCList.pp C.pp) renamed);
            Util.debugf ~section 2 "  new defs:@[%a@]@." 
              (fun k -> k (CCList.pp C.pp) new_defs);
              Some res, `Stop
          | None -> None, `Continue)
        else None, `Continue) None

  let clausify_eq c =
    let rule_name = "eq_elim" in
    fold_lits c
    |> Iter.fold (fun acc (lhs,rhs,sign,pos) -> 
        let i,_ = Ls.Pos.cut pos in
        let proof_cons = Proof.Step.inference ~infos:[] ~tags:[Proof.Tag.T_live_cnf; Proof.Tag.T_dont_increase_depth] in
        if not (T.is_true_or_false rhs) && Type.is_prop (T.ty lhs) then (
          let new_cls =
            if sign then (
              mk_or ~proof_cons ~rule_name [T.Form.not_ lhs; rhs] c i 
              @ mk_or ~proof_cons ~rule_name [lhs; T.Form.not_ rhs] c i
            ) else (
              (mk_or ~proof_cons ~rule_name [T.Form.not_ lhs; T.Form.not_ rhs] c i)
              @ (mk_or ~proof_cons ~rule_name [lhs; rhs] c i)) in
          let pen_inc = 
            if (T.is_ground lhs && T.is_ground rhs) then 0 
            else (if T.is_app_var lhs || T.is_app_var rhs then 3 else 1) in
          List.iter (fun c -> C.inc_penalty c pen_inc) new_cls;
          new_cls @ acc
        ) else acc) []
  
  let cnf_scope c =
    fold_lits c
    |> Iter.fold_while (fun _ (lhs,rhs,sign,pos) -> 
      let i,_ = Ls.Pos.cut pos in
      let proof_cons = Proof.Step.simp ~infos:[] ~tags:[Proof.Tag.T_live_cnf; Proof.Tag.T_dont_increase_depth] in
      if T.equal rhs T.true_ && T.is_appbuiltin lhs then (
        match cnf_scope_form lhs with 
        | Some f ->
          let rule_name = CCFormat.sprintf "lazy_cnf_%sscoping" 
            (if Env.flex_get k_scoping == `Maxi then "maxi" else "mini") in
          let app_sign = if sign then CCFun.id else T.Form.not_ in
          Some (mk_or ~proof_cons [app_sign f] c i ~rule_name), `Stop
        | None -> None, `Continue
      ) else None, `Continue) None

  let lazy_clausify_simpl c =
    update_form_counter ~action:`Increase c;

    let proof_cons = Proof.Step.simp ~infos:[] ~tags:[Proof.Tag.T_live_cnf; Proof.Tag.T_dont_increase_depth] in
    let res = Iter.to_list @@ lazy_clausify_driver ~ignore_eq:true ~proof_cons c  in
    if not @@ CCList.is_empty res then (
      Util.debugf ~section 2 "lazy_cnf_simp(@[%a@])=" (fun k -> k C.pp c);
      Util.debugf ~section 2 "@[%a@]@." (fun k -> k (CCList.pp C.pp) res);
      update_form_counter ~action:`Decrease c;
      CCList.iter (update_form_counter ~action:`Increase) res;
    ) else Util.debugf ~section 3 "lazy_cnf_simp(@[%a@])=Ø" (fun k -> k C.pp c);
    if CCList.is_empty res then None
    else (Some res)

  let lazy_clausify_inf c =
    let proof_cons = Proof.Step.inference ~infos:[] ~tags:[Proof.Tag.T_live_cnf; Proof.Tag.T_dont_increase_depth] in
    let res = Iter.to_list (lazy_clausify_driver ~ignore_eq:false ~proof_cons c) in
    if not @@ CCList.is_empty res then (
      Util.debugf ~section 2 "lazy_cnf_inf(@[%a@])=" (fun k -> k C.pp c);
      Util.debugf ~section 2 "@[%a@]@." (fun k -> k (CCList.pp C.pp) res);
    ) else Util.debugf ~section 3 "lazy_cnf_simp(@[%a@])=Ø" (fun k -> k C.pp c);
    res

  let setup () =
    if !enabled then (
      (* orders are still not really fixed for completeness *)
      Env.Ctx.lost_completeness ();
      begin match Env.flex_get k_lazy_cnf_kind with 
      | `Inf -> Env.add_unary_inf "lazy_cnf" lazy_clausify_inf
      | `Simp -> 
          Env.add_unary_inf "elim eq" clausify_eq;
          Env.add_multi_simpl_rule ~priority:5 lazy_clausify_simpl 
      end;

      (* ** IMPORTANT **
         RENAMING MUST BE ADDED AFTER CLAUSIFICATION RULES (so that
         it runs ***before*** lazy_clausify_simpl) *)
      if Env.flex_get k_renaming_threshold > 0 then(
        Env.add_multi_simpl_rule ~priority:5 rename_subformulas
      );
      if Env.flex_get k_scoping != `Off then (
        Env.add_multi_simpl_rule ~priority:5 cnf_scope;
      )
    )
end

let _lazy_cnf_kind = ref `Simp
let _renaming_threshold = ref 8
let _rename_eq = ref true
let _scoping = ref `Off
let _solve_formulas = ref false
let _skolem_mode = ref `Skolem

let extension =
  let register env =
    let module E = (val env : Env.S) in
    let module ET = Make(E) in
    E.flex_add k_lazy_cnf_kind !_lazy_cnf_kind;
    E.flex_add k_renaming_threshold !_renaming_threshold;
    E.flex_add k_rename_eq !_rename_eq;
    E.flex_add k_scoping !_scoping;
    E.flex_add k_solve_formulas !_solve_formulas;
    E.flex_add k_skolem_mode !_skolem_mode;

    let handler f c =
      f c;
      Signal.ContinueListening in

    if E.flex_get k_lazy_cnf_kind == `Inf then (
      Signal.on E.ProofState.PassiveSet.on_add_clause 
        (handler (ET.update_form_counter ~action:`Increase));
      Signal.on E.ProofState.ActiveSet.on_add_clause
        (handler (ET.update_form_counter ~action:`Increase));
      Signal.on E.ProofState.PassiveSet.on_remove_clause
        (handler (ET.update_form_counter ~action:`Decrease));
      Signal.on E.ProofState.ActiveSet.on_remove_clause
        (handler (ET.update_form_counter ~action:`Decrease))
    );

    ET.setup ()
  in
  { Extensions.default with
    Extensions.name = "lazy_cnf";
    env_actions=[register];
  }

let () =
  Options.add_opts [
    "--lazy-cnf", Arg.Bool ((:=) enabled), " turn on lazy clausification";
    "--lazy-cnf-scoping", Arg.Symbol (["off"; "mini"; "maxi"], (fun str -> 
      match str with 
      | "mini" -> _scoping := `Mini
      | "maxi" -> _scoping := `Maxi
      | "off" -> _scoping := `Off
      | _ -> assert false)), 
    " use mini/maxi scoping rules for lazy cnf";
    "--lazy-cnf-renaming-threshold", Arg.Int ((:=) _renaming_threshold), 
      " set the subformula renaming threshold -- negative value turns renaming off";
    "--solve-formulas"
    , Arg.Bool (fun v -> _solve_formulas := v)
    , " solve phi != psi eagerly using unification, where phi and psi are formulas";
    
    "--lazy-cnf-skolem-mode", Arg.Symbol (["skolem"; "choice"], (fun str -> 
      match str with 
      | "skolem" -> _skolem_mode := `Skolem
      | "choice" -> _skolem_mode := `Choice
      | _ -> assert false)), " use lazy cnf as either simplification or inference";
    "--lazy-cnf-kind", Arg.Symbol (["inf"; "simp"], (fun str -> 
      match str with 
      | "inf" -> _lazy_cnf_kind := `Inf
      | "simp" -> _lazy_cnf_kind := `Simp
      | _ -> assert false)), " use lazy cnf as either simplification or inference";
    "--lazy-cnf-rename-eq", Arg.Bool ((:=) _rename_eq), " turn on/of renaming of boolean equalities"];

  Params.add_to_modes ["ho-complete-basic";
                       "ho-pragmatic";
                       "lambda-free-intensional";
                       "lambda-free-purify-intensional";
                       "lambda-free-extensional";
                       "ho-comb-complete";
                       "lambda-free-purify-extensional";
                       "fo-complete-basic"] (fun () ->
      enabled := false;
  );
  Extensions.register extension
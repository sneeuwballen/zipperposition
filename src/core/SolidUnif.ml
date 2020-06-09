module T = Term
module S = Subst
module PU = PatternUnif
module US = Unif_subst
module PUP = PragUnifParams

module US_A = struct

  let apply s t = Subst.FO.apply Subst.Renaming.none (US.subst s) t

  let apply_ty s ty = Subst.Ty.apply Subst.Renaming.none (US.subst s) ty
  let pp = US.pp

end

let prof_flex_same = ZProf.make "su.flex_same"
let prof_flex_diff = ZProf.make "su.flex_diff"
let prof_flex_rigid = ZProf.make "su.flex_rigid"
let prof_cover_rigid = ZProf.make "su.cover_rigid"
let prof_solidifier = ZProf.make "su.solidifier"

exception NotInFragment = PU.NotInFragment
exception NotUnifiable = PU.NotUnifiable

module Make (St : sig val st : Flex_state.t end) = struct

  let get_op k = Flex_state.get_exn k St.st

  (* exception NotSolid *)
  exception CoveringImpossible

  let eta_expand_otf ~subst ~scope pref1 pref2 t1 t2 =
    let do_exp_otf n types t = 
      let remaining = CCList.drop n types in
      assert(List.length remaining != 0);
      let num_vars = List.length remaining in
      let vars = List.mapi (fun i ty -> 
          let ty = S.Ty.apply S.Renaming.none subst (ty,scope) in
          T.bvar ~ty (num_vars-1-i)) remaining in
      let shifted = T.DB.shift num_vars t in
      T.app shifted vars in

    if List.length pref1 = List.length pref2 then (t1, t2, pref1)
    else (
      let n1, n2 = List.length pref1, List.length pref2 in 
      if n1 < n2 then (do_exp_otf n1 pref2 t1,t2,pref2)
      else (t1,do_exp_otf n2 pref1 t2,pref1))

  (* Perform some form of lazy normalization to bring term into solid
     form if that is possible (e.g. beta reducing applied variables) *)
  let solidify ?(limit=true) ?(exception_on_error=true) t =
    let rec aux t =
      (* right now working only on monomorphic terms *)
      if not (Type.is_ground (T.ty t)) then 
        raise NotInFragment;

      match T.view t with
      | AppBuiltin(hd, args) -> 
        let args' = List.map aux args in
        if T.same_l args args' then t 
        else T.app_builtin ~ty:(T.ty t) hd args'
      | App(hd, args) when not @@ T.is_var hd ->
        let hd' = aux hd in
        let args' = List.map aux args in
        if T.equal hd hd' && T.same_l args args' then t
        else T.app hd' args'
      | App(hd, args) ->
        assert (T.is_var hd);

        let solid_limit = get_op PUP.k_solidification_limit in
        if limit && List.length args > solid_limit then
          raise NotInFragment;

        let args' = List.map (fun arg -> 
            if Type.is_fun (T.ty arg) then (
              let arg = Lambda.eta_reduce arg in
              if (T.is_bvar arg && T.is_ground arg) || not exception_on_error then arg else raise NotInFragment
            ) else (
              let arg = Lambda.snf arg in
              if T.is_ground arg || not exception_on_error then arg else raise NotInFragment
            )) args in
        if T.same_l args args' then t
        else T.app hd args'
      | Fun (ty, body) ->
        let body' = aux body in
        T.fun_ ty body'
      | _ -> t in

    ZProf.enter_prof prof_solidifier;
    let res = aux t in
    ZProf.exit_prof prof_solidifier;
    res

  let all_combs ~combs_limit l =
    let rec aux = function 
      | [] -> []
      | x::xs ->
        let rest_combs = aux xs in
        if CCList.is_empty rest_combs then CCList.map (fun t->[t]) x 
        else CCList.flat_map 
            (fun i -> CCList.map (fun comb -> i::comb) rest_combs) 
            x
    in

    if CCList.for_all (fun l -> List.length l = 1 ) l then [CCList.flatten l]
    else (
      let rec limit_combinations max_c l = 
        if max_c <= 1 then CCList.map (fun l -> [List.hd l]) l
        else (match l with 
            | [] -> [] 
            | x :: xs -> 
              let n = List.length x in
              let x,max_c = 
                if n <= max_c then x, max_c / n
                else CCList.take (max max_c 1) x, 0 in
              x :: limit_combinations max_c xs) in
      match combs_limit with
      | None -> aux l
      | Some max -> aux (limit_combinations max l))

  let cover_rigid_skeleton ?(covers_limit = None) t solids =
    assert(List.for_all T.is_ground solids);
    (* If the term is not of base type, then it must be a bound variable *)
    assert(List.for_all (fun t -> not @@ Type.is_fun @@ T.ty t || T.is_bvar t) solids);
    let n = List.length solids in
    let combs_limit = match covers_limit with 
      | None -> None
      | Some x -> if x < 0 then None else Some x in

    (* if we are dealing with polymorphic constant it will be of the form 
        App(cst, tyargs) and with current design we might run into endless loop.
        That is why we set recurse to false, to run this function at most once.  *)
    let rec aux ?(recurse=true) ~depth s_args t : (T.t list)  =
      (* All the ways in which we can represent term t using solids *)
      let sols_as_db = List.mapi (fun i t -> 
          (t,T.bvar ~ty:(T.ty t) (n-i-1+depth))) s_args in
      let db_hits = 
        (CCList.filter_map (fun (s, s_db) -> 
             if T.equal s t then Some s_db else None) 
            sols_as_db) in
      let rest =
        try 
          match T.view t with
          | AppBuiltin (hd,args)  ->
            if CCList.is_empty args then [t]
            else (
              let args_combined = all_combs ~combs_limit (List.map (aux ~depth s_args) args) in
              List.map (T.app_builtin ~ty:(T.ty t) hd) args_combined
            )
          | App(hd,args) when recurse ->
            if Term.is_var hd then [t]
            else (
              assert(not (CCList.is_empty args));
              let hd, args = T.head_term_mono t, CCList.drop_while T.is_type args in
              let covered_hd = aux ~recurse:(recurse && not (T.is_app hd)) ~depth s_args hd in
              let covered_args = List.map (aux ~depth s_args) args in
              let cobmined = all_combs ~combs_limit (covered_hd :: covered_args) in
              List.map (fun l -> T.app (List.hd l) (List.tl l)) cobmined)
          | Fun _ -> 
            let ty_args, body = T.open_fun t in
            let d_inc = List.length ty_args in
            let s_args' = List.map (T.DB.shift d_inc) s_args in
            let res = aux ~depth:(depth+d_inc) s_args' body in
            List.map (fun t -> T.fun_l ty_args t) res
          | DB i when i >= depth -> []
          | _ -> [t]
        with CoveringImpossible -> [] in
      if CCList.is_empty db_hits && CCList.is_empty rest 
      then raise CoveringImpossible
      else CCList.interleave db_hits rest in

    try
      ZProf.enter_prof prof_cover_rigid;
      let res = aux ~depth:0 solids t in
      ZProf.exit_prof prof_cover_rigid;
      res
    with CoveringImpossible -> []

  let collect_flex_flex ~counter ~flex_args t =
    let replace_var ~bvar_tys ~target =
      if Type.returns_tType (T.ty target) || CCList.is_empty flex_args && CCList.is_empty (T.args target)
      then target,[]
      else (
        let bvars = 
          List.mapi (fun i ty -> (i,ty)) bvar_tys
          |> List.rev_map (fun (i,ty) -> T.bvar ~ty i) in
        let n_bvars = List.length bvars in
        let args' = (List.map (T.DB.shift n_bvars) flex_args) @ bvars in
        let fresh_var_ty = Type.arrow (List.map T.ty args') (T.ty target) in
        let fresh_var = HVar.fresh_cnt ~counter ~ty:fresh_var_ty () in
        let unif_w_target = T.app (T.var fresh_var) args' in
        let num_f_args = List.length flex_args in
        let flex_args_db =
          List.mapi (fun i a -> T.bvar ~ty: (T.ty a) (n_bvars+(num_f_args-1-i))) flex_args in
        let replacement = T.app (T.var fresh_var) (flex_args_db @ bvars) in
        replacement, [unif_w_target, target]
      ) in

    let rec aux ~bvar_tys t =
      match T.view t with
      | AppBuiltin (hd,args) ->
        let args', cons = List.fold_right (fun arg (acc_args, acc_cons) -> 
            let arg', cons' = aux ~bvar_tys arg in 
            (arg'::acc_args), (cons'@acc_cons)
          ) args ([],[]) in
        T.app_builtin ~ty:(T.ty t) hd args', cons  
      | App(hd,args) ->
        if Term.is_var hd then (
          replace_var ~bvar_tys ~target:t
        ) else (
          let hd', cons_hd = aux ~bvar_tys hd in
          let args', cons_args = List.fold_right (fun arg (acc_args, acc_cons) -> 
              let arg', cons' = aux ~bvar_tys arg in 
              arg'::acc_args, cons'@acc_cons
            ) args ([],[]) in
          T.app hd' args', (cons_hd @ cons_args)
        )
      | Fun (ty, body) -> 
        let bvar_tys = ty :: bvar_tys in
        let body', cons = aux ~bvar_tys body in
        T.fun_ ty body', cons
      | Var _ ->
        replace_var ~bvar_tys ~target:t
      | _ -> t, [] in

    aux ~bvar_tys:[] t

  let solve_flex_flex_diff ~subst ~counter ~scope lhs rhs =
    ZProf.enter_prof prof_flex_diff;
    let lhs = solidify @@ Lambda.whnf @@ Subst.FO.apply Subst.Renaming.none subst (lhs,scope) in 
    let rhs = solidify @@ Lambda.whnf @@ Subst.FO.apply Subst.Renaming.none subst (rhs,scope) in
    assert(Type.equal (Term.ty lhs) (Term.ty rhs));
    let pref_lhs, lhs = T.open_fun lhs and  pref_rhs, rhs = T.open_fun rhs in
    let lhs,rhs,_ = eta_expand_otf ~subst ~scope pref_lhs pref_rhs lhs rhs in

    let hd_l, args_l, n_l = 
      T.as_var_exn @@ T.head_term lhs, T.args lhs, List.length @@ T.args lhs in
    let hd_r, args_r, n_r = 
      T.as_var_exn @@ T.head_term rhs, T.args rhs, List.length @@ T.args rhs in
    assert(not @@ HVar.equal Type.equal hd_l hd_r);

    let cover_rigid_skeleton =
      let limit = get_op PUP.k_max_unifs_solid_ff in
      cover_rigid_skeleton ~covers_limit:(Some limit) in

    let res = 
      if CCList.is_empty args_l && CCList.is_empty args_r then (
        let res = Subst.FO.bind' subst (hd_l,scope) (rhs,scope) in
        US.of_subst res
      )
      else (
        let covered_l =
          CCList.flatten (List.mapi (fun i arg -> 
              let arg_covers = cover_rigid_skeleton arg args_r in
              let n = List.length arg_covers in
              List.combine 
                (CCList.replicate n (T.bvar (n_l-i-1) ~ty:(T.ty arg))) 
                arg_covers) 
              args_l) in
        let covered_r = 
          CCList.flatten (List.mapi (fun i arg -> 
              let arg_covers = cover_rigid_skeleton arg args_l in
              let n = List.length arg_covers in
              List.combine 
                arg_covers
                (CCList.replicate n (T.bvar (n_r-i-1) ~ty:(T.ty arg))))
              args_r) in
        let all_covers = covered_l @ covered_r in
        assert (List.for_all (fun (l_arg, r_arg) -> 
            Type.equal (Term.ty l_arg) (Term.ty r_arg)) all_covers);
        let fresh_var_ty = Type.arrow (List.map (fun (a_l, _) -> T.ty a_l ) all_covers) (T.ty lhs) in
        let fresh_var = T.var (HVar.fresh_cnt ~counter ~ty:fresh_var_ty ()) in
        let subs_l = T.fun_l (List.map T.ty args_l) (T.app fresh_var (List.map fst all_covers)) in
        let subs_r = T.fun_l (List.map T.ty args_r) (T.app fresh_var (List.map snd all_covers)) in
        let subst = Subst.FO.bind' subst (hd_l, scope) (subs_l,scope) in
        let subst = Subst.FO.bind' subst (hd_r, scope) (subs_r,scope) in
        US.of_subst subst) in
    ZProf.exit_prof prof_flex_diff;
    res

  let solve_flex_flex_same ~subst ~counter ~scope lhs rhs =
    ZProf.enter_prof prof_flex_same;
    let lhs = solidify @@ Lambda.whnf @@ Subst.FO.apply Subst.Renaming.none subst (lhs,scope) in 
    let rhs = solidify @@ Lambda.whnf @@ Subst.FO.apply Subst.Renaming.none subst (rhs,scope) in
    assert(Type.equal (Term.ty lhs) (Term.ty rhs));
    let pref_lhs, lhs = T.open_fun lhs and  pref_rhs, rhs = T.open_fun rhs in
    let lhs,rhs,_ = eta_expand_otf ~subst ~scope pref_lhs pref_rhs lhs rhs in

    let hd_l, args_l, n_l = 
      T.as_var_exn @@ T.head_term lhs, T.args lhs, List.length @@ T.args lhs in
    let hd_r, args_r, n_r = 
      T.as_var_exn @@ T.head_term rhs, T.args rhs, List.length @@ T.args rhs in

    let res = 
      if CCList.is_empty args_l && CCList.is_empty args_r then (
        US.of_subst subst
      ) else (
        assert(HVar.equal Type.equal hd_l hd_r);
        assert(n_l = n_r);
        let same_args = 
          List.combine args_l args_r
          |> List.mapi (fun i (a,b) -> if T.equal a b then Some (T.bvar ~ty:(T.ty a) (n_l-i-1)) else None)
          |> CCList.filter_map CCFun.id in


        let fresh_var_ty = Type.arrow (List.map (fun t -> T.ty t) same_args) (T.ty lhs) in
        let fresh_var = T.var (HVar.fresh_cnt ~counter ~ty:fresh_var_ty ()) in
        let subs_val = T.fun_l (List.map T.ty args_l) (T.app fresh_var same_args) in
        let subst = Subst.FO.bind' subst (hd_l,scope) (subs_val,scope) in
        US.of_subst subst) in
    ZProf.exit_prof prof_flex_same;
    res


  let cover_flex_rigid ~subst ~counter ~scope flex rigid =
    ZProf.enter_prof prof_flex_rigid;
    assert(T.is_var (T.head_term flex));
    assert(not @@ T.is_app_var rigid);

    let rigid = Lambda.snf @@ Subst.FO.apply Subst.Renaming.none subst (rigid, scope) in
    let rigid_orig = rigid in
    let flex, rigid = solidify flex, solidify rigid in
    let flex_args = T.args flex in
    let to_bind, flex_constraints = collect_flex_flex ~counter ~flex_args rigid in

    let subst = List.fold_left (fun subst (lhs,rhs) -> 
        US.subst (solve_flex_flex_diff ~subst ~counter ~scope lhs rhs)
      ) subst flex_constraints in

    let covers_limit = Some (2 * get_op PUP.k_max_inferences) in
    let rigid_covers = cover_rigid_skeleton ~covers_limit to_bind flex_args in
    let res = 
      if CCList.is_empty rigid_covers then (
        raise NotUnifiable
      ) else (
        let head_var = T.as_var_exn @@ T.head_term flex in

        if CCList.is_empty flex_args then (
          (* avoid creating fresh vars *)
          let rigid = List.hd rigid_covers in
          if not  (Term.DB.is_closed rigid) then (
            CCFormat.printf "@[%a@]=?=@[%a@]@ has bound vars in RHS@." T.pp flex T.pp rigid_orig;
            assert(false)
          );
          assert (List.length rigid_covers = 1);

          let res = Subst.FO.bind' subst (head_var,scope) (rigid,scope) in
          [US.of_subst res]
        ) else (
          let tys = List.map T.ty flex_args in
          List.map (fun r ->
              let closed_rigid = T.fun_l tys r in
              assert(T.DB.is_closed closed_rigid);
              let subs_flex = Subst.FO.bind' subst (head_var,scope) (closed_rigid,scope) in
              US.of_subst subs_flex
            ) rigid_covers)
      ) in
    ZProf.exit_prof prof_flex_rigid;
    res

  let var_conditions s t = 
    (T.is_linear s || T.is_linear t) &&
    T.VarSet.is_empty (T.VarSet.inter (T.vars s) (T.vars t))

  let norm t =
    if Term.is_fun (T.head_term t)
    then Lambda.whnf t else t

  let rec norm_deref subst (t,sc) =
    let pref, tt = T.open_fun t in
    let t' =  
      begin match T.view tt with
        | T.Var _ ->
          let u, _ = US.FO.deref subst (tt,sc) in
          if T.equal tt u then u
          else norm_deref subst (u,sc)
        | T.App (f0, l) ->
          let f = norm_deref subst (f0, sc) in
          let t =
            if T.equal f0 f then tt else T.app f l in
          let u = norm t in
          if T.equal t u
          then t
          else norm_deref subst (u,sc)
        | _ -> tt
      end in
    if T.equal tt t' then t
    else T.fun_l pref t'

  let build_constraints args1 args2 rest =
    let rf, other = 
      CCList.combine args1 args2
      |> CCList.partition (fun (s,t) -> T.is_const (T.head_term s) && T.is_const (T.head_term t)) in
    rf @ rest @ other

  let project_flex_rigid ~subst ~scope flex rigid =
    assert(T.is_var (T.head_term flex));
    let flex_var, flex_args = T.as_var_exn @@ T.head_term flex,
                              List.mapi (fun i a -> i,a) (T.args flex) in
    let pref_tys, ret_ty = Type.open_fun @@ HVar.ty flex_var in
    let n = List.length pref_tys in
    let trailing_tys = CCList.drop (List.length flex_args) pref_tys in
    let rigid_hd = T.head rigid in
    match rigid_hd with 
    | None -> []
    | Some id -> 
      CCList.filter_map ( fun (i, fa) ->
          begin match T.head fa with 
            | Some id' when ID.equal id id' && Type.equal (T.ty fa) ret_ty ->
              let subs_term = T.fun_l pref_tys (T.bvar ~ty:ret_ty (n-1-i)) in
              let subst = Subst.FO.bind' subst (flex_var, scope) (subs_term, scope) in
              let flex_arg = if CCList.is_empty trailing_tys then fa else T.fun_l trailing_tys fa in
              assert(Type.equal (T.ty flex_arg) (T.ty rigid));
              Some (subst, flex_arg, rigid)
            | _ -> None end
        ) flex_args

  let rec unify ~scope ~counter ~subst constraints =
    match constraints with
    | [] -> [subst]
    | (s,t) :: rest -> 

      if not (Type.equal (T.ty s) (T.ty t)) then (
        raise NotInFragment
      );

      let s', t' = norm_deref subst (s,scope), norm_deref subst (t,scope) in
      if not (Term.equal s' t') then (
        let pref_s, body_s = T.open_fun s' in
        let pref_t, body_t = T.open_fun t' in 
        let body_s', body_t', _ = eta_expand_otf ~subst:(US.subst subst) ~scope pref_s pref_t body_s body_t in
        let hd_s, args_s = T.as_app body_s' in
        let hd_t, args_t = T.as_app body_t' in
        match T.view hd_s, T.view hd_t with 
        | (T.Var _, T.Var _) ->
          let solver = 
            if not (T.equal hd_s hd_t)
            then solve_flex_flex_diff
            else solve_flex_flex_same in
          let subst = solver ~subst:(US.subst subst) ~scope ~counter body_s' body_t' in
          unify ~scope ~counter ~subst rest
        | (T.Var _, _) ->
          let projected = project_flex_rigid ~subst:(US.subst subst) ~scope body_s' body_t' in
          let covered = cover_flex_rigid ~subst:(US.subst subst) ~counter ~scope  body_s' body_t' in
          (CCList.flat_map (fun subst -> unify ~scope ~counter ~subst rest) covered) @
          (CCList.flat_map (fun (subst,s,t) -> unify ~scope ~counter ~subst:(US.of_subst subst) ((s,t)::rest)) projected)
        | (_, T.Var _) ->
          let projected = project_flex_rigid ~subst:(US.subst subst) ~scope body_t' body_s' in
          let covered = cover_flex_rigid ~subst:(US.subst subst) ~counter ~scope  body_t' body_s' in
          (CCList.flat_map (fun subst -> unify ~scope ~counter ~subst rest) covered) @ 
          (CCList.flat_map (fun (subst,s,t) -> unify ~scope ~counter ~subst:(US.of_subst subst) ((s,t)::rest)) projected)
        | T.AppBuiltin(hd_s, args_s'), T.AppBuiltin(hd_t, args_t') when
            Builtin.equal hd_s hd_t &&
            List.length args_s' + List.length args_s = 
            List.length args_t' + List.length args_t ->
          let args_lhs, args_rhs = 
            Unif.norm_logical_disagreements hd_s (args_s'@args_s) (args_t'@args_t) in
          unify ~subst ~counter ~scope @@ build_constraints args_lhs args_rhs rest
        | T.Const f , T.Const g when ID.equal f g && List.length args_s = List.length args_t ->
          unify ~subst ~counter ~scope @@ build_constraints args_s args_t rest
        | T.DB i, T.DB j when i = j && List.length args_s = List.length args_t ->
          unify ~subst ~counter ~scope @@ build_constraints args_s args_t rest
        | _ -> raise NotUnifiable) 
      else (
        unify ~subst ~counter ~scope rest
      )


  let unify_scoped ?(subst=US.empty) ?(counter = ref 0) t0_s t1_s =
    try 
      let res = 
        if US.is_empty subst then (
          let t0',t1',scope,subst = US.FO.rename_to_new_scope ~counter t0_s t1_s in
          if var_conditions t0' t1' then (
            let t0',t1' = solidify t0', solidify t1' in
            unify ~scope ~counter ~subst [(t0', t1')])
          else raise NotInFragment
        )
        else (
          if Scoped.scope t0_s != Scoped.scope t1_s then (
            raise (Invalid_argument "scopes should be the same")
          )
          else (
            let t0', t1' = US_A.apply subst t0_s, US_A.apply subst t1_s in
            if var_conditions t0' t1' then (
              let t0',t1' = solidify t0', solidify t1' in
              unify ~scope:(Scoped.scope t0_s) ~counter ~subst [(t0', t1')]
            )
            else (raise NotInFragment)
          )
        ) 
      in

      assert(CCList.for_all (fun sub -> 
          let norm t = Lambda.eta_reduce @@ Lambda.snf t in
          let lhs_o = norm @@ US_A.apply subst t0_s and rhs_o = norm @@ US_A.apply subst t1_s in
          (* CCFormat.printf "l_o:@[%a@];r_0:@[%a@]@.sub:@[%a@]@." T.pp lhs_o T.pp rhs_o US.pp sub; *)
          let lhs = norm @@ US_A.apply sub t0_s and rhs = norm @@ US_A.apply sub t1_s in
          if T.equal lhs rhs then true
          else (
            CCFormat.printf "orig: @[%a@]=?=@[%a@]@." (Scoped.pp T.pp) t0_s (Scoped.pp T.pp) t1_s ;
            CCFormat.printf "orig_sub: @[%a@]@." US.pp subst;
            CCFormat.printf "orig_app: @[%a@]=?=@[%a@]@." (T.pp) lhs_o (T.pp) rhs_o ;
            CCFormat.printf "new_sub: @[%a@]@." US.pp sub ;
            CCFormat.printf "res: @[%a@]=?=@[%a@]@." T.pp lhs T.pp rhs ;
            false
          )) res);
      res    
    with NotInFragment ->
      (* let norm t = Lambda.eta_reduce @@ Lambda.snf t in
         let lhs_o = norm @@ US_A.apply subst t0_s and rhs_o = norm @@ US_A.apply subst t1_s in *)
      (* CCFormat.printf "@[%a@]=?=@[%a@]@ is not solid@." T.pp lhs_o T.pp rhs_o; *)
      raise NotInFragment
       | NotUnifiable ->
         (* let norm t = Lambda.eta_reduce @@ Lambda.snf t in
            let lhs_o = norm @@ US_A.apply subst t0_s and rhs_o = norm @@ US_A.apply subst t1_s in *)
         (* CCFormat.printf "@[%a@]=?=@[%a@]@ is not unifiable@." T.pp lhs_o T.pp rhs_o; *)
         raise NotUnifiable
end
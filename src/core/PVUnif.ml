module T = Term
module US = Unif_subst

type subst = US.t

module S = struct

  let apply s t = Subst.FO.apply Subst.Renaming.none (US.subst s) t

  let apply_ty s ty = Subst.Ty.apply Subst.Renaming.none (US.subst s) ty

end


let make_fresh_var fresh_var_ ~ty () = 
  let var = HVar.make ~ty !fresh_var_ in 
  incr fresh_var_; 
  var

(* apply a substitution and reduce to normal form *)
let nfapply s u = Lambda.snf (S.apply s u)

let unif_simple ~scope t s = 
  try 
    let type_unifier = Unif.FO.unify_syn ~subst:Subst.empty (t, scope) (s, scope) in
    Some (US.of_subst type_unifier)
  with Unif.Fail -> None

let eta_expand_otf pref1 pref2 t1 t2 =
  let do_exp_otf n types t = 
    let remaining = CCList.drop n types in
    let vars = List.mapi (fun i ty -> T.bvar ~ty (n-1-i)) remaining in
    let shifted = T.DB.shift n t in
    T.app shifted vars in 

  let t1',t2' = 
    if List.length pref1 = List.length pref2 then (t1, t2)
    else (
      let n1, n2 = List.length pref1, List.length pref2 in 
      if n1 < n2 then (
        (do_exp_otf (n2-n1) pref2 t1,t2)
      ) else (
        assert(n1 > n2);
        (t1,do_exp_otf (n1-n2) pref1 t2)
      )
    ) in
    assert(List.length (T.args t1') == List.length (T.args t2'));
    (t1',t2')

let eliminate_at_idx ~scope ~fresh_var_ v k =  
  (* create substitution: v |-> λ u1 ... um. x u1 ... u{k-1} u{k+1} ... um *)
  let prefix_types, return_type = Type.open_fun (HVar.ty v) in
  let m = List.length prefix_types in
  let bvars = List.mapi (fun i ty -> T.bvar ~ty (m-1-i)) prefix_types in
  let prefix_types' = CCList.remove_at_idx k prefix_types in
  let bvars' = CCList.remove_at_idx k bvars in
  let matrix_head = T.var (make_fresh_var fresh_var_ ~ty:(Type.arrow prefix_types' return_type) ()) in
  let matrix = T.app matrix_head bvars' in
  let subst_value = T.fun_l prefix_types matrix in
  let subst = US.FO.singleton (v, scope) (subst_value, scope) in
  subst
    
let project_hs_one ~fresh_var_ pref_types i type_ui =
  let pref_types_ui, _ = Type.open_fun type_ui in
  let n_args_free = List.length pref_types in
  let pref_args = pref_types |> List.mapi (fun i ty -> T.bvar ~ty (n_args_free-i-1)) in
  let new_vars = pref_types_ui |> List.map (fun ty -> T.var (make_fresh_var fresh_var_ ~ty:(Type.arrow pref_types ty) () )) in
  let new_vars_applied = new_vars |> List.map (fun nv -> T.app nv pref_args) in
  let matrix_hd = T.bvar ~ty:type_ui (n_args_free-i-1) in
  let matrix = T.app matrix_hd new_vars_applied in
  Lambda.eta_expand @@ T.fun_l pref_types matrix

let imitate_one ~scope ~fresh_var_ s t = 
  OSeq.nth 0 (JP_unif.imitate_onesided ~scope ~fresh_var_ s t)

let rec unify ~scope ~fresh_var_ ~subst = function
  | [] -> OSeq.return subst
  | (s,t) :: rest -> (
      let s', t' = nfapply subst (s, scope), nfapply subst (t, scope) in 
      match unif_simple ~scope (T.of_ty (T.ty s')) (T.of_ty (T.ty t')) with
      | Some ty_unif -> (
        let s' = nfapply ty_unif (s, scope) in
        let t' = nfapply ty_unif (t, scope) in
        let subst' = US.merge subst ty_unif in
        if Lambda.is_lambda_pattern s' && Lambda.is_lambda_pattern t' then (
          match unif_simple ~scope s' t' with
          | Some unif -> 
              let subst = US.merge subst' unif in
              unify ~scope ~fresh_var_ ~subst rest
          | None -> OSeq.empty
        )
        else (
          let pref_s, body_s = T.open_fun s' in
          let pref_t, body_t = T.open_fun t' in 
          let body_s', body_t' = eta_expand_otf pref_s pref_t body_s body_t in
          let hd_s, args_s = T.as_app body_s' in
          let hd_t, args_t = T.as_app body_t' in
          match T.view hd_s, T.view hd_t with 
          | (T.Var _, T.Var _) -> 
            if T.equal hd_s hd_t then
              flex_same ~subst ~fresh_var_ ~scope hd_s args_s args_t rest
            else
              identify ~subst ~fresh_var_ ~scope s' t' rest
          | (T.Var _, T.Const _) | (T.Var _, T.DB _) ->
              flex_rigid ~subst ~fresh_var_ ~scope body_s' body_t' rest
          | (T.Const _, T.Var _) | (T.DB _, T.Var _) ->
              flex_rigid ~subst ~fresh_var_ ~scope body_t' body_s' rest
          | T.Const f , T.Const g when ID.equal f g ->
              assert(List.length args_s = List.length args_t);
              unify ~subst ~fresh_var_ ~scope ((List.combine args_s args_t) @ rest)
          | T.DB i, T.DB j when i = j ->
              assert(List.length args_s = List.length args_t);
              unify ~subst ~fresh_var_ ~scope ((List.combine args_s args_t) @ rest)
          | _ -> OSeq.empty
        )
      )
      | None -> OSeq.empty)
and identify ~subst ~fresh_var_ ~scope s t rest = 
  let id_subs = OSeq.nth 0 (JP_unif.identify ~scope ~fresh_var_ s t []) in
  let subs_res = US.merge subst id_subs in
  unify ~scope ~fresh_var_ ~subst:subs_res ((s,t)::rest)
and flex_rigid ~subst ~fresh_var_ ~scope s t rest =
  assert (T.is_var @@ T.head_term s);
  assert (not @@ T.is_var @@ T.head_term t);
  let hd_s = T.as_var_exn @@ T.head_term s in
  let prefix_types, var_ret_ty = Type.open_fun (HVar.ty hd_s) in
  let proj_bindings = 
    prefix_types 
    |> List.mapi (fun i ty ->
        let _, arg_ret_ty = Type.open_fun ty in 
        match unif_simple ~scope (T.of_ty arg_ret_ty) (T.of_ty var_ret_ty) with
        | Some ty_unif -> 
          let pr_bind = project_hs_one ~fresh_var_ prefix_types i 
                        (S.apply_ty ty_unif (ty, scope)) in
            Some (US.FO.bind ty_unif (hd_s, scope) (pr_bind, scope))
        | None -> None) 
    |> CCList.filter_map (fun x -> x) in
    let imit_binding = imitate_one ~scope ~fresh_var_ s t in
    let substs = proj_bindings @ [imit_binding] in
    OSeq.of_list substs
    |> OSeq.flat_map (fun subst -> unify ~scope  ~fresh_var_ ~subst ((s,t) :: rest))
and flex_same ~subst ~fresh_var_ ~scope hd_s args_s args_t rest =
  assert(T.is_var hd_s);
  let new_cstrs = (List.combine args_s args_t) @ rest in
  let all_vars = CCList.range 0 ((List.length @@ fst @@ Type.open_fun (T.ty hd_s)) -1 ) in
  OSeq.append 
    (unify ~subst ~fresh_var_ ~scope new_cstrs)
    (OSeq.of_list all_vars |>
     OSeq.map (fun idx -> 
       idx, eliminate_at_idx ~scope ~fresh_var_ (T.as_var_exn hd_s) idx ) 
     |>  
      (OSeq.flat_map (fun (idx, subst') -> 
      let new_subst = US.merge subst subst' in
        unify ~scope  ~fresh_var_ ~subst:new_subst 
        (CCList.remove_at_idx idx new_cstrs))))


let unify_scoped (t0, scope0) (t1, scope1) =
    (* Find a scope that's different from the two given ones *)
    let unifscope = if scope0 < scope1 then scope1 + 1 else scope0 + 1 in
    let fresh_var_ = ref 0 in
    let add_renaming scope subst v =
    if US.FO.mem subst (v,scope) 
    then subst
    else 
        let newvar = T.var (make_fresh_var fresh_var_ ~ty:(S.apply_ty subst (HVar.ty v, scope)) ()) in
        US.FO.bind subst (v,scope) (newvar, unifscope) 
    in
    let subst = US.empty in
    (* Rename variables apart into scope `unifscope` *)
    let subst = T.Seq.vars t0 |> Sequence.fold (add_renaming scope0) subst in
    let subst = T.Seq.vars t1 |> Sequence.fold (add_renaming scope1) subst in
    (* Unify *)
    unify ~scope:unifscope ~fresh_var_ ~subst [S.apply subst (t0, scope0), S.apply subst (t1, scope1)]
    (* merge with var renaming *)
    |> OSeq.map (fun s -> Some (US.merge s subst))

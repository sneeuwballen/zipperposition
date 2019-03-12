module T = Term
module US = Unif_subst

type subst = US.t

module S = struct

  let apply s t = Subst.FO.apply Subst.Renaming.none (US.subst s) t

  let apply_ty s ty = Subst.Ty.apply Subst.Renaming.none (US.subst s) ty
  let pp = US.pp

end

let max_depth = 20

let _conservative_elim = ref false
let _imit_first = ref false
let _cons_ff = ref false


let make_fresh_var fresh_var_ ~ty () = 
  let var = HVar.make ~ty !fresh_var_ in 
  incr fresh_var_; 
  var

(* apply a substitution and reduce to normal form *)
let nfapply s u = Lambda.beta_red_head (S.apply s u)

let enable_conservative_elim () =
  _conservative_elim := true

let set_imit_first () = 
  _imit_first := true

let set_cons_ff () = 
  _cons_ff := true


let unif_simple ~scope t s = 
  try 
    let type_unifier = Unif.FO.unify_syn ~subst:Subst.empty (t, scope) (s, scope) in
    Some (US.of_subst type_unifier)
  with Unif.Fail -> None

let eta_expand_otf pref1 pref2 t1 t2 =
  let do_exp_otf n types t = 
    let remaining = CCList.drop n types in
    assert(List.length remaining != 0);
    let num_vars = List.length remaining in
    let vars = List.mapi (fun i ty -> T.bvar ~ty (num_vars-1-i)) remaining in
    let shifted = T.DB.shift num_vars t in
    T.app shifted vars in 

  if List.length pref1 = List.length pref2 then (t1, t2, pref1)
  else (
    let n1, n2 = List.length pref1, List.length pref2 in 
    if n1 < n2 then (
      (do_exp_otf n1 pref2 t1,t2,pref2)
    ) else (
      assert(n1 > n2);
      (t1,do_exp_otf n2 pref1 t2,pref1)
    )
  )

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
  T.fun_l pref_types matrix

let imitate_one ~scope ~fresh_var_  s t =
  try
    OSeq.nth 0 (JP_unif.imitate_onesided ~scope ~fresh_var_ s t)
  with Not_found -> Format.printf "Could not get imitation subst!\n"; raise Not_found

let proj_imit_bindings ~scope ~fresh_var_  s t = 
  let hd_s = T.as_var_exn @@ T.head_term s in
    let prefix_types, var_ret_ty = Type.open_fun (HVar.ty hd_s) in
    let proj_bindings = 
      prefix_types 
      |> List.mapi (fun i ty ->
            let _, arg_ret_ty = Type.open_fun ty in 
            match unif_simple ~scope (T.of_ty arg_ret_ty) (T.of_ty var_ret_ty) with
            | Some ty_unif -> 
              let pr_bind = project_hs_one ~fresh_var_ 
                            (List.map (fun typ -> S.apply_ty ty_unif (typ, scope)) prefix_types) i 
                            (S.apply_ty ty_unif (ty, scope)) in
              let hd_s = HVar.cast hd_s ~ty:(S.apply_ty ty_unif (HVar.ty hd_s, scope)) in
                Some (US.FO.bind ty_unif (hd_s, scope) (pr_bind, scope))
            | None -> None)
      |> CCList.filter_map (fun x -> x) in
      let imit_binding =
        let hd_s = T.head_term_mono s in 
        let hd_t = T.head_term_with_mandatory_args t in
        if (not @@ T.is_bvar @@ T.head_term t && 
            not (T.var_occurs ~var:(T.as_var_exn hd_s) hd_t)) 
          then [imitate_one ~scope ~fresh_var_ s t] 
        else [] in
  let first, second = 
    if !_imit_first then imit_binding , proj_bindings
    else proj_bindings, imit_binding in 
  first @ second

let rec unify ~depth ~scope ~fresh_var_ ~subst = function
  | [] -> OSeq.return (Some subst)
  | (s,t,ban_id) :: rest as l -> (
    if depth >= max_depth then
      OSeq.empty
    else 
      if (depth > 0 && depth mod 4 = 0) then
        if (depth < max_depth) then 
          OSeq.append 
            (OSeq.take 50 (OSeq.repeat None))
            (unify ~depth:(depth+1) ~scope ~fresh_var_ ~subst l)
        else OSeq.empty
      else  
        let s', t' = nfapply subst (s, scope), nfapply subst (t, scope) in
        match unif_simple ~scope (T.of_ty (T.ty s')) (T.of_ty (T.ty t')) with
        | Some ty_unif -> (
          let s' = nfapply ty_unif (s', scope) in
          let t' = nfapply ty_unif (t', scope) in

          let subst = US.merge subst ty_unif in

          (* Format.printf "Solving pair:\n@[%a@]\n=?=\n@[%a@].\n" T.pp s' T.pp t'; *)
          (* Format.print_flush (); *)

          if Lambda.is_lambda_pattern s' && Lambda.is_lambda_pattern t' &&
            T.DB.is_closed s' && T.DB.is_closed t' then (
            (* Format.printf "Solving lambda pattern.\n"; *)
            match unif_simple ~scope s' t' with
            | Some unif ->
                let subst = US.merge subst unif in
                unify ~depth:(depth+1) ~scope ~fresh_var_ ~subst rest
            | None -> OSeq.empty
          )
          else (
            let pref_s, body_s = T.open_fun s' in
            let pref_t, body_t = T.open_fun t' in 
            (* Format.printf "Trying to eta expand.\n"; *)
            let body_s', body_t', _ = eta_expand_otf pref_s pref_t body_s body_t in
            (* Format.printf "After eta_expand %a =?= %a\n" T.pp body_s' T.pp body_t'; *)
            let hd_s, args_s = T.as_app body_s' in
            let hd_t, args_t = T.as_app body_t' in
            match T.view hd_s, T.view hd_t with 
            | (T.Var _, T.Var _) -> 
              (* Format.printf "Var var!"; *)
              if T.equal hd_s hd_t then
                  flex_same ~depth ~subst ~fresh_var_ ~scope hd_s args_s args_t rest
              else (
                if ban_id then
                  (flex_proj_imit ~depth ~subst ~fresh_var_ ~scope  body_s' body_t' rest)
                else (
                  OSeq.append
                  (identify  ~depth ~subst ~fresh_var_ ~scope body_s' body_t' rest)
                  (if not !_cons_ff then (flex_proj_imit  ~depth ~subst ~fresh_var_ ~scope  body_s' body_t' rest)
                  else OSeq.empty)
                )
              ) 
            | (T.Var _, T.Const _) | (T.Var _, T.DB _) ->
                (* Format.printf "Var const/Var db!\n"; *)
                flex_rigid ~depth ~subst ~fresh_var_ ~scope ~ban_id body_s' body_t' rest
            | (T.Const _, T.Var _) | (T.DB _, T.Var _) ->
                (* Format.printf "Const var/DB var!\n"; *)
                flex_rigid ~depth ~subst ~fresh_var_ ~scope ~ban_id body_t' body_s' rest
            | T.Const f , T.Const g when ID.equal f g ->
                (* Format.printf "Same fun symb heads.\n"; *)
                assert(List.length args_s = List.length args_t);
                (*  depth stays the same for the decomposition steps   *)
                unify ~depth ~subst ~fresh_var_ ~scope 
                  ((List.map (fun (a,b) -> a,b,ban_id) (List.combine args_s args_t)) @ rest)
            | T.DB i, T.DB j when i = j ->
                (* Format.printf "Same DB heads.\n"; *)
                assert (List.length args_s = List.length args_t);
                unify ~depth ~subst ~fresh_var_ ~scope 
                  ((List.map (fun (a,b) -> a,b,ban_id) (List.combine args_s args_t)) @ rest)
            | _ -> OSeq.empty
          )
        )
        | None -> OSeq.empty)
and identify ~depth ~subst ~fresh_var_ ~scope s t rest =
  (* Format.printf "Getting identification subst for %a and %a!\n" T.pp s T.pp t; *)
  let id_subs = OSeq.nth 0 (JP_unif.identify ~scope ~fresh_var_ s t []) in
  (* Format.printf "Merging id \n"; *)
  let subs_res = US.merge subst id_subs in
  unify ~depth:(depth+1) ~scope ~fresh_var_ ~subst:subs_res 
    ((s, t, true)::rest)
and flex_rigid ~depth ~subst ~fresh_var_ ~scope ~ban_id s t rest =
  assert (T.is_var @@ T.head_term s);
  assert (not @@ T.is_var @@ T.head_term t);
  let bindings = proj_imit_bindings ~scope ~fresh_var_ s t in
  let substs = List.map (US.merge subst) bindings in
  OSeq.of_list substs
  |> OSeq.flat_map (fun subst -> unify ~depth:(depth+1) ~scope  ~fresh_var_ ~subst 
                                  ((s, t,ban_id) :: rest))
and flex_same ~depth ~subst ~fresh_var_ ~scope hd_s args_s args_t rest =
  assert(T.is_var hd_s);
  assert(List.length args_s = List.length args_t);
  let new_cstrs = (List.map (fun (a,b) -> a,b,true) (List.combine args_s args_t)) @ rest in
  let all_vars = CCList.range 0 ((List.length @@ fst @@ Type.open_fun (T.ty hd_s)) -1 ) in
  let all_args_unif = unify ~depth:(depth+1)~subst ~fresh_var_ ~scope new_cstrs in
  OSeq.append 
    all_args_unif
    (if(!_conservative_elim &&
        OSeq.exists CCOpt.is_some (OSeq.take 1 all_args_unif)) then
      OSeq.empty
    else (
      (OSeq.of_list all_vars |>
      OSeq.map (fun idx -> 
        idx, eliminate_at_idx ~scope ~fresh_var_ (T.as_var_exn hd_s) idx ) 
      |>  
        (OSeq.flat_map (fun (idx, subst') -> 
        let new_subst = US.merge subst subst' in
          unify ~depth:(depth+1) ~scope  ~fresh_var_ ~subst:new_subst 
          (CCList.remove_at_idx idx new_cstrs)))))
    )
and flex_proj_imit  ~depth ~subst ~fresh_var_ ~scope s t rest = 
  let bindings = proj_imit_bindings  ~scope ~fresh_var_ s t in
  let bindings = proj_imit_bindings  ~scope ~fresh_var_ t s @ bindings in
  let substs = List.map (US.merge subst) bindings in
  OSeq.of_list substs
  |> OSeq.flat_map (fun subst -> 
      unify ~depth:(depth+1) ~scope  ~fresh_var_ ~subst 
        ((s, t, true) :: rest))

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
    let t0', t1' = S.apply subst (t0, scope0), S.apply subst (t1, scope1) in
    (* Unify *)
    unify ~depth:0 ~scope:unifscope ~fresh_var_ ~subst [t0', t1', false]
    (* merge with var renaming *)
    (* |> OSeq.map (CCOpt.map (US.merge subst)) *)
    |> OSeq.map (CCOpt.map (fun sub -> 
      (* let l = Lambda.eta_expand @@ Lambda.snf @@ S.apply sub (t0, scope0) in 
      let r = Lambda.eta_expand @@ Lambda.snf @@ S.apply sub (t1, scope1) in
      if not (T.equal l r) then (
        Format.printf "For problem: %a =?= %a\n" T.pp t0 T.pp t1;
        Format.printf "Subst: %a\n" S.pp sub;
        Format.printf "%a <> %a\n" T.pp l T.pp r;
        assert(false);
      );
      if not (T.Seq.subterms l |> Sequence.append (T.Seq.subterms r) |> 
         Sequence.for_all (fun st -> List.for_all T.DB.is_closed @@ T.get_mand_args st)) then ( 
         Format.printf "Unequal subst: %a =?= %a, res %a.\n" T.pp t0 T.pp t1 T.pp l; 
         assert(false); 
      );
      assert (T.equal l r); *)
      sub))

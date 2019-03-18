module T = Term
module US = Unif_subst

type subst = US.t

module S = struct

  let apply s t = Subst.FO.apply Subst.Renaming.none (US.subst s) t

  let apply_ty s ty = Subst.Ty.apply Subst.Renaming.none (US.subst s) ty
  let pp = US.pp

end

let max_depth = 13

let _conservative_elim = ref true
let _imit_first = ref false
let _cons_ff = ref true
let _compose = ref false


let make_fresh_var fresh_var_ ~ty () = 
  let var = HVar.make ~ty !fresh_var_ in 
  incr fresh_var_; 
  var

(* apply a substitution and reduce to normal form *)
let nfapply s u = Lambda.beta_red_head (S.apply s u)

let disable_conservative_elim () =
  _conservative_elim := false

let set_imit_first () = 
  _imit_first := true

let disable_cons_ff () = 
  _cons_ff := true

let set_compose () = 
  _compose := true

let compose_sub ~scope s1 s2 =
  if !_compose then US.compose ~scope s1 s2
  else US.merge s1 s2

let build_constraints ~ban_id args1 args2 rest = 
  let zipped = List.map (fun (x,y) -> (x,y,ban_id)) (List.combine args1 args2) in
  let rigid, non_rigid = List.partition (fun (s,t,_) ->
    T.is_const (T.head_term s) || T.is_const (T.head_term t)) zipped in
  rigid @ rest @ non_rigid

let unif_simple ?(subst=Subst.empty) ~scope t s = 
  try 
    let type_unifier = Unif.FO.unify_syn ~subst (t, scope) (s, scope) in
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
  let new_ty = Type.arrow prefix_types' return_type in
  let bvars' = CCList.remove_at_idx k bvars in
  let matrix_head = T.var (make_fresh_var fresh_var_ ~ty:new_ty ()) in
  let matrix = T.app matrix_head bvars' in
  let subst_value = T.fun_l prefix_types matrix in
  let subst = US.FO.singleton (v, scope) (subst_value, scope) in
  subst
    
let project_hs_one ~fresh_var_ pref_types i type_ui =
  let pref_types_ui, _ = Type.open_fun type_ui in
  let n_args_free = List.length pref_types in
  let pref_args = pref_types 
                  |> List.mapi (fun i ty -> T.bvar ~ty (n_args_free-i-1)) in
  let new_vars = pref_types_ui 
                 |> List.map (fun ty ->
                    let new_ty =  (Type.arrow pref_types ty) in
                    T.var (make_fresh_var fresh_var_ ~ty:new_ty ())) in
  let new_vars_applied = new_vars |> List.map (fun nv -> T.app nv pref_args) in
  let matrix_hd = T.bvar ~ty:type_ui (n_args_free-i-1) in
  let matrix = T.app matrix_hd new_vars_applied in
  T.fun_l pref_types matrix

let imitate_one ~scope ~fresh_var_  s t =
  try
    OSeq.nth 0 (JP_unif.imitate_onesided ~scope ~fresh_var_ s t)
  with Not_found ->  raise Not_found

let proj_imit_bindings ~depth ~scope ~fresh_var_  s t = 
  let hd_s = T.as_var_exn @@ T.head_term s in
    let prefix_types, var_ret_ty = Type.open_fun (HVar.ty hd_s) in
    let proj_bindings = 
      prefix_types
      |> List.mapi (fun i ty -> i, ty)
      |> (fun l ->
        if depth <= 4 then l
        else
          (* if the depth is greater than 4, we only project to terms that will
             decrease the size of unification problem *)
          List.filter (fun (_, ty) -> List.length @@ Type.expected_args ty = 0) l)
      |> List.map (fun (i, ty) ->
          let _, arg_ret_ty = Type.open_fun ty in
          match unif_simple ~scope (T.of_ty arg_ret_ty) (T.of_ty var_ret_ty) with
          | Some ty_unif ->
            let pr_bind =
              project_hs_one ~fresh_var_ 
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
    if !_imit_first then imit_binding, proj_bindings
    else proj_bindings, imit_binding in 
  first @ second

let rec unify ~depth ~scope ~fresh_var_ ~subst = function
  | [] -> OSeq.return (Some subst)
  | (s,t,ban_id) :: rest as l -> (
    if T.equal s t then
      unify ~depth ~scope ~fresh_var_ ~subst rest
    else (
      if depth >= max_depth then
        OSeq.empty
      else 
        if (depth > 0 && depth mod 6 = 0) then
            OSeq.append 
              (OSeq.take 50 (OSeq.repeat None))
              (unify ~depth:(depth+1) ~scope ~fresh_var_ ~subst l)
        else  
          let s', t' = nfapply subst (s, scope), nfapply subst (t, scope) in
          match unif_simple ~scope (T.of_ty (T.ty s')) (T.of_ty (T.ty t')) with
          | Some ty_unif -> (
            let s' = nfapply ty_unif (s', scope) in
            let t' = nfapply ty_unif (t', scope) in
            let merged = compose_sub ~scope subst ty_unif in

            (* Format.printf "Solving: @[%a@] =?= @[%a@]\n" T.pp s' T.pp t'; *)
            (* Format.printf "Subst: %a" US.pp subst; *)

            if Term.is_fo_term s' && Term.is_fo_term t' then (
              match unif_simple ~subst:(US.subst subst) ~scope s' t' with
              | Some unif ->
                  unify ~depth ~scope ~fresh_var_ ~subst:(US.merge subst unif) rest
              | None -> OSeq.empty
            )
            else (
              let pref_s, body_s = T.open_fun s' in
              let pref_t, body_t = T.open_fun t' in 
              let body_s', body_t', _ = eta_expand_otf pref_s pref_t body_s body_t in
              let hd_s, args_s = T.as_app body_s' in
              let hd_t, args_t = T.as_app body_t' in
              match T.view hd_s, T.view hd_t with 
              | (T.Var _, T.Var _) -> 
                if T.equal hd_s hd_t then (
                  flex_same ~depth ~subst:merged ~fresh_var_ ~scope hd_s args_s args_t rest l
                )
                else (
                  if ban_id then
                    flex_proj_imit ~depth ~subst:merged ~fresh_var_ ~scope  body_s' body_t' rest
                  else (
                    OSeq.append
                    (identify  ~depth ~subst:merged ~fresh_var_ ~scope body_s' body_t' rest)
                    (if not !_cons_ff then 
                      (flex_proj_imit  ~depth ~subst:merged ~fresh_var_ ~scope  body_s' body_t' rest)
                    else 
                      OSeq.empty)
                  )
                ) 
              | (T.Var _, T.Const _) | (T.Var _, T.DB _) ->
                  flex_rigid ~depth ~subst:merged ~fresh_var_ ~scope ~ban_id body_s' body_t' rest
              | (T.Const _, T.Var _) | (T.DB _, T.Var _) ->
                  flex_rigid ~depth ~subst:merged ~fresh_var_ ~scope ~ban_id body_t' body_s' rest
              | T.Const f , T.Const g when ID.equal f g ->
                  assert(List.length args_s = List.length args_t);
                  unify ~depth ~subst:merged ~fresh_var_ ~scope 
                    (build_constraints ~ban_id args_s args_t rest)
              | T.DB i, T.DB j when i = j ->
                  assert (List.length args_s = List.length args_t);
                  unify ~depth ~subst:merged ~fresh_var_ ~scope 
                    (build_constraints ~ban_id args_s args_t rest)
              | _ -> OSeq.empty
            )
          )
          | None -> OSeq.empty))

and identify ~depth ~subst ~fresh_var_ ~scope s t rest =
  (* Format.printf "Getting identification subst for %a and %a!\n" T.pp s T.pp t; *)
  let id_subs = OSeq.nth 0 (JP_unif.identify ~scope ~fresh_var_ s t []) in
  (* Format.printf "Merging id \n"; *)
  let subs_res = compose_sub ~scope subst id_subs in
  unify ~depth:(depth+1) ~scope ~fresh_var_ ~subst:subs_res 
    ((s, t, true)::rest)

and flex_rigid ~depth ~subst ~fresh_var_ ~scope ~ban_id s t rest =
  assert (T.is_var @@ T.head_term s);
  assert (not @@ T.is_var @@ T.head_term t);
  let bindings = proj_imit_bindings ~depth ~scope ~fresh_var_ s t in
  let substs = List.map (compose_sub ~scope subst) bindings in
  OSeq.of_list substs
  |> OSeq.flat_map (fun subst -> unify ~depth:(depth+1) ~scope  ~fresh_var_ ~subst 
                                  ((s, t,ban_id) :: rest))

and flex_same ~depth ~subst ~fresh_var_ ~scope hd_s args_s args_t rest all =
  assert(T.is_var hd_s);
  assert(List.length args_s = List.length args_t);
  assert(List.length args_s <= List.length @@ fst @@ Type.open_fun (T.ty hd_s));
  if List.length args_s > 0 then (
    let new_cstrs = build_constraints ~ban_id:true args_s args_t rest in
    let all_vars = CCList.range 0 ((List.length args_s) -1 ) in
    (* Format.printf "all vars %a\n" (CCList.pp CCInt.pp) all_vars; *)
    let all_args_unif = unify ~depth ~subst ~fresh_var_ ~scope new_cstrs in
    let first_unif = OSeq.take 1 all_args_unif |> OSeq.to_list in
    if !_conservative_elim && CCList.exists CCOpt.is_some first_unif  then (
        OSeq.of_list first_unif
    ) 
    else 
      OSeq.append
        all_args_unif
        (OSeq.of_list all_vars |>
        OSeq.filter_map (fun idx -> 
          assert(idx >= 0);
          if not @@ T.equal (List.nth args_s idx) (List.nth args_t idx) then
            Some (eliminate_at_idx ~scope ~fresh_var_ (T.as_var_exn hd_s) idx)
          else None) 
        |>  
          (OSeq.flat_map (fun subst' -> 
          let new_subst = compose_sub ~scope  subst subst' in
            unify ~depth:(depth+1) ~scope  ~fresh_var_ ~subst:new_subst all))))
    else 
      unify ~depth ~subst ~fresh_var_ ~scope rest

and flex_proj_imit  ~depth ~subst ~fresh_var_ ~scope s t rest = 
  let bindings = proj_imit_bindings ~depth  ~scope ~fresh_var_ s t in
  let bindings = proj_imit_bindings ~depth ~scope ~fresh_var_ t s @ bindings in
  let substs = List.map (compose_sub ~scope subst) bindings in
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
    let subst = T.Seq.vars t0 |> Sequence.fold (add_renaming scope0) subst in
    let subst = T.Seq.vars t1 |> Sequence.fold (add_renaming scope1) subst in
    let t0', t1' = S.apply subst (t0, scope0), S.apply subst (t1, scope1) in
    unify ~depth:0 ~scope:unifscope ~fresh_var_ ~subst [t0', t1', false]
    |> OSeq.map (CCOpt.map (fun sub -> 
      let l = Lambda.eta_expand @@ Lambda.snf @@ S.apply sub (t0, scope0) in 
      let r = Lambda.eta_expand @@ Lambda.snf @@ S.apply sub (t1, scope1) in
      if not (T.equal l r) then (
        Format.printf "For problem: %a =?= %a\n" T.pp t0' T.pp t1';
        Format.printf "Subst: @[%a@]\n" S.pp sub;
        Format.printf "%a <> %a\n" T.pp l T.pp r;
        assert(false);
      );
      if not (T.Seq.subterms l |> Sequence.append (T.Seq.subterms r) |> 
         Sequence.for_all (fun st -> List.for_all T.DB.is_closed @@ T.get_mand_args st)) then ( 
         Util.debugf 1 "Unequal subst: %a =?= %a, res %a.\n" (fun k-> k T.pp t0 T.pp t1 T.pp l); 
         assert(false); 
      );
      assert (T.equal l r);
      sub))

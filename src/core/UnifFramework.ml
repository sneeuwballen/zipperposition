module S = Subst
module LL = OSeq
module T = Term
module U = Unif

module type PARAMETERS = sig
  exception NotInFragment
  exception NotUnifiable
  val frag_algs : (T.t Scoped.t -> T.t Scoped.t -> S.t -> S.t) list
  val pb_oracle : (T.t Scoped.t -> T.t Scoped.t -> 'a -> (S.t * 'a) LL.t  )
end

module Make (P : PARAMETERS) = struct 
  (* apply a substitution and reduce to whnf *)
  let nfapply s u = Lambda.beta_red_head (S.FO.apply S.Renaming.none s u)


  let rec nfapply_mono subst (t,sc) =
    let pref, tt = T.open_fun t in
    let t' =  
      begin match T.view tt with
        | T.Var _ ->
          let u, _ = S.FO.deref subst (tt,sc) in
          if T.equal tt u then u
          else nfapply_mono subst (u,sc)
        | T.App (f0, l) ->
          let f = nfapply_mono subst (f0, sc) in
          let t =
            if T.equal f0 f then tt else T.app f l in
          let u = Lambda.whnf t in
          if T.equal t u
          then t
          else nfapply_mono subst (u,sc)
        | _ -> tt
      end in
    if T.equal tt t' then t
    else T.fun_l pref t'

  let normalize ~mono = if mono then nfapply_mono else nfapply

  let eta_expand_otf ~subst ~scope pref1 pref2 t1 t2 =
    let do_exp_otf n types t = 
      let remaining = CCList.drop n types in
      assert(List.length remaining != 0);
      let num_vars = List.length remaining in
      let vars = List.mapi (fun i ty -> 
        let ty = S.Ty.apply S.Renaming.none subst (ty,scope) in
        T.bvar ~ty (num_vars-1-i)) remaining in
      let shifted = T.DB.shift num_vars t in
      (* T.app_w_ty shifted vars ~ty:(S.apply_ty subst (T.ty shifted, scope)) in  *)
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


  let rec do_unif problem subst mono unifscope =
    let decompose args_l args_r rest flag =
      let flagged = List.map (fun (l,r) -> (l,r,flag)) @@ List.combine args_l args_r in
      let rigid, non_rigid = List.partition (fun (s,t,_) ->
        T.is_const (T.head_term s) && T.is_const (T.head_term t)) flagged in
      let pure_var, app_var = List.partition (fun (s,t,_) ->
        T.is_var s && T.is_var t) non_rigid in
    rigid @ pure_var @ rest @ app_var in

    let decompose_and_continue args_l args_r rest flag =
      let new_prob = decompose args_l args_r rest flag in
      do_unif new_prob subst mono unifscope in
    
    match problem with 
    | [] -> OSeq.return subst
    | (lhs, rhs, flag) :: rest ->
    
      let lhs = normalize ~mono subst (lhs, unifscope) 
        and rhs = normalize ~mono subst (rhs, unifscope) in
      let (pref_lhs, body_lhs) = T.open_fun lhs
        and (pref_rhs, body_rhs) = T.open_fun rhs in 
      let body_lhs, body_rhs, _ = 
        eta_expand_otf ~subst ~scope:unifscope pref_lhs pref_rhs body_lhs body_rhs in

      let (hd_lhs, args_lhs), (hd_rhs, args_rhs) = T.as_app body_lhs, T.as_app body_rhs in
      match T.view hd_lhs, T.view hd_rhs with
      | T.DB i, T.DB j ->
        if i = j then decompose_and_continue args_lhs args_rhs rest flag
        else raise Unif.Fail
      | T.Const f, T.Const g ->
        if ID.equal f g then decompose_and_continue args_lhs args_rhs rest flag
        else raise Unif.Fail
      | _ -> 
        let args_unif = 
          if T.is_var hd_lhs && T.is_var hd_rhs && T.equal hd_lhs hd_rhs then
            decompose_and_continue args_lhs args_rhs rest flag
          else LL.empty in
        let mgu = CCList.find_map (fun alg ->  
          try
            Some (alg (body_lhs, unifscope) (body_rhs, unifscope) subst)
          with 
            | P.NotInFragment -> None
            | P.NotUnifiable -> raise Unif.Fail 
        ) P.frag_algs in 
        match mgu with 
        | Some subst ->
           (* We assume that the substitution was augmented so that it is mgu for
              lhs and rhs *)
           do_unif rest subst mono unifscope
        | None ->
          let flagged_pb = P.pb_oracle (body_lhs, unifscope) (body_rhs, unifscope) flag in
          LL.fold_left (fun res (pb, flag') -> 
            let subst' = Subst.merge subst pb in
            let unif_for_pb = do_unif ((lhs,rhs,flag')::rest) subst' mono unifscope in
            LL.interleave res unif_for_pb
          ) args_unif flagged_pb
        
        

end
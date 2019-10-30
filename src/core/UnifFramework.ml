module S = Subst
module LL = OSeq
module T = Term
module U = Unif
module Q = CCDeque

module type PARAMETERS = sig
  exception NotInFragment
  exception NotUnifiable
  type flag_type
  val init_flag : flag_type
  val identify_scope : T.t Scoped.t -> T.t Scoped.t -> T.t * T.t * Scoped.scope * S.t
  val frag_algs : unit -> (T.t Scoped.t -> T.t Scoped.t -> S.t -> S.t list) list
  val pb_oracle : (T.t Scoped.t -> T.t Scoped.t -> flag_type -> S.t -> Scoped.scope -> (S.t * flag_type) list * (S.t * flag_type) option LL.t)
  val oracle_composer : 'a OSeq.t -> 'a OSeq.t -> 'a OSeq.t
end

module Make (P : PARAMETERS) = struct 
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
  
  (* apply a substitution, possibly eta-expand because
     a type substitution might introduce a need for expansion and reduce to whnf *)
  let nfapply s u = Lambda.whnf @@ Lambda.eta_expand @@ S.FO.apply S.Renaming.none s u

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
      T.app shifted vars in

    if List.length pref1 = List.length pref2 then (t1, t2, pref1)
    else (
      let n1, n2 = List.length pref1, List.length pref2 in 
      if n1 < n2 then (do_exp_otf n1 pref2 t1,t2,pref2)
      else (t1,do_exp_otf n2 pref1 t2,pref1))

  let different_rigid_heads s t =
    not @@ T.is_var s && not @@ T.is_var t &&
      match T.view s with
      | T.DB _ -> not @@ T.is_bvar t
      | T.Const _  -> not @@ T.is_const t
      | T.AppBuiltin _ ->  not @@ T.is_appbuiltin t
      | _ -> false

  let tasks_taken = 100

  let do_unif problem subst mono unifscope =   
    let rec aux ~steps subst problem =
      let decompose args_l args_r rest flag =
        let flagged = List.map (fun (l,r) -> (l,r,flag)) @@ List.combine args_l args_r in
        let rigid_rigid, non_rigid = List.partition (fun (s,t,_) ->
          T.is_const (T.head_term s) && T.is_const (T.head_term t)) flagged in
      rigid_rigid @ rest  @ non_rigid in

      let decompose_and_cont args_l args_r rest flag subst =
        let new_prob = decompose args_l args_r rest flag in
        aux ~steps subst new_prob in
      
      match problem with 
      | [] -> OSeq.return (Some subst)
      | (lhs, rhs, flag) :: rest ->
        match PatternUnif.unif_simple ~subst ~scope:unifscope 
                  (T.of_ty (T.ty lhs)) (T.of_ty (T.ty rhs)) with 
        | None -> OSeq.empty
        | Some subst ->
          let subst = Unif_subst.subst subst in
          let lhs = normalize ~mono subst (lhs, unifscope) 
            and rhs = normalize ~mono subst (rhs, unifscope) in
          let (pref_lhs, body_lhs) = T.open_fun lhs
            and (pref_rhs, body_rhs) = T.open_fun rhs in 
          let body_lhs, body_rhs, _ = 
            eta_expand_otf ~subst ~scope:unifscope pref_lhs pref_rhs body_lhs body_rhs in
          let (hd_lhs, args_lhs), (hd_rhs, args_rhs) = T.as_app body_lhs, T.as_app body_rhs in
        
          if T.equal body_lhs body_rhs then (
            aux ~steps subst rest
          ) else (
            match T.view hd_lhs, T.view hd_rhs with
            | T.DB i, T.DB j ->
              if i = j then decompose_and_cont args_lhs args_rhs rest flag subst
              else OSeq.empty
            | T.Const f, T.Const g ->
              if ID.equal f g && List.length args_lhs = List.length args_rhs 
              then decompose_and_cont args_lhs args_rhs rest flag subst
              else OSeq.empty
            | T.AppBuiltin(b1, args1), T.AppBuiltin(b2, args2) ->
              let args_lhs = args_lhs @ args1 and args_rhs = args_rhs @ args2 in
              if Builtin.equal b1 b2 && List.length args_lhs = List.length args_rhs then (
                decompose_and_cont (args_lhs@args1) (args_rhs@args2) rest flag subst
              ) else OSeq.empty
            | _ when different_rigid_heads hd_lhs hd_rhs -> OSeq.empty
            | _ -> 
              try 
                let mgu = CCList.find_map (fun alg ->  
                  try
                    Some (alg (lhs, unifscope) (rhs, unifscope) subst)
                  with 
                    | P.NotInFragment -> None
                    | P.NotUnifiable -> 
                    (* CCFormat.printf "f:@[%a@]=?=@[%a@]@." T.pp (nfapply subst (lhs,unifscope)) T.pp (nfapply subst (rhs,unifscope)); *)
                    raise Unif.Fail
                ) (P.frag_algs ()) in 
                match mgu with 
                | Some substs ->
                  (* We assume that the substitution was augmented so that it is mgu for
                      lhs and rhs *)
                  OSeq.map (fun sub ->
                    aux ~steps:(steps+1) sub rest
                  ) (OSeq.of_list substs)
                  |> OSeq.merge
                | None ->
                  let args_unif =
                    if T.is_var hd_lhs && T.is_var hd_rhs && T.equal hd_lhs hd_rhs then
                      decompose_and_cont args_lhs args_rhs rest flag subst
                    else OSeq.empty in
                  
                  let finite_branch_oracle, infinite_branch_oracle = 
                    P.pb_oracle (body_lhs, unifscope) (body_rhs, unifscope) flag subst unifscope in
                  
                  let finite_branch_w_none = 
                    (* delaying this unification steps every once in a whilemake *)
                    let delay = if steps > 3 then 5*steps else steps in 
                    OSeq.append
                      (OSeq.take (delay) (OSeq.repeat None))
                      (OSeq.of_list @@ List.map (fun x -> Some x) finite_branch_oracle)
                    in
                  let all_oracles =
                    OSeq.append finite_branch_w_none infinite_branch_oracle in
                  
                  let oracle_unifs = OSeq.map (fun sub_flag_opt -> 
                    match sub_flag_opt with 
                    | None -> OSeq.return None
                    | Some (sub', flag') ->
                      try 
                        let subst' = Subst.merge subst sub' in
                        aux ~steps:(steps+1) subst' ((lhs,rhs,flag')::rest)
                      with Subst.InconsistentBinding _ ->
                       OSeq.return None) all_oracles
                  |> OSeq.merge in

                  OSeq.interleave args_unif oracle_unifs
              with Unif.Fail -> OSeq.empty) in
    aux ~steps:0 subst problem
  
  let unify_scoped t0s t1s =
    let lhs,rhs,unifscope,subst = P.identify_scope t0s t1s in
    let mono = 
      Iter.is_empty @@ Iter.append (Term.Seq.ty_vars lhs) (Term.Seq.ty_vars rhs) in
    try
      do_unif [(lhs,rhs,P.init_flag)] subst mono unifscope
      (* |> OSeq.map (fun opt -> CCOpt.map (fun subst -> 
        let l = Lambda.eta_reduce @@ Lambda.snf @@ S.FO.apply Subst.Renaming.none subst t0s in 
        let r = Lambda.eta_reduce @@ Lambda.snf @@ S.FO.apply Subst.Renaming.none subst t1s in
        if not ((T.equal l r) && (Type.equal (Term.ty l) (Term.ty r))) then (
          CCFormat.printf "orig:@[%a@]=?=@[%a@]@." (Scoped.pp T.pp) t0s (Scoped.pp T.pp) t1s;
          CCFormat.printf "subst:@[%a@]@." Subst.pp subst;
          CCFormat.printf "new:@[%a@]=?=@[%a@]@." T.pp l T.pp r;
          assert(false)
        ); subst) opt)
      res *)
    with Unif.Fail -> OSeq.empty
end
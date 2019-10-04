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
  
  (* apply a substitution and reduce to whnf *)
  let nfapply s u = Lambda.whnf (S.FO.apply S.Renaming.none s u)

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

  (* rigid path check *)
  let rp_check s t =
    let rec rp_check_aux var rigid =
      match T.view rigid with 
      | T.AppBuiltin(hd, args) ->
        let args_hds = List.map T.head_term args in
        if List.exists (T.equal var) args_hds then false
        else List.for_all (rp_check_aux var) args
      | T.App(hd, args) when not (T.is_var hd) ->
        let args_hds = List.map T.head_term args in
        if List.exists (T.equal var) args_hds then false
        else List.for_all (rp_check_aux var) args
      | T.Fun(_, body) -> rp_check_aux var body
      | _ -> true in

    let hd_s = T.head_term s in
    let hd_t = T.head_term t in
    if Term.is_var hd_s && not (T.is_var hd_t) then (
      rp_check_aux hd_s t
    ) else if Term.is_var hd_t && not (T.is_var hd_s) then (
      rp_check_aux hd_t s
    ) else true

  let do_unif problem_subst mono unifscope =   
    let queue = Q.create () in
    Q.push_back queue problem_subst;

    let rec aux ~depth oracles =
      let decompose args_l args_r rest flag =
        let flagged = List.map (fun (l,r) -> (l,r,flag)) @@ List.combine args_l args_r in
        let rigid, non_rigid = List.partition (fun (s,t,_) ->
          T.is_const (T.head_term s) && T.is_const (T.head_term t)) flagged in
        let pure_var, app_var = List.partition (fun (s,t,_) ->
          T.is_var s && T.is_var t) non_rigid in
      rigid @ pure_var @ rest @ app_var in

      let decompose_and_add args_l args_r rest flag subst =
        let new_prob = decompose args_l args_r rest flag in
        Q.push_front queue (new_prob,subst); in

          
      if Q.is_empty queue then (
        if OSeq.is_empty oracles then OSeq.empty
        else (
          let front, back = OSeq.take tasks_taken oracles, OSeq.drop tasks_taken oracles in
          let none_cnt = ref 0 in
          OSeq.iter (fun prob_subs_opt -> 
            match prob_subs_opt with 
            | Some prob_subs -> 
              Q.push_front queue prob_subs;
            | None ->
              incr none_cnt)
          front;
          OSeq.append (OSeq.take (50* !none_cnt) (OSeq.repeat None)) (aux ~depth:(depth+1) back))
      )
      else (
        let problem,subst = Q.take_front queue in
        match problem with 
        | [] -> OSeq.cons (Some subst) (aux ~depth oracles)
        | (lhs, rhs, flag) :: rest ->
          match PatternUnif.unif_simple ~subst ~scope:unifscope 
                    (T.of_ty (T.ty lhs)) (T.of_ty (T.ty rhs)) with 
          | None -> aux ~depth oracles
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
              Q.push_front queue (rest,subst);
              aux ~depth oracles
            ) else if not (rp_check body_lhs body_rhs) then (
              aux ~depth oracles
            )
            else (
              match T.view hd_lhs, T.view hd_rhs with
              | T.DB i, T.DB j ->
                if i = j then decompose_and_add args_lhs args_rhs rest flag subst;
                aux ~depth oracles
              | T.Const f, T.Const g ->
                if ID.equal f g then decompose_and_add args_lhs args_rhs rest flag subst;
                aux ~depth oracles
              | T.AppBuiltin(b1, args1), T.AppBuiltin(b2, args2) ->
                let args_lhs = args_lhs @ args1 and args_rhs = args_rhs @ args2 in
                if Builtin.equal b1 b2 && List.length args_lhs = List.length args_rhs then (
                  decompose_and_add (args_lhs@args1) (args_rhs@args2) rest flag subst
                );
                aux ~depth oracles
              | _ when different_rigid_heads hd_lhs hd_rhs -> aux ~depth oracles
              | _ -> 
                try 
                  let mgu = CCList.find_map (fun alg ->  
                    try
                      Some (alg (body_lhs, unifscope) (body_rhs, unifscope) subst)
                    with 
                      | P.NotInFragment -> None
                      | P.NotUnifiable -> raise Unif.Fail
                  ) (P.frag_algs ()) in 
                  match mgu with 
                  | Some substs ->
                    (* We assume that the substitution was augmented so that it is mgu for
                        lhs and rhs *)
                    CCList.iter (fun subst -> Q.push_front queue (rest,subst)) substs;
                    aux ~depth oracles
                  | None ->
                    if T.is_var hd_lhs && T.is_var hd_rhs && T.equal hd_lhs hd_rhs then
                      decompose_and_add args_lhs args_rhs rest flag subst;
                    
                    let finite_branch_oracle, infinite_branch_oracle = 
                      P.pb_oracle (body_lhs, unifscope) (body_rhs, unifscope) flag subst unifscope in
                    let new_oracle = OSeq.map (fun pb_flag_opt ->
                      match pb_flag_opt with
                      | Some (pb, flag') ->
                        let subst' = Subst.merge subst pb in
                        Some ((lhs,rhs,flag')::rest, subst')
                      | None -> None) infinite_branch_oracle in
                    
                    List.iter (fun (pb, flag') -> 
                      Q.push_back queue ((lhs,rhs,flag')::rest,Subst.merge subst pb);
                    ) finite_branch_oracle;

                    aux ~depth:(depth+1) (OSeq.interleave oracles new_oracle)                    
                with Unif.Fail -> aux ~depth oracles)) in
    aux ~depth:0 OSeq.empty
  
  let unify_scoped t0s t1s =
    let lhs,rhs,unifscope,subst = P.identify_scope t0s t1s in
    let mono = 
      Iter.is_empty @@ Iter.append (Term.Seq.ty_vars lhs) (Term.Seq.ty_vars rhs) in
    try
      do_unif ([(lhs,rhs,P.init_flag)],subst) mono unifscope
    with Unif.Fail -> OSeq.empty
end
module S = Subst
module LL = OSeq
module T = Term
module U = Unif
module US = Unif_subst
module PUP = PragUnifParams

module type PARAMETERS = sig
  exception NotInFragment
  exception NotUnifiable
  type flag_type
  val init_flag : flag_type
  val flex_state : Flex_state.t
  val identify_scope : T.t Scoped.t -> T.t Scoped.t -> T.t * T.t * Scoped.scope * S.t
  val frag_algs : unit -> (T.t Scoped.t -> T.t Scoped.t -> US.t -> US.t list) list
  val pb_oracle : (T.t Scoped.t -> T.t Scoped.t -> flag_type -> S.t -> Scoped.scope -> (S.t * flag_type) option LL.t)
end

(* Given a sequence of sequences (i.e., a generator) A
       take one element from the A[0],
       then one element from A[1] and A[0]
       then one element from A[2],A[1] and A[0], etc.. *)
let take_fair gens =
  (* Take one element from A[0],A[1],...,A[k-1] *)
  let rec take_first_k k acc gens =
    if k = 0 then (acc, gens)
    else (match gens () with 
        | OSeq.Nil -> (acc, OSeq.empty)
        | OSeq.Cons(x,xs) ->
          begin match x () with 
            | OSeq.Nil ->
              take_first_k (k-1) acc xs
            | OSeq.Cons(y, ys) ->
              let taken, new_gens = take_first_k (k-1) (OSeq.cons y acc) xs in
              (taken, OSeq.cons ys new_gens) end) in

  (* Take one element from A[0],A[1],...A[i-1]
      and then take one element from A[0],A[1],...,A[i]   *)
  let rec aux i gens =
    let taken, new_gens = take_first_k i OSeq.empty gens in
    if OSeq.is_empty new_gens then taken
    else (OSeq.append taken (function () -> aux (i+1) new_gens ())) 
  in
  aux 1 gens

module Make (P : PARAMETERS) = struct 
  exception PolymorphismDetected

  let rec nfapply_mono subst (t,sc) =
    let pref, tt = T.open_fun t in
    let t' =  
      begin match T.view tt with
        | T.Var _ ->
          if not (Type.is_ground (T.ty tt)) then (
            raise PolymorphismDetected
          );

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
  let nfapply s u = Lambda.whnf @@ S.FO.apply S.Renaming.none s u

  let normalize s u =
    try
      if not (Type.is_ground (T.ty (fst u))) then
        raise PolymorphismDetected;

      nfapply_mono s u 
    with PolymorphismDetected -> 
      nfapply s u 

  let eta_expand_otf ~subst ~scope pref1 pref2 t1 t2 =
    let do_exp_otf n types t = 
      let remaining = CCList.drop n types in
      assert(List.length remaining != 0);
      let num_vars = List.length remaining in
      let vars = List.mapi (fun i ty -> 
          (* let ty = S.Ty.apply S.Renaming.none subst (ty,scope) in *)
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

  let do_unif ~bind_cnt problem subst unifscope =
    let delay_pair ~delayed lhs rhs flag = (lhs,rhs,flag) :: delayed in
    let delay_enabled = 
      Flex_state.get_exn PUP.k_delay_flex_flex P.flex_state in

    let slow_down res =
      let skipper = 
        int_of_float (Flex_state.get_exn PUP.k_skip_multiplier P.flex_state) in
      if !bind_cnt mod skipper == 0 then (
        let none_count = !bind_cnt / skipper in
        OSeq.append (OSeq.take none_count (OSeq.repeat None)) res
      ) else res 
    in

    let rec aux ~root ~delayed ~lambda_pref subst problem =
      let delay_ff ~lambda_pref ~subst ~rest lhs rhs flag =
        assert(T.is_var (T.head_term lhs));
        assert(T.is_var (T.head_term rhs));

        let delayed = delay_pair ~delayed lhs rhs flag in
        aux ~lambda_pref ~root:false ~delayed subst rest in
      
      let decompose args_l args_r rest flag =
        let rec zipped_with_flag = function 
          | [], [] -> []
          | x::xs, y::ys -> (x,y,flag) :: (zipped_with_flag (xs,ys))
          | _, _ -> invalid_arg "lists must be of the same size." in

        let new_args = zipped_with_flag (args_l,args_r) in

        let to_classify, rest = 
          if List.length rest <= 15
          then new_args@rest, []
          else new_args, rest in

        let sort_class =
          List.sort (fun (l,r,_) (l', r',_) ->
              let l,l' = CCPair.map_same (fun t -> T.head_term @@ snd @@ T.open_fun t) (l,l') in
              let r,r' = CCPair.map_same (fun t -> T.head_term @@ snd @@ T.open_fun t) (r,r') in
              if (not (Term.is_app l) || not (Term.is_app r)) &&
                 (not (Term.is_app l') || not (Term.is_app r')) then 0
              else if not (Term.is_app l) || not (Term.is_app r) then -1
              else if not (Term.is_app l') || not (Term.is_app r') then 1
              else Term.ho_weight l + Term.ho_weight r - 
                   Term.ho_weight l' - Term.ho_weight r'
            ) in

        let classify_one s =
          let rec follow_bindings t =
            let hd = T.head_term @@ snd @@ (T.open_fun t) in
            let derefed,_ = Subst.FO.deref (US.subst subst) (hd, unifscope) in
            if T.equal hd derefed then hd
            else follow_bindings derefed in

          let hd = follow_bindings s in

          if T.is_const hd then `Const
          else if T.is_var hd then `Var
          (* when it is bound variable, we do not know what will 
             happen when it is reduced *)
          else `Unknown in


        (* classifies the pairs as (rigid-rigid, flex-rigid, and flex-flex *)
        let rec classify = function 
          | ((lhs,rhs,flag) as cstr) :: xs ->
            let rr,fr,unsure,ff = classify xs in
            begin match classify_one lhs, classify_one rhs with 
              | `Const, `Const -> cstr::rr,fr,unsure,ff
              | _, `Const  | `Const, _ -> rr, cstr::fr, unsure, ff
              | _, `Unknown | `Unknown, _ -> rr, fr, cstr::unsure, ff
              | `Var, `Var -> rr,fr,unsure, cstr::ff end
          | [] -> ([],[],[],[]) in

        let rr,fr,unsure,ff = classify to_classify in
        if Flex_state.get_exn PragUnifParams.k_sort_constraints P.flex_state then
          sort_class rr @ sort_class fr @ sort_class unsure @ rest @ sort_class ff
        else rr @ fr @ unsure @ rest @ ff in

      let decompose_and_cont ?(inc_step=0) ~lambda_pref args_l args_r rest flag subst =
        let new_prob = decompose args_l args_r rest flag in
        aux ~root:false ~lambda_pref ~delayed subst new_prob in

      match problem with 
      | [] -> 
        if CCList.is_empty delayed then OSeq.return (Some subst)
        else (
          let get_head t = T.head_term (snd (T.open_fun t)) in
          let is_mapped (lhs,rhs,flag) =
            assert (T.is_var (get_head lhs) && T.is_var (get_head rhs));
            US.mem subst ((T.as_var_exn (get_head lhs), unifscope) :> InnerTerm.t HVar.t Scoped.t) ||
            US.mem subst ((T.as_var_exn (get_head rhs), unifscope) :> InnerTerm.t HVar.t Scoped.t)
          in
          let mapped, unmapped = List.partition is_mapped delayed in
          if CCList.is_empty mapped then (
            let subst = List.fold_left (fun subst (l,r,_) ->
              let l,r = CCPair.map_same (T.fun_l (List.rev lambda_pref)) (l,r) in
              assert(T.DB.is_closed l && T.DB.is_closed r);
              US.add_constr (Unif_constr.make_fo ~tags:[] (l,unifscope) (r,unifscope)) subst
            ) (US.of_subst (US.subst subst)) delayed 
            in
            OSeq.return (Some subst)
          )
          else (
            aux ~root:false ~delayed:unmapped ~lambda_pref subst mapped
           )
        )
      | (lhs, rhs, flag) :: rest ->
        match PatternUnif.unif_simple ~subst ~scope:unifscope 
                (T.of_ty (T.ty lhs)) (T.of_ty (T.ty rhs)) with 
        | None -> OSeq.empty
        | Some subst ->
          let lhs = normalize (US.subst subst) (lhs, unifscope) 
          and rhs = normalize (US.subst subst) (rhs, unifscope) in
          let (pref_lhs, body_lhs) = T.open_fun lhs
          and (pref_rhs, body_rhs) = T.open_fun rhs in 
          let body_lhs, body_rhs, new_pref = 
            eta_expand_otf ~subst ~scope:unifscope pref_lhs pref_rhs body_lhs body_rhs in
          let (hd_lhs, args_lhs), (hd_rhs, args_rhs) = T.as_app body_lhs, T.as_app body_rhs in
          let lambda_pref = (List.rev new_pref) @ lambda_pref in

          if T.equal body_lhs body_rhs then (
            aux ~root:false ~delayed ~lambda_pref subst rest
          ) else (
            match T.view hd_lhs, T.view hd_rhs with
            | T.DB i, T.DB j ->
              if i = j then decompose_and_cont ~lambda_pref args_lhs args_rhs rest flag subst
              else OSeq.empty
            | T.Const f, T.Const g ->
              if ID.equal f g && List.length args_lhs = List.length args_rhs 
              then decompose_and_cont ~lambda_pref args_lhs args_rhs rest flag subst
              else OSeq.empty
            | T.AppBuiltin(b1, args1), T.AppBuiltin(b2, args2) ->
              let args_lhs = args_lhs @ args1 and args_rhs = args_rhs @ args2 in
              if Builtin.equal b1 b2 && List.length args_lhs = List.length args_rhs then (
                let args_lhs, args_rhs = 
                  Unif.norm_logical_disagreements b1 args_lhs args_rhs in
                decompose_and_cont ~lambda_pref (args_lhs) (args_rhs) rest flag subst
              ) else OSeq.empty
            | _ when different_rigid_heads hd_lhs hd_rhs -> OSeq.empty
            | _ -> 
              try
                let mgu =
                  (* if steps > 3 then None else *)
                  CCList.find_map (fun alg ->  
                      try
                        Some (alg (lhs, unifscope) (rhs, unifscope) subst)
                      with 
                      | P.NotInFragment -> None
                      | P.NotUnifiable -> 
                      (* CCFormat.printf "@[%a@]@ =@ @[%a@] not unif@."  T.pp lhs T.pp rhs; *)
                      raise Unif.Fail
                    ) (P.frag_algs ()) in 
                match mgu with 
                | Some substs ->
                  (* We assume that the substitution was augmented so that it is mgu for
                      lhs and rhs *)
                  CCList.map (fun sub () -> 
                    let cstr = US.constr_l sub in
                    let new_delayed = 
                      List.map 
                        (fun (l,r) -> (T.of_term_unsafe l, T.of_term_unsafe r,flag)) 
                        (Unif_constr.apply_subst_l S.Renaming.none S.empty cstr) in
                    let delayed = new_delayed @ delayed in
                    aux ~lambda_pref ~root:false ~delayed sub rest ()) substs
                  |> OSeq.of_list
                  |> OSeq.merge
                | None ->
                  if not root && delay_enabled &&
                     T.is_var hd_lhs && T.is_var rhs then (
                    delay_ff ~lambda_pref ~subst ~rest body_lhs body_rhs flag
                  ) else (
                    let args_unif =
                      if T.is_var hd_lhs && T.is_var hd_rhs && T.equal hd_lhs hd_rhs then
                        decompose_and_cont ~lambda_pref args_lhs args_rhs rest flag subst
                      else OSeq.empty in

                    let all_oracles = 
                      P.pb_oracle (body_lhs, unifscope) (body_rhs, unifscope) flag (US.subst subst) unifscope in

                    let oracle_unifs = 
                      OSeq.map (fun sub_flag_opt ->
                          match sub_flag_opt with 
                          | None -> OSeq.return None
                          | Some (sub', flag') ->
                            try
                              let subst' = US.merge subst (US.of_subst sub') in
                              incr bind_cnt;
                              slow_down (fun () -> aux ~delayed ~lambda_pref ~root:false subst' ((lhs,rhs,flag') :: rest) ())
                            with Subst.InconsistentBinding _ ->
                              OSeq.empty) all_oracles
                      |> OSeq.merge in
                    OSeq.interleave oracle_unifs args_unif)
              with Unif.Fail -> OSeq.empty) in
    aux ~root:true ~delayed:[] ~lambda_pref:[] subst problem

  let try_lfho_unif ((s,_) as t0) ((t,_) as t1) =
    
    (* term is eligible for LFHO unif if it has applied variables
       but has no lambdas -- we want to avoid FO terms to avoid
       returning same unifier twice (since our unif algo will also
       compute FO unifier)  *)
    let eligible_for_lfho t =
      let exception LambdaFound in
      let no_lams t =
        if Iter.exists T.is_fun (T.Seq.subterms ~include_builtin:true t) 
        then raise LambdaFound
        else true in

      let rec aux t = 
        match T.view t with
        | T.App(hd, args) when T.is_const hd -> 
          List.for_all no_lams args
        | T.App(hd, args) -> List.exists aux (hd::args)
        | T.AppBuiltin(_, args)  -> List.exists aux args
        | T.Fun _ -> raise LambdaFound
        (* unif algo can easily take care of naked vars and consts *)
        | _ -> false in
      
      try
        aux t
      with LambdaFound -> false in
    
    if Flex_state.get_exn PUP.k_try_lfho P.flex_state &&
       eligible_for_lfho s && eligible_for_lfho t then (
        try
          OSeq.return (Some (US.of_subst @@ Unif.FO.unify_syn t0 t1))
        with Unif.Fail -> OSeq.empty
    ) else OSeq.empty



  let unify_scoped t0s t1s =
    let lhs,rhs,unifscope,subst = P.identify_scope t0s t1s in
    let subst = US.of_subst subst in
    let bind_cnt = ref 0 in
    try
      OSeq.append 
        (try_lfho_unif t0s t1s)
        (do_unif ~bind_cnt [(lhs,rhs,P.init_flag)] subst unifscope)
      |> OSeq.map (fun opt -> CCOpt.map (fun subst ->
        if not @@ US.has_constr subst then (
          let s = US.subst subst in
          let norm t = T.normalize_bools @@ Lambda.eta_expand @@ Lambda.snf t in
          let l = norm @@ S.FO.apply Subst.Renaming.none s t0s in 
          let r = norm @@ S.FO.apply Subst.Renaming.none s t1s in
          if not ((T.equal l r) && (Type.equal (Term.ty l) (Term.ty r))) then (
            CCFormat.printf "subst:@[%a@]@." Subst.pp s;
            CCFormat.printf "orig:@[%a@]@.=?=@.@[%a@]@." (Scoped.pp T.pp) t0s (Scoped.pp T.pp) t1s;
            CCFormat.printf "new:@[%a:%a@]@.=?=@.@[%a:%a@]@." T.pp l Type.pp (T.ty l) T.pp r Type.pp (T.ty r);
            assert(false)
        )); subst) opt)
    with Unif.Fail -> OSeq.empty    
end
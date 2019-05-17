module T = Term
module US = Unif_subst
module P = PatternUnif
module H = HVar


type subst = US.t
type unif_state =
{
  num_identifications : int;
  num_var_imitations  : int;
  num_app_projections : int;
  num_elims           : int;
  depth               : int
}


module S = struct
  let apply s t = Subst.FO.apply Subst.Renaming.none (US.subst s) t
  let apply_ty s ty = Subst.Ty.apply Subst.Renaming.none (US.subst s) ty
  
  let pp = US.pp
end

let max_depth = ref 7
let max_app_projections = ref 3
let max_var_imitations = ref 1
let max_identifications = ref 0
let max_elims           = ref 2

let _cons_e = ref true
let _imit_first = ref false
let _solve_var = ref true

(* apply a substitution and reduce to whnf *)
let nfapply s u = Lambda.beta_red_head (S.apply s u)

let disable_conservative_elim () =
  _cons_e := false

let enable_imit_first () = 
  _imit_first := true

let enable_solve_var () = 
  _solve_var := true

let compose_sub s1 s2 =
  US.merge s1 s2

let continue_unification state =
  state.depth <= !max_depth

(* Make new list of constraints, prefering the rigid-rigid pairs *)
let build_constraints ~ban_id args1 args2 rest = 
  let zipped = List.map (fun (x,y) -> (x,y,ban_id)) (List.combine args1 args2) in
  let rigid, non_rigid = List.partition (fun (s,t,_) ->
    T.is_const (T.head_term s) && T.is_const (T.head_term t)) zipped in
  rigid @ rest @ non_rigid

(* Create substitution: v |-> λ u1 ... um. x u1 ... u{k-1} u{k+1} ... um *)
let eliminate_at_idx ~scope ~counter v k =  
  let prefix_types, return_type = Type.open_fun (HVar.ty v) in
  let m = List.length prefix_types in
  let bvars = List.mapi (fun i ty -> T.bvar ~ty (m-1-i)) prefix_types in
  let prefix_types' = CCList.remove_at_idx k prefix_types in
  let new_ty = Type.arrow prefix_types' return_type in
  let bvars' = CCList.remove_at_idx k bvars in
  let matrix_head = T.var (H.fresh_cnt ~counter ~ty:new_ty ()) in
  let matrix = T.app matrix_head bvars' in
  let subst_value = T.fun_l prefix_types matrix in
  let subst = US.FO.singleton (v, scope) (subst_value, scope) in
  subst

(* Create substitution: v |-> λ u1 ... um. u_i (H1 u1 ... um) ... (Hn u1 ... um)
   where type of u_i is τ1 -> ... τn -> τ where τ is atomic and H_i have correct
   type. This substitution is called a projection. *)
let project_hs_one ~counter pref_types i type_ui =
  let pref_types_ui, _ = Type.open_fun type_ui in
  let n_args_free = List.length pref_types in
  let pref_args = 
    List.mapi (fun i ty -> T.bvar ~ty (n_args_free-i-1)) pref_types in
  let new_vars = 
    List.map (fun ty ->
      let new_ty =  (Type.arrow pref_types ty) in
      T.var (H.fresh_cnt ~counter ~ty:new_ty ()))
    pref_types_ui  in
  let new_vars_applied = List.map (fun nv -> T.app nv pref_args) new_vars in
  let matrix_hd = T.bvar ~ty:type_ui (n_args_free-i-1) in
  let matrix = T.app matrix_hd new_vars_applied in
  T.fun_l pref_types matrix

(* Create substitution: v |-> λ u1 ... um. f (H1 u1 ... um) ... (Hn u1 ... um)
   where type of f is τ1 -> ... τn -> τ where τ is atomic, H_i have correct
   type and f is a constant. This substitution is called an imitation.*)
let imitate_one ~scope ~counter  s t =
  try
    OSeq.nth 0 (JP_unif.imitate_onesided ~scope ~counter s t)
  with Not_found ->  raise Not_found

(* Create all possible projection and imitation bindings. *)
let proj_imit_bindings ~state ~subst ~scope ~counter  s t =
  let hd_s, args_s = CCPair.map1 (fun x -> T.as_var_exn x) (T.as_app s) in
  let hd_t,_ = T.as_app (snd (T.open_fun t)) in
  let pref_tys, var_ret_ty = Type.open_fun (HVar.ty hd_s) in
  let proj_bindings = 
    pref_tys
    |> List.mapi (fun i ty -> i, ty)
    |> (fun l ->
        (* if we performed more than N projections that applied the
           bound variable we back off *)
        if state.num_app_projections <= !max_app_projections then l
        else
          List.filter (fun (_, ty) -> 
            List.length (Type.expected_args ty) = 0) l)
    (* If heads are different constants, do not project to those subterms *)
    |> CCList.filter_map (fun ((i, _) as p) -> 
        if i < List.length args_s then (
          let s_i = List.nth args_s i in
          let s_i = T.head_term (snd (T.open_fun s_i)) in
          if (T.is_const s_i && T.is_const hd_t && (not (T.equal s_i hd_t))) then None 
          else Some p
        ) else Some p
    )
    |> List.map (fun (i, ty) ->
        let _, arg_ret_ty = Type.open_fun ty in
        match P.unif_simple ~scope ~subst:(US.subst subst) 
                (T.of_ty arg_ret_ty) (T.of_ty var_ret_ty) with
        | Some ty_unif ->
          (* we project only to arguments whose type can be unified
             with head's return type *)
          let pr_bind =
            project_hs_one ~counter 
              (List.map (fun ty -> S.apply_ty ty_unif (ty, scope)) pref_tys) i
              (S.apply_ty ty_unif (ty, scope)) in
          let hd_s = HVar.cast hd_s ~ty:(S.apply_ty ty_unif (HVar.ty hd_s, scope)) in
            Some (US.FO.bind ty_unif (hd_s, scope) (pr_bind, scope),
                  List.length @@ Type.expected_args ty)
        | None -> None)
    |> CCList.filter_map (fun x -> x) in
       let imit_binding =
       let hd_s = T.head_term_mono s in 
       let hd_t = T.head_term_with_mandatory_args t in
       if (not @@ T.is_bvar @@ T.head_term t && 
           not (T.var_occurs ~var:(T.as_var_exn hd_s) hd_t)) then 
         [(imitate_one ~scope ~counter s t,-1)]
       else [] in
  let first, second = 
    if !_imit_first then imit_binding, proj_bindings
    else proj_bindings, imit_binding in 
  first @ second

let rec unify ~state ~scope ~counter ~subst = function
  | [] -> 
      (* all constraints solved for the initial problem *)
      OSeq.return (Some subst)
  | (s,t,ban_id) :: rest as l -> (
    if not @@ continue_unification state then OSeq.empty
    else (
      let s', t' = nfapply subst (s, scope), nfapply subst (t, scope) in
      if not (Term.equal s' t') then (
        match P.unif_simple ~subst:(US.subst subst) ~scope 
                (T.of_ty (T.ty s')) (T.of_ty (T.ty t')) with
        | Some ty_unif -> (
          let s', t' = nfapply ty_unif (s', scope), nfapply ty_unif (t', scope) in
          let subst = ty_unif in
          try
            if !_solve_var then (
              (* trying pattern unification *)
              let subst = P.unify_scoped ~counter ~subst (s',scope) (t',scope) in
              unify ~state ~scope ~counter ~subst rest 
            ) 
            else (
              if Term.is_fo_term s' && Term.is_fo_term t' then (
                let subst = P.unif_simple ~scope ~subst:(US.subst subst) s' t' in
                if CCOpt.is_some subst then (
                  let subst = CCOpt.get_exn subst in
                  unify ~state ~scope ~counter ~subst rest
                ) else raise P.NotUnifiable
              ) else raise P.NotInFragment
            )
          with
          |P.NotUnifiable -> 
            (* A weaker unification procedure determined the constraint is unsolvable *)
            OSeq.empty 
          |P.NotInFragment ->
            (* A weaker unification procedure gave up *)
            let (pref_s, body_s), (pref_t, body_t) = T.open_fun s', T.open_fun t' in
            let body_s', body_t', _ = P.eta_expand_otf ~subst ~scope pref_s pref_t body_s body_t in
            let (hd_s, args_s), (hd_t, args_t) = T.as_app body_s', T.as_app body_t' in
            match T.view hd_s, T.view hd_t with 
            | (T.Var _, T.Var _) -> 
              if T.equal hd_s hd_t then (
                flex_same ~state ~subst ~counter ~scope hd_s args_s args_t rest l
              )
              else (
                (* Flex-flex pairs with different heads are solved as follows: 
                    -The flex-flex pairs from the original problem with
                    different heads are solved using identification rule (and
                    possibly other rules if cons_ff is true)
                    -For flex-flex pairs that are created inside the algorithm
                    we disallow application of identification (since you can play this
                    game quite long) and in many practical cases you get the
                    MGU with just one level of identification  
                  
                  Constraints on which iteration rule has not been applied
                  have the ban_id value of false.                    
                  *)
                  if !_imit_first then (
                    OSeq.append
                      (flex_proj_imit  ~state ~subst ~counter ~scope body_s' body_t' rest)
                      (identify ~state ~subst ~counter ~scope body_s' body_t' rest))
                  else (
                    OSeq.append
                      (identify ~state ~subst ~counter ~scope body_s' body_t' rest)
                      (flex_proj_imit  ~state ~subst ~counter ~scope body_s' body_t' rest)
                  )
              )
            | (T.Var _, T.Const _) | (T.Var _, T.DB _) ->
                flex_rigid ~state ~subst ~counter ~scope ~ban_id body_s' body_t' rest
            | (T.Const _, T.Var _) | (T.DB _, T.Var _) ->
                flex_rigid ~state ~subst ~counter ~scope ~ban_id body_t' body_s' rest
            | T.Const f , T.Const g when ID.equal f g && List.length args_s = List.length args_t ->
                unify ~state ~subst ~counter ~scope (build_constraints ~ban_id args_s args_t rest)
            | T.AppBuiltin(hd_s, args_s'), T.AppBuiltin(hd_t, args_t') when
                Builtin.equal hd_s hd_t && 
                  List.length args_s' + List.length args_s = 
                  List.length args_t' + List.length args_t ->
                unify ~state ~subst ~counter ~scope (build_constraints ~ban_id (args_s'@args_s) (args_t'@args_t) rest)
            | T.DB i, T.DB j when i = j && List.length args_s = List.length args_t ->
                unify ~state ~subst ~counter ~scope (build_constraints ~ban_id args_s args_t rest)  
            | _ -> OSeq.empty
          )
        | None -> OSeq.empty)
        else (
          unify ~state ~scope ~counter ~subst rest
        )
    )
  )

(* Create identification substitution, apply it and continue solving the problem *)
and identify ~state ~subst ~counter ~scope s t rest =
  let state = {state with 
                  num_identifications = state.num_identifications + 1;
                  depth               = state.depth + 1} in
  if state.num_identifications <= !max_identifications then ( 
    let id_subs = OSeq.nth 0 (JP_unif.identify ~scope ~counter s t []) in
    let subst = compose_sub subst id_subs in
    unify ~state ~scope ~counter ~subst ((s, t, true)::rest)
  ) 
  else OSeq.empty

and flex_rigid ~state ~subst ~counter ~scope ~ban_id s t rest =
  assert (T.is_var @@ T.head_term s);
  assert (not @@ T.is_var @@ T.head_term t);  
  let bindings = proj_imit_bindings ~state ~subst ~scope ~counter s t in
  let substs = List.map (fun (s, n_args) -> 
    compose_sub subst s, n_args) bindings in
  OSeq.of_list substs
  |> OSeq.flat_map (fun (subst,n_args) ->
    let state = 
      {state with 
          depth               = state.depth + 1;
          num_app_projections = state.num_app_projections + 
                                if n_args > 0 then 1 else 0;} in
    if state.num_app_projections <= !max_app_projections then (
      unify ~state ~scope  ~counter ~subst ((s, t,ban_id) :: rest)
    ) else OSeq.empty
  )

and flex_same ~subst ~state ~counter ~scope hd_s args_s args_t rest all =
  assert(T.is_var hd_s);
  assert(List.length args_s = List.length args_t);
  assert(List.length args_s <= List.length @@ fst @@ Type.open_fun (T.ty hd_s));
  if List.length args_s > 0 then (
    let state = {state with depth = state.depth + 1} in
    let new_cstrs = build_constraints ~ban_id:true args_s args_t rest in
    let all_vars = CCList.range 0 ((List.length args_s) -1 ) in
    let all_args_unif = unify ~state ~subst ~counter ~scope new_cstrs in
    let first_unif = OSeq.take 1 all_args_unif in
    if !_cons_e && not (OSeq.is_empty first_unif)  then (
      first_unif
    ) 
    else (
      if state.num_elims < !max_elims then (
        assert(List.length all_vars != 0);
        OSeq.append
          all_args_unif
          (OSeq.of_list all_vars
          |> OSeq.filter_map (fun idx -> 
              assert(idx >= 0);
              if not (T.equal (List.nth args_s idx) (List.nth args_t idx)) then
                Some (eliminate_at_idx ~scope ~counter (T.as_var_exn hd_s) idx)
              else None) 
          |> (OSeq.flat_map (fun subst' -> 
              let subst = compose_sub subst subst' in
                unify ~state:{state with num_elims=state.num_elims+1} 
                      ~scope  ~counter ~subst all))))
      else all_args_unif
    )
  )
  else unify ~subst ~state ~counter ~scope rest

(* Create all possible elimination substitutions, apply them and continue solving *)
and eliminate_subs ~state ~subst ~counter ~scope t constraints = 
  let hd, args = T.as_app t in
  if T.is_var hd && List.length args > 0 then (
    let all_vars = CCList.range 0 ((List.length args)-1) in
      OSeq.of_list all_vars
      |> OSeq.map (eliminate_at_idx ~scope ~counter (T.as_var_exn hd))
      |> OSeq.flat_map (fun subst' -> 
          let new_subst = compose_sub subst subst' in
          unify ~state:{state with depth=state.depth+1} ~scope  ~counter ~subst:new_subst constraints))
  else OSeq.empty

and flex_proj_imit ~subst ~state ~counter ~scope s t rest = 
  let bindings = proj_imit_bindings ~subst ~state  ~scope ~counter s t in
  let bindings = proj_imit_bindings ~subst ~state ~scope ~counter t s @ bindings in
  let substs = List.map (fun (s,num_args) -> compose_sub subst s, num_args) bindings in
  OSeq.of_list substs
  |> OSeq.flat_map (fun (subst,num_args) ->
      let proj_dif = if num_args > 0 then 1 else 0 in
      let imit_dif = if num_args = -1 then 1 else 0 in
      let state = {
        state with 
          num_app_projections = state.num_app_projections + proj_dif;
          num_var_imitations  = state.num_var_imitations + imit_dif;
          depth               = state.depth + 1
      } in
      if state.num_app_projections <= !max_app_projections && 
         state.num_var_imitations <= !max_var_imitations then (
        unify ~scope ~state ~counter ~subst ((s, t, true) :: rest)
      ) 
      else OSeq.empty
  )

let unify_scoped t0_s t1_s =
  let counter = ref 0 in
  let lfho_unif = 
    try 
      let unif = Unif.FO.unify_syn ~subst:(Subst.empty) t0_s t1_s in
      Some (US.of_subst unif)
    with Unif.Fail -> None in
  let t0',t1',unifscope,subst = US.FO.rename_to_new_scope ~counter t0_s t1_s in
  let prefix = if (CCOpt.is_some lfho_unif) then OSeq.cons lfho_unif else (fun x -> x) in
  let state = 
  {
    num_identifications = 0;
    num_var_imitations  = 0;
    num_app_projections = 0;
    num_elims           = 0;
    depth               = 0
  } in
  let res =
      prefix
      (unify ~state ~scope:unifscope ~counter ~subst [t0', t1', false]) 
      (* OSeq.empty *)
  in

  res
  |> OSeq.map (CCOpt.map (fun sub ->       
      (* let l = Lambda.eta_expand @@ Lambda.snf @@ S.apply sub t0_s in 
      let r = Lambda.eta_expand @@ Lambda.snf @@ S.apply sub t1_s in
      assert(Type.equal (Term.ty l) (Term.ty r));
      if not (T.equal l r) then (
        Format.printf "For problem: %a =?= %a\n" T.pp t0' T.pp t1';
        Format.printf "Subst: @[%a@]\n" S.pp sub;
        Format.printf "%a <> %a\n" T.pp l T.pp r;
        assert(false);
      );
      assert (T.equal l r); *)
    sub))
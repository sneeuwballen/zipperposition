module T = Term
module S = Subst
module PU = PatternUnif

exception CoveringImpossible

let rec all_combs = function 
  | [] -> []
  | x::xs ->
      let rest_combs = all_combs xs in
      if CCList.is_empty rest_combs then CCList.map (fun t->[t]) x 
      else CCList.flat_map 
            (fun i -> CCList.map (fun comb -> i::comb) rest_combs) 
           x

let cover_rigid_skeleton t solids =
  assert(List.for_all T.is_ground solids);
  (* If the term is not of base type, then it must be a bound variable *)
  assert(List.for_all (fun t -> not @@ Type.is_fun @@ T.ty t || T.is_bvar t) solids);

  let n = List.length solids in
  let sols_as_db = List.mapi (fun i t -> (t,T.bvar ~ty:(T.ty t) (n-i-1))) solids in

  let rec aux t =
    (* All the ways in which we can represent term t using solids *)
    let db_hits = 
      CCList.filter_map (fun (s, s_db) -> 
        if T.equal s t then Some s_db else None) 
      sols_as_db in
    let rest = 
      try 
        match T.view t with
        | AppBuiltin (hd,args) ->
          if CCList.is_empty args then [T.app_builtin ~ty:(T.ty t) hd []]
          else (
            let args_combined = all_combs (List.map aux args) in
            List.map (fun args -> T.app_builtin ~ty:(T.ty t) hd args) args_combined
          )
        | App(hd,args) ->
          if Term.is_var hd then [t]
          else (
            assert(not (CCList.is_empty args));
            let hd, args = T.head_term_mono t, CCList.drop_while T.is_type args in
            let hd_args_combined = all_combs (aux hd :: (List.map aux args)) in
            List.map (fun l -> T.app (List.hd l) (List.tl l)) hd_args_combined
          )
        | Fun _ -> 
          let ty_args, body = T.open_fun t in
          let pref_len = List.length ty_args in
          let res = aux (T.DB.unshift pref_len body) in
          List.map (fun r -> T.fun_l ty_args (T.DB.shift pref_len r)) res
        | DB i when i >= 0 -> []
        | _ -> [t]
      with CoveringImpossible -> [] in
    if CCList.is_empty db_hits && CCList.is_empty rest 
    then raise CoveringImpossible
    else db_hits @ rest in
  
  try
    aux t
  with CoveringImpossible -> []

let collect_flex_flex ~subst ~counter ~scope t args  =
  let t' = S.FO.apply S.Renaming.none subst (t, scope) in
  let t' = Lambda.eta_expand @@ Lambda.snf t' in

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
        let bvars = 
          List.mapi (fun i ty -> (i,ty)) bvar_tys
          |> List.rev_map (fun (i,ty) -> T.bvar ~ty i) in
        let n_bvars = List.length bvars in
        let args' = (List.map (T.DB.shift n_bvars) args) @ bvars in
        let fresh_var_ty = Type.arrow (List.map T.ty args') (T.ty t) in
        let fresh_var = HVar.fresh_cnt ~counter ~ty:fresh_var_ty () in
        let replacement = T.app (T.var fresh_var) args' in
        replacement, [replacement, t]  
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
    | _ -> t, [] in

  aux ~bvar_tys:[] t'

let solve_flex_flex ~subst ~counter ~scope lhs rhs =
  let norm_args args =
    List.map (fun arg -> 
      let ty = T.ty arg in
      if Type.is_fun ty then (
        let arg = Lambda.eta_reduce arg in
        if Term.is_bvar arg then arg else raise PU.NotInFragment
      ) else (
        let arg = S.FO.apply S.Renaming.none subst (arg, scope) in
        let arg = Lambda.snf arg in
        if Term.is_ground arg then arg else raise PU.NotInFragment
      )
    ) args in

  assert(Term.is_app_var lhs && T.is_app_var rhs);
  assert(Type.equal (Term.ty lhs) (Term.ty rhs));
  let hd_l, args_l, n_l = T.as_var_exn @@ T.head_term lhs, 
                          norm_args @@ T.args lhs, List.length @@ T.args lhs in
  let hd_r, args_r, n_r = T.as_var_exn @@ T.head_term rhs, 
                          norm_args @@ T.args rhs, List.length @@ T.args rhs in
  
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
  subst 



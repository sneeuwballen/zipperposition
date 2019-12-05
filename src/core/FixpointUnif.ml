module T = Term
module US = Unif_subst

exception NotUnif = PatternUnif.NotUnifiable
exception DontKnow = PatternUnif.NotInFragment

(* 
  an example showing that variable appearing under a rigid symbol can still
  unify: F =?= \u. u (F (\vw.v)) (G F)
  A unifier is {F |-> \u.u Y Z; G |-> Z} 


  Nonunifiable rigid path is the one with ends up with nonapplied variable,
  or which is for the one that has an empty prefix for the original rhs
*)

let norm_deref = PatternUnif.norm_deref

(* If there is a nonunifiable rigid path raises NotUnif
   If the variable occurs on a flex path or unifiable rigid path returns None
   Otherwise, variable does not occur and it returns the term var needs to be bound to *)
let path_check ~subst ~scope var t =
  let pref, _ = T.open_fun t in
  let no_prefix = CCList.is_empty pref in

  let rec aux ~depth ~under_var t =
    let t = norm_deref subst (t,scope) in
    match T.view t with
    | T.App(hd,args) when T.is_var hd ->
      assert(not (CCList.is_empty args));
      assert(not (US.FO.mem subst (T.as_var_exn hd,scope)));
      if T.equal hd var then (
        if under_var || not no_prefix then None
        else raise NotUnif)
      else (
        CCOpt.map (fun args' -> 
            if T.same_l args args' then t else T.app hd args') 
          (aux_l ~depth ~under_var:true args))
    | T.App(hd,args) -> 
      assert(not (T.is_fun hd));
      begin match aux ~depth ~under_var hd with
        | None -> None
        | Some hd' -> 
          CCOpt.map (fun args' -> 
              if T.same_l args args' && T.equal hd hd' then t 
              else T.app hd' args') 
            (aux_l ~depth ~under_var args) end
    | T.AppBuiltin(b, args) -> 
      CCOpt.map (fun args' -> 
          if T.same_l args args' then t 
          else T.app_builtin ~ty:(T.ty t) b args') 
        (aux_l ~depth ~under_var args)
    | T.Var _ ->
      assert(not (US.FO.mem subst (T.as_var_exn t,scope)));
      if T.equal var t then
        (if under_var || Type.is_fun (T.ty t) then None else raise NotUnif)
      else Some t
    | T.Fun _ ->
      let pref_tys, body' = T.open_fun t in
      let depth_inc = List.length pref_tys in
      begin match aux ~depth:(depth+depth_inc) ~under_var body' with
        | None -> None
        | Some t' -> 
          if T.equal t' t then Some t
          else Some (T.fun_l pref_tys t') end
    | T.DB i when i >= depth -> 
      if under_var then None else raise NotUnif
    | _ -> Some t 
  and aux_l ~depth ~under_var args =
    match args with 
    | [] -> Some []
    | x :: xs ->
      let xs' = aux_l ~depth ~under_var xs in
      match aux ~depth ~under_var x with
      | None -> ignore(xs'); None
      | Some t -> CCOpt.map (fun ts -> t :: ts) xs' in

  aux ~depth:0 ~under_var:false t

let unify_scoped ?(subst=US.empty) ?(counter = ref 0) t0_s t1_s =
  let driver s t scope subst =
    let s, t = Lambda.eta_reduce @@ norm_deref subst(s,scope),
               Lambda.eta_reduce @@ norm_deref subst(t,scope) in
    if T.is_var s && T.is_var t then (
      if T.equal s t then subst
      else US.FO.bind subst (T.as_var_exn s, scope) (t, scope))
    else if not (T.is_var s) && not (T.is_var t) then (
      raise DontKnow) 
    else (
      let var, rigid = if T.is_var s then s, t else t,s in
      match path_check ~subst ~scope var rigid with 
      | None -> 
        raise DontKnow
      | Some rigid ->
        assert (T.DB.is_closed rigid);
        US.FO.bind subst (T.as_var_exn var, scope) (rigid, scope)) in

  if US.is_empty subst then (
    let t0',t1',scope,subst = US.FO.rename_to_new_scope ~counter t0_s t1_s in
    driver t0' t1' scope subst)
  else (
    if Scoped.scope t0_s != Scoped.scope t1_s then (
      raise (Invalid_argument "scopes should be the same"))
    else (
      let t0', t1' = fst t0_s, fst t1_s in
      driver t0' t1' (Scoped.scope t0_s) subst
    )) 

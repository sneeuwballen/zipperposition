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

(* If there is a nonunifiable rigid path raises NotUnifiable
   If the variable occurs on a flex path or unifiable rigid path raises
   NotInFragment Otherwise, returns true *)
let path_check ~subst ~scope var t =
  let pref, body = T.open_fun t in
  let no_prefix = CCList.is_empty pref in

  let rec aux ~under_var t =
    let t = norm_deref subst (t,scope) in
    let _,t = T.open_fun t in
    match T.view t with
    | T.App(hd,args) when T.is_var hd ->
      assert(not (CCList.is_empty args));
      assert(not (US.FO.mem subst (T.as_var_exn hd,scope)));
      if T.equal hd var then (
        if under_var || not no_prefix then false
        else raise NotUnif)
      else aux_l ~under_var:true args
    | T.App(hd,args) -> aux_l ~under_var args
    | T.AppBuiltin(b, args) -> aux_l ~under_var args
    | T.Var _ ->
      assert(not (US.FO.mem subst (T.as_var_exn t,scope)));
      if T.equal var t then
        (if under_var then false else raise NotUnif)
      else true
    | _ -> true 
  and aux_l ~under_var = function 
    | [] -> true
    | x :: xs -> 
      (* not short-circuting since we have to inspect all possible rigid paths *)
      aux_l ~under_var xs && aux ~under_var x in
  
  aux ~under_var:false t

let unify_scoped ?(subst=US.empty) ?(counter = ref 0) t0_s t1_s =
  let driver s t scope subst =
    let s, t = Lambda.eta_reduce s, Lambda.eta_reduce t in
    if T.is_var s && T.is_var t then (
      if T.equal s t then subst
      else US.FO.bind subst (T.as_var_exn s, scope) (t, scope))
    else if not (T.is_var s) && not (T.is_var t) then (
      raise DontKnow) 
    else (
      let var, rigid = if T.is_var s then s, t else t,s in
      if path_check ~subst ~scope var rigid 
      then (US.FO.bind subst (T.as_var_exn var, scope) (rigid, scope))
      else raise DontKnow)
  in

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

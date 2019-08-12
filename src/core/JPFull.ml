module U = Unif_subst
module T = Term
module H = HVar
module S = Subst

let elim_rule ~counter ~scope t u = 
  let eliminate_at_idx v k =  
    let prefix_types, return_type = Type.open_fun (HVar.ty v) in
    let m = List.length prefix_types in
    let bvars = List.mapi (fun i ty -> T.bvar ~ty (m-1-i)) prefix_types in
    let prefix_types' = CCList.remove_at_idx k prefix_types in
    let new_ty = Type.arrow prefix_types' return_type in
    let bvars' = CCList.remove_at_idx k bvars in
    let matrix_head = T.var (H.fresh_cnt ~counter ~ty:new_ty ()) in
    let matrix = T.app matrix_head bvars' in
    let subst_value = T.fun_l prefix_types matrix in
    let subst = S.FO.bind' Subst.empty (v, scope) (subst_value, scope) in
    subst in 
  
  let eliminate_one t = 
    let hd, args = T.as_app t in
    if T.is_var hd && List.length args > 0 then (
      let all_vars = CCList.range 0 ((List.length args)-1) in
        OSeq.of_list all_vars
        |> OSeq.map (eliminate_at_idx (T.as_var_exn hd)))
    else OSeq.empty in
  OSeq.append (eliminate_one t) (eliminate_one u)
  |> OSeq.map (fun x -> Some x)

let iter_rule ~counter ~scope t u  =
  JP_unif.iterate ~scope ~counter t u []
  |> OSeq.map (CCOpt.map (U.subst))

let imit_rule ~counter ~scope t u =
  JP_unif.imitate ~scope ~counter t u []
  |> OSeq.map (fun x -> Some (U.subst x))

let proj_rule ~counter ~scope s t =
  OSeq.append
    (JP_unif.project_onesided ~scope ~counter s)
    (JP_unif.project_onesided ~scope ~counter t)
  |> OSeq.map (fun x -> Some (U.subst x))

let ident_rule ~counter ~scope t u = 
  JP_unif.identify ~scope ~counter t u []
  |> OSeq.map (fun x -> Some (U.subst x))

let renamer ~counter t0s t1s = 
  let lhs,rhs, unifscope, us = U.FO.rename_to_new_scope ~counter t0s t1s in
  lhs,rhs,unifscope,U.subst us

let pattern_frag ~counter =
  [(fun s t sub -> U.subst @@  PatternUnif.unify_scoped ~subst:(U.of_subst sub) ~counter s t)]

let head_classifier s =
  match T.view @@ T.head_term s with 
  | T.Var x -> `Flex x
  | _ -> `Rigid

let oracle ~counter ~scope (s,_) (t,_) _ = 
  match head_classifier s, head_classifier t with 
  | `Flex x, `Flex y when HVar.equal (fun _ _ -> true) x y ->
    (* eliminate + iter *)
    OSeq.interleave (elim_rule ~counter ~scope s t)
                    (iter_rule ~counter ~scope s t)
  | `Flex x, `Flex y ->
    (* all rules  *)
    OSeq.append 
      (elim_rule ~counter ~scope s t)
      (OSeq.append 
        (proj_rule ~counter ~scope s t)
        (OSeq.append 
          (ident_rule ~counter ~scope s t)
          (OSeq.interleave 
            (iter_rule ~counter ~scope s t)
            (imit_rule ~counter ~scope s t))))
  | `Flex _, `Rigid
  | `Rigid, `Flex _ ->
    OSeq.append
      (proj_rule ~counter ~scope s t)
      (OSeq.interleave 
            (iter_rule ~counter ~scope s t)
            (imit_rule ~counter ~scope s t))
  | _ -> assert false

let unify_scoped =
  let enclose_with_flag flag =
    OSeq.map (CCOpt.map (fun x -> x, flag)) in
  
  let counter = ref 0 in

  let module JPFullParams = struct
    exception NotInFragment
    exception NotUnifiable
    type flag_type = int
    let init_flag = (0:flag_type)
    let identify_scope = renamer ~counter
    let frag_algs = pattern_frag ~counter
    let pb_oracle s t (f:flag_type) scope = enclose_with_flag init_flag (oracle ~counter ~scope s t f) 
  end in
  
  let module JPFull = UnifFramework.Make(JPFullParams) in
  JPFull.unify_scoped
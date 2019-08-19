module U = Unif_subst
module T = Term
module H = HVar
module S = Subst


let delay depth res =
  (* CCFormat.printf "depth: %d@." depth; *)
  OSeq.append 
    (OSeq.take (depth*10) (OSeq.repeat None))
    res 
  (* res *)

let elim_rule ~counter ~scope t u depth = 
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
  |> OSeq.map (fun x -> Some (x, depth))

let iter_rule ~counter ~scope t u depth  =
  delay depth
    (JP_unif.iterate ~scope ~counter t u []
    |> OSeq.map (CCOpt.map (fun s -> U.subst s, depth+1)))

let imit_rule ~counter ~scope t u depth =
  delay depth
    (JP_unif.imitate ~scope ~counter t u []
    |> OSeq.map (fun x -> Some (U.subst x, depth+1)))

let proj_rule ~counter ~scope s t depth =
  OSeq.append
    (JP_unif.project_onesided ~scope ~counter s)
    (JP_unif.project_onesided ~scope ~counter t)
  |> OSeq.map (fun x -> Some (U.subst x, depth))

let ident_rule ~counter ~scope t u depth = 
  delay depth
    (JP_unif.identify ~scope ~counter t u []
    |> OSeq.map (fun x -> Some (U.subst x, depth+1)))

let renamer ~counter t0s t1s = 
  let lhs,rhs, unifscope, us = U.FO.rename_to_new_scope ~counter t0s t1s in
  lhs,rhs,unifscope,U.subst us

let pattern_frag ~counter =
  [(fun s t sub -> U.subst @@  PatternUnif.unify_scoped ~subst:(U.of_subst sub) ~counter s t)]

let head_classifier s =
  match T.view @@ T.head_term s with 
  | T.Var x -> `Flex x
  | _ -> `Rigid

let oracle ~counter ~scope (s,_) (t,_) flag = 
  match head_classifier s, head_classifier t with 
  | `Flex x, `Flex y when HVar.equal Type.equal x y ->
    (* eliminate + iter *)
    (* OSeq.append 
      (proj_rule ~counter ~scope s t flag) *)
      (OSeq.append (elim_rule ~counter ~scope s t flag)
                   (iter_rule ~counter ~scope s t flag))
  | `Flex x, `Flex y ->
    (* all rules  *)
    OSeq.append 
      (proj_rule ~counter ~scope s t flag)
        (OSeq.append 
          (imit_rule ~counter ~scope s t flag)
          (OSeq.append 
            (ident_rule ~counter ~scope s t flag)
            (iter_rule ~counter ~scope s t flag)))
  | `Flex _, `Rigid
  | `Rigid, `Flex _ ->
    OSeq.append
      (proj_rule ~counter ~scope s t flag)
      (OSeq.append 
        (imit_rule ~counter ~scope s t flag)
        (iter_rule ~counter ~scope s t flag))
  | _ -> 
    CCFormat.printf "Did not disassemble properly: [%a]\n[%a]@." T.pp s T.pp t;
    assert false

let unify_scoped =  
  let counter = ref 0 in

  let module JPFullParams = struct
    exception NotInFragment = PatternUnif.NotInFragment
    exception NotUnifiable = PatternUnif.NotUnifiable
    type flag_type = int
    let init_flag = (0:flag_type)
    let identify_scope = renamer ~counter
    let frag_algs = pattern_frag ~counter
    let pb_oracle s t (f:flag_type) scope = 
      oracle ~counter ~scope s t f 
  end in
  
  let module JPFull = UnifFramework.Make(JPFullParams) in
  (fun x y -> 
    OSeq.map (CCOpt.map Unif_subst.of_subst) (JPFull.unify_scoped x y))
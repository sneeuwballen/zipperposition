module U = Unif_subst
module T = Term
module H = HVar
module S = Subst

let skip = 10

let delay depth res =
  (* CCFormat.printf "depth:%d@." depth; *)
    OSeq.append
      (OSeq.take (10*depth) (OSeq.repeat None))
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
  eliminate_one t
  |> OSeq.map (fun x -> Some (x, depth))

let iter_rule ~counter ~scope t u depth  =
  JP_unif.iterate ~scope ~counter t u []
  |> OSeq.map (CCOpt.map (fun s -> U.subst s, depth+1))

let imit_rule ~counter ~scope t u depth =
  JP_unif.imitate ~scope ~counter t u []
  |> OSeq.map (fun x -> Some (U.subst x, depth+1))

let hs_proj_flex_rigid ~counter ~scope ~flex u depth =
  PUnif.proj_hs ~counter ~scope ~flex u
   |> OSeq.of_list
   |> OSeq.map (fun x -> Some (x,depth+1))

let proj_rule ~counter ~scope s t depth =
  OSeq.append
    (JP_unif.project_onesided ~scope ~counter s)
    (JP_unif.project_onesided ~scope ~counter t)
  |> OSeq.map (fun x -> Some (U.subst x, depth))

let ident_rule ~counter ~scope t u depth = 
  JP_unif.identify ~scope ~counter t u []
  |> OSeq.map (fun x -> Some (U.subst x, depth+1))

let renamer ~counter t0s t1s = 
  let lhs,rhs, unifscope, us = U.FO.rename_to_new_scope ~counter t0s t1s in
  lhs,rhs,unifscope,U.subst us

let deciders ~counter () =
  let pattern = 
    if !PragUnifParams.pattern_decider then 
      [(fun s t sub -> [(U.subst @@ PatternUnif.unify_scoped ~subst:(U.of_subst sub) ~counter s t)])] 
    else [] in
  let solid = 
    if !PragUnifParams.solid_decider then 
      [(fun s t sub -> (List.map U.subst @@ SolidUnif.unify_scoped ~subst:(U.of_subst sub) ~counter s t))] 
    else [] in
  pattern @ solid

let head_classifier s =
  match T.view @@ T.head_term s with 
  | T.Var x -> `Flex x
  | _ -> `Rigid

let oracle ~counter ~scope (s,_) (t,_) flag = 
  match head_classifier s, head_classifier t with 
  | `Flex x, `Flex y when HVar.equal Type.equal x y ->
    (* eliminate + iter *)
    CCList.filter_map CCFun.id (OSeq.to_list (elim_rule ~counter ~scope s t flag)),
    delay (flag+1) @@ iter_rule ~counter ~scope s t flag
  | `Flex _, `Flex _ ->
    (* all rules  *)
    CCList.filter_map CCFun.id 
      (List.append (OSeq.to_list (proj_rule ~counter ~scope s t flag))
                   (OSeq.to_list (ident_rule ~counter ~scope s t flag))),
    delay (flag+1) @@ iter_rule ~counter ~scope s t flag  
  | `Flex _, `Rigid
  | `Rigid, `Flex _ ->
    CCList.filter_map CCFun.id  @@
      CCList.append 
        (OSeq.to_list (
            let flex, rigid = if Term.is_var (T.head_term s) then s,t else t,s in
            hs_proj_flex_rigid ~counter ~scope ~flex rigid flag))
        (OSeq.to_list (imit_rule ~counter ~scope s t flag)), 
    OSeq.empty
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
    let frag_algs = deciders ~counter
    let pb_oracle s t (f:flag_type) _ scope = 
      oracle ~counter ~scope s t f
    let oracle_composer = OSeq.append
  end in
  
  let module JPFull = UnifFramework.Make(JPFullParams) in
  (fun x y -> 
    OSeq.map (CCOpt.map Unif_subst.of_subst) (JPFull.unify_scoped x y))
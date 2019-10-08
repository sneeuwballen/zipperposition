module U = Unif_subst
module T = Term
module H = HVar
module S = Subst
module IntSet = PUnif.IntSet

let skip = 10

let elim_vars = ref IntSet.empty

let delay depth res =
    OSeq.append
      (OSeq.take (10*depth) (OSeq.repeat None))
      res

let iter_rule ?(flex_same=false) ~counter ~scope t u (depth,_)  =
  JP_unif.iterate ~flex_same ~scope ~counter t u []
  |> OSeq.map (CCOpt.map (fun s -> U.subst s, (depth+1,false)))

let imit_rule ~counter ~scope t u (depth,_) =
  JP_unif.imitate ~scope ~counter t u []
  |> OSeq.map (fun x -> Some (U.subst x, (depth+1,false)))

let hs_proj_flex_rigid ~counter ~scope ~flex u (depth,ident_last) =
  if ident_last then OSeq.empty
  else(
    PUnif.proj_hs ~counter ~scope ~flex u
      |> OSeq.of_list
      |> OSeq.map (fun x -> Some (x,(depth+1,false))))

let proj_rule ~counter ~scope s t (depth,ident_last) =
  if ident_last then OSeq.empty
  else (
    OSeq.append
      (JP_unif.project_onesided ~scope ~counter s)
      (JP_unif.project_onesided ~scope ~counter t)
    |> OSeq.map (fun x -> Some (U.subst x, (depth+1,false))))

let ident_rule ~counter ~scope t u (depth,ident_last) = 
  JP_unif.identify ~scope ~counter t u []
  |> OSeq.map (fun x -> Some (U.subst x, (depth+1,true)))

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

let oracle ~counter ~scope (s,_) (t,_) ((depth,ident_last) as flag) =
  let hd_t, hd_s = T.head_term s, T.head_term t in
  if T.is_var hd_t && T.is_var hd_s && T.equal hd_s hd_t &&
     IntSet.mem (HVar.id @@ T.as_var_exn hd_t) !elim_vars then (
    [], OSeq.empty)
  else (
    match head_classifier s, head_classifier t with 
    | `Flex x, `Flex y when HVar.equal Type.equal x y ->
      (* eliminate + iter *)
      [],
      OSeq.append 
        (OSeq.map (fun x -> Some x) @@
           PUnif.elim_subsets_rule ~max_elims:None ~elim_vars ~counter ~scope s t (fst flag))
        (delay (depth+1) @@ iter_rule ~flex_same:true ~counter ~scope s t flag)
    | `Flex _, `Flex _ ->
      (* all rules  *)
      CCList.filter_map CCFun.id 
        (List.append (OSeq.to_list (proj_rule ~counter ~scope s t flag))
                     (OSeq.to_list (ident_rule ~counter ~scope s t flag))),
      delay (depth+1) @@ iter_rule ~counter ~scope s t flag  
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
      assert false
  )

let unify_scoped =  
  let counter = ref 0 in

  let module JPFullParams = struct
    exception NotInFragment = PatternUnif.NotInFragment
    exception NotUnifiable = PatternUnif.NotUnifiable
    type flag_type = int*bool
    let init_flag = (0,false:flag_type)
    let identify_scope = renamer ~counter
    let frag_algs = deciders ~counter
    let pb_oracle s t (f:flag_type) _ scope = 
      oracle ~counter ~scope s t f
    let oracle_composer = OSeq.append
  end in
  
  let module JPFull = UnifFramework.Make(JPFullParams) in
  (fun x y ->
    elim_vars := IntSet.empty;
    OSeq.map (CCOpt.map Unif_subst.of_subst) (JPFull.unify_scoped x y))
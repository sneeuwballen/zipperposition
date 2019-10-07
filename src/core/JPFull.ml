module U = Unif_subst
module T = Term
module H = HVar
module S = Subst
module IntSet = Set.Make(CCInt)

let skip = 10

let eliminated_vars = ref IntSet.empty

let delay depth res =
  (* CCFormat.printf "depth:%d@." depth; *)
    OSeq.append
      (OSeq.take (10*depth) (OSeq.repeat None))
      res
  (* res *)

let delay_l depth res =
  let n = if depth > 5 then depth else 0 in 
  CCList.append (CCList.replicate n None) res


let k_subset ~k l =
  let rec aux i acc l = 
    if i = 0 then OSeq.return acc
    else if i > List.length l then OSeq.empty 
    else (
      match l with 
      | x :: xs ->
        OSeq.append (aux i acc xs) (aux (i-1) (x::acc) xs)
      | [] -> assert(false)
    ) in
  
  assert(k>=0);
  let res = aux k [] l in
  (* CCFormat.printf "new prob: @[%a@]@." (CCList.pp T.pp) l;
  OSeq.iter ( fun subset ->
    CCFormat.printf "%d:@[%a@]/@[%a@]@." k (CCList.pp T.pp) subset (CCList.pp T.pp) l;
  ) res; *)
  res

let elim_subsets_rule ~counter ~scope t u depth =
  let hd_t, args_t = T.head_term t, Array.of_list (T.args t) in
  let hd_u, args_u = T.head_term u, Array.of_list (T.args u) in
  assert(T.is_var hd_t);
  assert(T.is_var hd_u);
  assert(T.equal hd_t hd_u);

  let hd_var = T.as_var_exn hd_t in
  let var_id = !counter in
  eliminated_vars := IntSet.add var_id !eliminated_vars;
  incr counter;

  let pref_tys, ret_ty = Type.open_fun (T.ty hd_t) in
  let pref_len = List.length pref_tys in

  let same_args, diff_args = 
    List.mapi (fun i ty -> 
      if i < Array.length args_t && i < Array.length args_u &&
        T.equal args_t.(i) args_u.(i) 
      then `Left (T.bvar ~ty (pref_len-i-1))
      else `Right (T.bvar ~ty (pref_len-i-1))) pref_tys
    |> CCList.partition_map CCFun.id in


  let diff_args_num = List.length diff_args in
  CCList.range_by (diff_args_num-1) 0 ~step:(-1)
  |> OSeq.of_list
  |> OSeq.flat_map (fun k ->
    k_subset ~k diff_args
    |> OSeq.map (fun diff_args_subset ->
      assert(List.length diff_args_subset = k);
      let all_args = diff_args_subset @ same_args in
      assert(List.length all_args <= pref_len);
      let arg_tys = List.map T.ty all_args in
      let ty = Type.arrow arg_tys ret_ty in
      let matrix = T.app (T.var (HVar.make ~ty var_id)) all_args in
      let subs_term = T.fun_l pref_tys matrix in
      assert(T.DB.is_closed subs_term);
      (* CCFormat.printf "%d(s:%d,d:%d)/%d:%a@." pref_len (List.length same_args) diff_args_num k T.pp subs_term; *)
      Some (Subst.FO.bind' Subst.empty (hd_var, scope) (subs_term, scope),depth+1)))


let iter_rule ?(flex_same=false) ~counter ~scope t u depth  =
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
  let hd_t, hd_s = T.head_term s, T.head_term t in
  if T.is_var hd_t && T.is_var hd_s && T.equal hd_s hd_t &&
     IntSet.mem (HVar.id @@ T.as_var_exn hd_t) !eliminated_vars then (
    [], OSeq.empty)
  else (
    match head_classifier s, head_classifier t with 
    | `Flex x, `Flex y when HVar.equal Type.equal x y ->
      (* eliminate + iter *)
      [],
      OSeq.append 
        (delay (flag+1) @@ elim_subsets_rule ~counter ~scope s t flag)
        (delay (flag+1) @@ iter_rule ~counter ~scope s t flag)
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
  )

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
    eliminated_vars := IntSet.empty;
    OSeq.map (CCOpt.map Unif_subst.of_subst) (JPFull.unify_scoped x y))
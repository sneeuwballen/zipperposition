module U = Unif_subst
module T = Term
module H = HVar
module S = Subst
module P = PatternUnif
module Params = PragUnifParams

type op =
  | ProjApp
  | ImitFlex
  | ImitRigid
  | Ident
  | Elim

let op_masks =
  [ProjApp, (63, 0, "proj");
   ImitFlex, (63 lsl 6, 6, "imit_flex");
   ImitRigid, (63 lsl 12, 12, "imit_rigid");
   Ident, (63 lsl 18, 18, "ident");
   Elim, (63 lsl 24, 24, "elim")]

let get_op flag op =
  let mask,shift,name = List.assoc op op_masks in
  (flag land mask) lsr shift

let inc_op flag op =
  let old = get_op flag op in
  let mask, shift, _ = List.assoc op op_masks in
  let op_val = (((flag land mask) lsr shift) + 1) lsl shift in
  let res = (flag land (lnot mask)) lor op_val in
  assert(old + 1 = (get_op res op));
  res

let pp_flag out flag =
  List.iter (fun (op, (_,_,name)) ->
     CCFormat.fprintf out "|%s:%d" name (get_op flag op);
  ) op_masks

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

(* Sometimes the head of the term can be a quantifier. Inside
   the quantifier body, we can have some variables that have
   to be dereferenced. *)
let [@inline] handle_quants ~subst ~scope s =
  let _, body = Term.open_fun s in
  match Term.view body with
  | AppBuiltin(b,[_]) when Builtin.is_quantifier b ->
      Subst.FO.apply Subst.Renaming.none subst (s,scope)
  | _ -> s

(* Create substitution: v |-> λ u1 ... um. f (H1 u1 ... um) ... (Hn u1 ... um)
   where type of f is τ1 -> ... τn -> τ where τ is atomic, H_i have correct
   type and f is a constant. This substitution is called an imitation.*)
let imitate_one ~scope ~counter  s t =
  try
    OSeq.nth 0 (JP_unif.imitate_onesided ~scope ~counter s t)
  with Not_found ->  invalid_arg "no_imits"

(*Create all possible projection and imitation bindings. *)
let proj_imit_lr ~counter ~scope s t flag =
  try
    let hd_s, args_s = CCPair.map1 T.as_var_exn (T.as_app s) in
    let hd_t,_ = T.as_app (snd (T.open_fun t)) in
    let pref_tys, var_ret_ty = Type.open_fun (HVar.ty hd_s) in
    let proj_bindings = 
      pref_tys
      |> List.mapi (fun i ty -> i, ty)
      |> (fun l ->
          (* if we performed more than N projections that applied the
            bound variable we back off *)
          if get_op flag ProjApp <= !Params.max_app_projections then l
          else
            List.filter (fun (_, ty) ->
              (List.length args_s <= 1) ||  
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
      |> CCList.filter_map (fun (i, ty) ->
          let _, arg_ret_ty = Type.open_fun ty in
          if Type.equal  arg_ret_ty var_ret_ty then (
            (* we project only to arguments of appropriate type *)
            let pr_bind = project_hs_one ~counter pref_tys i ty in
            let max_num_of_apps = 
              if List.length args_s <= 1 then 0  else List.length @@ Type.expected_args ty in
            let flag' = if max_num_of_apps > 0 then inc_op flag ProjApp else flag in
            Some (Subst.FO.bind' Subst.empty (hd_s, scope) (pr_bind, scope), flag'))
          else None) in
      let imit_binding =
        try 
          let flag' = if Term.is_app_var t then inc_op flag ImitFlex else inc_op flag ImitRigid in
          [U.subst @@ imitate_one ~scope ~counter s t, flag']
        with Invalid_argument s when String.equal s "no_imits" -> [] in
    let first, second = 
      if !Params._imit_first then imit_binding, proj_bindings
      else proj_bindings, imit_binding in 
    OSeq.of_list @@ CCList.map (fun x -> Some x) @@ first @ second
  with Invalid_argument s when String.equal s "as_var_exn" ->
    OSeq.empty

let elim_rule ~counter ~scope t u flag = 
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
  |> OSeq.map (fun x -> Some (x, inc_op flag Elim))

let renamer ~counter t0s t1s = 
  let lhs,rhs, unifscope, us = U.FO.rename_to_new_scope ~counter t0s t1s in
  lhs,rhs,unifscope,U.subst us

let pattern_frag ~counter =
  [(fun s t sub -> U.subst @@  PatternUnif.unify_scoped ~subst:(U.of_subst sub) ~counter s t)]

let head_classifier s =
  match T.view @@ T.head_term s with 
  | T.Var x -> `Flex x
  | _ -> `Rigid

let get_depth flag =
  let ops = [ProjApp; ImitFlex; ImitRigid; Ident; Elim] in
  List.fold_left (fun acc o -> get_op flag o + acc ) 0 ops

let oracle ~counter ~scope (s,_) (t,_) flag =
  if get_depth flag < !Params.max_depth then (
    match head_classifier s, head_classifier t with 
    | `Flex x, `Flex y when HVar.equal Type.equal x y ->
      let num_elims = get_op flag Elim in
      if num_elims < !Params.max_elims 
      then elim_rule ~counter ~scope s t flag 
      else OSeq.empty
  | `Flex x, `Flex y ->
      (* all rules  *)
      let var_imits =
        if get_op flag ImitFlex < !Params.max_var_imitations then (
          JP_unif.imitate ~scope ~counter s t []
          |> OSeq.map (fun x -> Some (U.subst x, inc_op flag ImitFlex)))
        else OSeq.empty in
      let ident = 
        if get_op flag Ident < !Params.max_identifications then (
          JP_unif.identify ~scope ~counter s t []
          |> OSeq.map (fun x -> Some (U.subst x, inc_op flag Ident)))
        else OSeq.empty in
      OSeq.append ident var_imits
    | `Flex _, `Rigid
    | `Rigid, `Flex _ ->
      OSeq.append
        (proj_imit_lr ~counter ~scope s t flag)
        (proj_imit_lr ~counter ~scope t s flag) 
    | _ -> 
      CCFormat.printf "Did not disassemble properly: [%a]\n[%a]@." T.pp s T.pp t;
      assert false)
  else OSeq.empty

let unify_scoped =  
  let counter = ref 0 in

  let module PragUnifParams = struct
    exception NotInFragment = PatternUnif.NotInFragment
    exception NotUnifiable = PatternUnif.NotUnifiable
    type flag_type = int
    let init_flag = (0:flag_type)
    let identify_scope = renamer ~counter
    let frag_algs = pattern_frag ~counter
    let pb_oracle s t (f:flag_type) scope = 
      oracle ~counter ~scope s t f
    let oracle_composer = OSeq.append
  end in
  
  let module PragUnif = UnifFramework.Make(PragUnifParams) in
  (fun x y -> 
    OSeq.map (CCOpt.map Unif_subst.of_subst) (PragUnif.unify_scoped x y))
(* This file is free software, part of Logtk. See file "license" for more details. *)

(** {1 Pragmatic variant of JP algorithm} *)

module U = Unif_subst
module T = Term
module H = HVar
module S = Subst
module P = PatternUnif
module Params = PragUnifParams
module I = Int32
module IntSet = CCSet.Make(CCInt)

let elim_vars = ref IntSet.empty
let ident_vars = ref IntSet.empty


type op =
  | ProjApp
  | ImitFlex
  | ImitRigid
  | Ident
  | Elim

let (<<<) = I.shift_left 
let (>>>) = I.shift_right_logical  
let (&&&) = I.logand
let (|||) = I.logor
let (~~~) = I.lognot

let (i63) = I.of_int 63

let op_masks =
  [ProjApp, ( i63, (0), "proj");
   ImitFlex, (i63 <<< 6, 6, "imit_flex");
   ImitRigid, (i63 <<< 12, 12, "imit_rigid");
   Ident, (i63 <<< 18, 18, "ident");
   Elim, (i63 <<< 24, 24, "elim")]

let get_op flag op =
  let mask,shift,_ = List.assoc op op_masks in
  I.to_int ((flag &&& mask) >>> shift)

let get_depth flag =
  let ops = [ProjApp; ImitFlex; ImitRigid; Ident; Elim] in
  List.fold_left (fun acc o -> get_op flag o + acc ) 0 ops

let inc_op flag op =
  let old = get_op flag op in
  let mask, shift, _ = List.assoc op op_masks in
  let op_val = (I.succ ((flag &&& mask) >>> shift)) <<< shift in
  let res = (flag &&& (~~~ mask)) ||| op_val in
  assert( old + 1 = (get_op res op));
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

(* Create substitution: v |-> λ u1 ... um. f (H1 u1 ... um) ... (Hn u1 ... um)
   where type of f is τ1 -> ... τn -> τ where τ is atomic, H_i have correct
   type and f is a constant. This substitution is called an imitation.*)
let imitate_one ~scope ~counter  s t =
  try
    OSeq.nth 0 (JP_unif.imitate_onesided ~scope ~counter s t)
  with Not_found ->  invalid_arg "no_imits"

let proj_lr ~counter ~scope ~subst s t flag max_app_projs = 
  let hd_s, args_s = CCPair.map1 T.as_var_exn (T.as_app s) in
  let argss_arr = CCArray.of_list args_s in
  let hd_t,_ = T.as_app (snd (T.open_fun t)) in
  let pref_tys, hd_ret_ty = Type.open_fun (HVar.ty hd_s) in
  pref_tys
  |> List.mapi (fun i ty -> i, ty)
  |> (fun l ->
      (* if we performed more than N projections that applied the
         bound variable we back off *)
      if get_op flag ProjApp < max_app_projs then l
      else List.filter (fun (_, ty) -> List.length (Type.expected_args ty) = 0) l)
  (* If heads are different constants, do not project to those subterms *)
  |> CCList.filter_map (fun ((i, _) as p) -> 
      if i < List.length args_s then (
        let s_i = snd (T.open_fun argss_arr.(i)) in
        let hd_si = T.head_term s_i in
        if ((T.is_const hd_si && T.is_const hd_t && (not (T.equal hd_si hd_t))) 
            || (T.is_bvar argss_arr.(i) && T.is_bvar hd_t && (not (T.equal argss_arr.(i) hd_t)))) then None 
        else Some p
      ) else Some p
    )
  |> CCList.filter_map(fun (i, ty) ->
      let _, arg_ret_ty = Type.open_fun ty in
      match PatternUnif.unif_simple ~subst ~scope 
              (T.of_ty arg_ret_ty) (T.of_ty hd_ret_ty) with
      | Some subst' ->
        (* we project only to arguments of appropriate type *)
        let subst' = Unif_subst.subst subst' in
        let pr_bind = project_hs_one ~counter pref_tys i ty in
        let max_num_of_apps = 
          List.length @@ Type.expected_args ty in
        let flag' = if max_num_of_apps > 0 then inc_op flag ProjApp else flag in
        (* let flag' = inc_op flag ProjApp in *)
        Some (Subst.FO.bind' subst' (hd_s, scope) (pr_bind, scope), flag')
      | None -> None)

let proj_hs ~counter ~scope ~flex s =
  CCList.map fst @@ proj_lr ~counter ~scope ~subst:Subst.empty flex s Int32.zero max_int

let k_subset ~k l =
  let rec aux i acc l = 
    if i = 0 then OSeq.return acc
    else if i > List.length l then OSeq.empty 
    else (
      match l with 
      | x :: xs ->
        OSeq.interleave (aux i acc xs) (aux (i-1) (x::acc) xs)
      | [] -> assert(false)
    ) in

  assert(k>=0);
  aux k [] l

let elim_subsets_rule  ?(max_elims=None) ~elim_vars ~counter ~scope t u depth =
  let hd_t, args_t = T.head_term t, Array.of_list (T.args t) in
  let hd_u, args_u = T.head_term u, Array.of_list (T.args u) in
  assert(T.is_var hd_t);
  assert(T.is_var hd_u);
  assert(T.equal hd_t hd_u);

  let hd_var = T.as_var_exn hd_t in
  let var_id = !counter in
  elim_vars := IntSet.add var_id !elim_vars;
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
  let end_ = match max_elims with 
    | None -> 0 
    | Some x -> assert(x>0); diff_args_num-x in
  let start,step = max (diff_args_num-1) 0, -1 in
  CCList.range_by start (max end_ 0) ~step
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
          (Subst.FO.bind' Subst.empty (hd_var, scope) (subs_term, scope),
           (depth+(diff_args_num-k)))))

let subset_elimination ~max_elims ~counter ~scope t u =
  elim_subsets_rule ~elim_vars ~max_elims ~counter ~scope t u 0
  |> OSeq.map (fun sub_flag -> 
      fst sub_flag, (snd sub_flag))

module Make (St : sig val st : Flex_state.t end) = struct
  module PUP = PragUnifParams 
  module SU = SolidUnif.Make(St)


  let get_option k = Flex_state.get_exn k St.st 

  let skip depth = 
    if depth > 1 then int_of_float @@ log10 (float_of_int depth) *. get_option PUP.k_skip_multiplier
    else 0 

  let delay depth res =
    OSeq.append
      (OSeq.take (skip depth) (OSeq.repeat None))
      res

  (*Create all possible projection and imitation bindings. *)
  let proj_imit_lr ?(disable_imit=false) ~counter ~scope ~subst s t flag =
    try
      let simp_proj, func_proj = 
        let is_ident_last = 
          let hd_var_id = HVar.id (T.as_var_exn (T.head_term s)) in
          IntSet.mem hd_var_id !ident_vars in
        if is_ident_last then [],[]
        else (
          proj_lr ~counter ~scope ~subst s t flag (get_option PUP.k_max_app_projections)
          |> CCList.partition_map (fun ((sub,_) as r) -> 
              let binding,_ = Subst.FO.deref sub (T.head_term s,scope) in
              let _,body = T.open_fun binding in
              if T.is_bvar body then `Left (Some r) else `Right (Some r))) in
      let imit_binding =
        try
          if not disable_imit && not (Term.is_var (T.head_term t)) &&
             get_op flag ImitRigid < get_option PUP.k_max_rigid_imitations then (
            let flag' = inc_op flag ImitRigid in
            [Some (U.subst @@ imitate_one ~scope ~counter s t, flag')])
          else []
        with Invalid_argument s when String.equal s "no_imits" -> [] in
      (* OSeq.of_list (simp_proj @ imit_binding @ func_proj) *)
      OSeq.append 
        (OSeq.of_list simp_proj)
        (delay (get_depth flag) @@ OSeq.append (OSeq.of_list imit_binding) (OSeq.of_list func_proj))
    with Invalid_argument s when String.equal s "as_var_exn" ->
      OSeq.empty

  let elim_rule ~counter ~scope t _ flag = 
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
        CCList.range 0 ((List.length args)-1) 
        |> List.map (eliminate_at_idx (T.as_var_exn hd)))
      else [] in
    eliminate_one t
    |> List.map (fun x -> Some (x, inc_op flag Elim))

  (* removes all arguments of an applied variable
     v |-> λ u1 ... um. x
  *)
  let elim_trivial ~scope ~counter v =  
    let prefix_types, return_type = Type.open_fun (HVar.ty v) in
    let matrix_head = T.var (H.fresh_cnt ~counter ~ty:return_type ()) in
    let subst_value = T.fun_l prefix_types matrix_head in
    let subst = Subst.FO.bind' Subst.empty (v, scope) (subst_value, scope) in
    subst

  let renamer ~counter t0s t1s = 
    let lhs,rhs, unifscope, us = U.FO.rename_to_new_scope ~counter t0s t1s in
    lhs,rhs,unifscope,U.subst us

  let deciders ~counter () =
    let pattern = 
      if get_option PUP.k_pattern_decider then 
        [(fun s t sub -> [(U.subst @@ PatternUnif.unify_scoped ~subst:(U.of_subst sub) ~counter s t)])] 
      else [] in
    let solid = 
      if get_option PUP.k_solid_decider then 
        [(fun s t sub -> (List.map U.subst @@ SU.unify_scoped ~subst:(U.of_subst sub) ~counter s t))] 
      else [] in
    let fixpoint = 
      if get_option PUP.k_fixpoint_decider then 
        [(fun s t sub -> [(U.subst @@ FixpointUnif.unify_scoped ~subst:(U.of_subst sub) ~counter s t)])] 
      else [] in
    fixpoint @ pattern @ solid

  let head_classifier s =
    match T.view @@ T.head_term s with 
    | T.Var x -> `Flex x
    | _ -> `Rigid

  let oracle ~counter ~scope ~subst (s,_) (t,_) (flag:I.t) =
    let depth = get_depth flag in
    let res = 
      if depth < get_option PUP.k_max_depth then (
        match head_classifier s, head_classifier t with
        | `Flex x, `Flex y when HVar.equal Type.equal x y ->
          let num_elims = get_op flag Elim in
          let remaining_elims = get_option PUP.k_max_elims - num_elims in
          if remaining_elims > 0 then (
            (subset_elimination ~max_elims:(Some remaining_elims) ~counter ~scope s t
             |> OSeq.map (fun (sub, inc) ->
                 let flag' = CCList.fold_left (fun acc _ -> inc_op acc Elim) 
                     flag (CCList.replicate inc None) in
                 Some (sub, flag'))))
          else OSeq.return (Some (elim_trivial ~counter ~scope x, flag))
        | `Flex _, `Flex _ ->
          (* all rules  *)
          let ident = 
            if get_op flag Ident < get_option PUP.k_max_identifications then (
              JP_unif.identify ~scope ~counter s t []
              |> OSeq.map (fun x -> 
                  let subst = U.subst x  in
                  (* variable introduced by identification *)
                  let subs_t = T.of_term_unsafe @@ fst (snd (List.hd (Subst.to_list subst))) in
                  let new_var, _ = T.as_app (snd (T.open_fun subs_t)) in
                  let new_var_id = HVar.id (T.as_var_exn new_var) in
                  (* remembering that we introduced this var in identification *)
                  ident_vars := IntSet.add new_var_id !ident_vars;
                  Some (subst, inc_op flag Ident)))
            else OSeq.empty in
          let projs =
            OSeq.append
              (proj_imit_lr ~disable_imit:true ~scope ~counter ~subst s t flag)
              (proj_imit_lr ~disable_imit:true ~scope ~counter ~subst t s flag) in
          delay depth @@ OSeq.append projs ident
        | `Flex _, `Rigid
        | `Rigid, `Flex _ ->
          OSeq.append
            (proj_imit_lr ~counter ~scope ~subst s t flag)
            (proj_imit_lr ~counter ~scope ~subst t s flag)
        | _ -> 
          CCFormat.printf "Did not disassemble properly: [%a]\n[%a]@." T.pp s T.pp t;
          assert false)
      else OSeq.empty in
    let hd_t, hd_s = T.head_term s, T.head_term t in
    if T.is_var hd_t && T.is_var hd_s && T.equal hd_s hd_t &&
       IntSet.mem (HVar.id @@ T.as_var_exn hd_t) !elim_vars then (
      OSeq.empty)
    else res

  let unify_scoped =  
    let counter = ref 0 in

    let module PragUnifParams = struct
      exception NotInFragment = PatternUnif.NotInFragment
      exception NotUnifiable = PatternUnif.NotUnifiable
      type flag_type = int32
      let init_flag = (Int32.zero:flag_type)
      let identify_scope = renamer ~counter
      let frag_algs = deciders ~counter (*[]*)
      let pb_oracle s t (f:flag_type) subst scope = 
        oracle ~counter ~scope ~subst s t f
      let oracle_composer = OSeq.append
    end in

    let module PragUnif = UnifFramework.Make(PragUnifParams) in
    (fun x y ->
       elim_vars := IntSet.empty;
       ident_vars := IntSet.empty;
       OSeq.map (CCOpt.map Unif_subst.of_subst) (PragUnif.unify_scoped x y))
end
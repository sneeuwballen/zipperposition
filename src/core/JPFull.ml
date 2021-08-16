module U = Unif_subst
module T = Term
module H = HVar
module S = Subst
module IntSet = PUnif.IntSet
module PUP = PragUnifParams

let elim_vars = ref IntSet.empty
let ident_vars = ref IntSet.empty

module Make (S : sig val st: Flex_state.t end) = struct
  module SU = SolidUnif.Make(S)

  let get_op k = Flex_state.get_exn k S.st

  let delay depth res = 
    OSeq.append (OSeq.take (2*depth) (OSeq.repeat None)) res
  
  let iter_rule ?(flex_same=false) ~counter ~scope t u depth  =
    JP_unif.iterate ~flex_same ~scope ~counter t u []
    |> OSeq.map (CCOpt.map (fun s -> U.subst s, depth+1))

  let imit_rule ~counter ~scope t u depth =
    JP_unif.imitate ~scope ~counter t u []
    |> OSeq.map (fun x -> Some (U.subst x, depth+1))

  let hs_proj_flex_rigid ~counter ~scope ~flex u depth =
    let flex_var = T.as_var_exn (T.head_term flex) in
    let flex_hd_id = HVar.id flex_var  in
    if IntSet.mem flex_hd_id !ident_vars then OSeq.empty
    else
      let projections = 
        PUnif.proj_hs ~counter ~scope ~flex u in
      let simp_projs, func_projs =
        CCList.partition (fun sub -> 
            let binding,_ = Subst.FO.deref sub (T.head_term flex,scope) in
            let _,body = T.open_fun binding in
            T.is_bvar body) projections in
      let simp_projs = CCList.map (fun s -> Some (s,depth)) simp_projs in
      let func_projs = CCList.map (fun s -> Some (s,depth+1)) func_projs in
      OSeq.append 
        (OSeq.of_list simp_projs)
        (if CCList.is_empty func_projs then OSeq.empty
         else (OSeq.of_list func_projs))

  let proj_rule ~counter ~scope s t depth =
    let maybe_project u =
      let flex_hd_id = HVar.id (T.as_var_exn (T.head_term u)) in
      if IntSet.mem flex_hd_id !ident_vars then OSeq.empty
      else JP_unif.project_onesided ~scope ~counter u in

    OSeq.append (maybe_project s) (maybe_project t)
    |> OSeq.map (fun x -> Some (U.subst x, depth+1))

  let ident_rule ~counter ~scope t u depth = 
    JP_unif.identify ~scope ~counter t u []
    |> OSeq.map (fun x -> 
        let subst = U.subst x  in
        (* variable introduced by identification *)
        let subs_t = T.of_term_unsafe (fst (snd (List.hd (Subst.to_list subst)))) in
        let new_var, _ = T.as_app (snd (T.open_fun subs_t)) in
        let new_var_id = HVar.id (T.as_var_exn new_var) in
        (* remembering that we introduced this var in identification *)
        ident_vars := IntSet.add new_var_id !ident_vars;
        Some (subst, depth+1))

  let renamer ~counter t0s t1s = 
    let lhs,rhs, unifscope, us = U.FO.rename_to_new_scope ~counter t0s t1s in
    lhs,rhs,unifscope,U.subst us
  
  let renamer_l ~counter t0s t1s = 
    let lhs,rhs, unifscope, us = U.FO.rename_l_to_new_scope ~counter t0s t1s in
    lhs,rhs,unifscope,U.subst us

  let deciders ~counter () =
    let pattern = 
      if get_op PUP.k_pattern_decider then [fun s t sub -> 
          [U.subst @@ PatternUnif.unify_scoped ~subst:(U.of_subst sub) ~counter s t]]
      else [] in
    let solid = 
      if get_op PUP.k_solid_decider then [fun s t sub -> 
          List.map U.subst @@ SU.unify_scoped ~subst:(U.of_subst sub) ~counter s t] 
      else [] in
    let fixpoint = 
      if get_op PUP.k_fixpoint_decider then [fun s t sub -> 
          [U.subst @@ FixpointUnif.unify_scoped ~subst:(U.of_subst sub) ~counter s t]]
      else [] in
    fixpoint @ pattern @ solid
  (* pattern @ fixpoint @ solid *)

  let head_classifier s =
    match T.view @@ T.head_term s with 
    | T.Var x -> `Flex x
    | _ -> `Rigid

  let oracle ~counter ~scope (s,_) (t,_) depth =
    let hd_t, hd_s = T.head_term s, T.head_term t in
    if T.is_var hd_t && T.is_var hd_s && T.equal hd_s hd_t &&
       IntSet.mem (HVar.id @@ T.as_var_exn hd_t) !elim_vars then (
      OSeq.empty)
    else (
      match head_classifier s, head_classifier t with 
      | `Flex x, `Flex y when HVar.equal Type.equal x y ->
        (* eliminate + iter *)
        OSeq.append
          (OSeq.map (fun x -> Some x) @@
           PUnif.elim_subsets_rule ~max_elims:None ~elim_vars ~counter ~scope s t depth)
          (delay depth @@ iter_rule ~flex_same:true ~counter ~scope s t depth)
      | `Flex _, `Flex _ ->
        (* all rules  *)
        let proj_ident =
          OSeq.append
            (proj_rule ~counter ~scope s t depth)
            (ident_rule ~counter ~scope s t depth) in
        OSeq.append proj_ident 
          (delay depth @@ iter_rule ~counter ~scope s t depth)
      | `Flex _, `Rigid
      | `Rigid, `Flex _ ->
        let flex, rigid = if Term.is_var (T.head_term s) then s,t else t,s in
        (* let delay_fr imit = 
          if depth > 4 then OSeq.append (OSeq.take (depth*2) (OSeq.repeat None)) imit else imit in *)
        OSeq.append
          (imit_rule ~counter ~scope s t depth)
          (hs_proj_flex_rigid ~counter ~scope ~flex rigid depth) 
      | _ -> 
        assert false)

  let unify_scoped =  
    let counter = ref 0 in

    let module JPFullParams = struct
      exception NotInFragment = PatternUnif.NotInFragment
      exception NotUnifiable = PatternUnif.NotUnifiable
      type flag_type = int
      let flex_state = S.st 
      let init_flag = (0:flag_type)
      let identify_scope = renamer ~counter
      let identify_scope_l = renamer_l ~counter
      let frag_algs = deciders ~counter
      let pb_oracle s t (f:flag_type) _ scope = 
        oracle ~counter ~scope s t f
    end in

    let module JPFull = UnifFramework.Make(JPFullParams) in
    (fun x y ->
       elim_vars := IntSet.empty;
       ident_vars := IntSet.empty;
       OSeq.map (CCOpt.map Unif_subst.of_subst) (JPFull.unify_scoped x y))
  
  let unify_scoped_l =  
    let counter = ref 0 in

    let module JPFullParams = struct
      exception NotInFragment = PatternUnif.NotInFragment
      exception NotUnifiable = PatternUnif.NotUnifiable
      type flag_type = int
      let flex_state = S.st 
      let init_flag = (0:flag_type)
      let identify_scope = renamer ~counter
      let identify_scope_l = renamer_l ~counter
      let frag_algs = deciders ~counter
      let pb_oracle s t (f:flag_type) _ scope = oracle ~counter ~scope s t f
    end in

    let module JPFull = UnifFramework.Make(JPFullParams) in
    (fun x y ->
       elim_vars := IntSet.empty;
       ident_vars := IntSet.empty;
       OSeq.map (CCOpt.map Unif_subst.of_subst) (JPFull.unify_scoped_l x y))
end

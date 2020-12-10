module TS = Term.Set
module T  = Term
module VT = T.VarTbl
module L = Literal
module Ls = Literals
module IntSet = Set.Make(CCInt)
module PUP = PragUnifParams

(* Meta subst *)
module MS = Term.VarMap

exception SolidMatchFail
exception UnsupportedLiteralKind

module Make (S : sig val st : Flex_state.t end) = struct
  module SU = SolidUnif.Make(S)
  let get_op k = Flex_state.get_exn k S.st

  (* Multiterm is built more or less like a normal term,
     except that at any point (at any constructor) there is an
     alternative way to see this term -- as a bound variable corresponding
     to some argument of a __solid pattern__.CCArray
     
     For example let s = F a (f b) be a solid pattern.
     For a term t = g (f a) (f b) multiterm
     {g} [{f} [{a,1}] {}; {f} [{b}] {0} ] {}
     represents exponentially many ways to match term t using pattern s
     (in this case 2*2=4) *)
  type multiterm =
    (* Application of Builtin constant to a list of multiterms.
       Third argument are possible replacements. *)
    | AppBuiltin of Builtin.t * multiterm list * TS.t
    (* Application of constants or bound variables to a list of multiterms.
       Head can be represented possibly in many ways!
       Third argument are possible replacements. *)
    | App of TS.t * multiterm list * TS.t
    (* Lambda abstraction of multiterms *)
    | Fun of Type.t * multiterm 
    (* Replacements that are either bound variables or constants *)
    | Repl of TS.t

  let bvar_or_const t =
    Term.is_const t || Term.is_bvar t

  let app_builtin b args repls =
    if TS.for_all bvar_or_const repls then (
      AppBuiltin(b,args,repls)
    ) else invalid_arg "replacements must be bound vars or constants"

  let app hd args repls =
    if TS.for_all bvar_or_const hd then (
      if TS.for_all bvar_or_const repls then App (hd,args,repls)
      else invalid_arg "replacements must be bound vars or constants"
    ) else invalid_arg "head of multiterm is bound var or constant"

  let fun_ ty body =
    Fun(ty,body)

  let fun_l ty_args body =
    List.fold_right fun_ ty_args body

  let open_builtin = function
    | AppBuiltin(hd,args,repls) -> (hd,args,repls)
    | _ -> invalid_arg "cannot open builtin"

  let open_app = function
    | App(hds,args,repls) -> (hds,args,repls)
    | _ -> invalid_arg "cannot open app"

  let open_fun = function
    | Fun(ty,bodys) -> (ty,bodys)
    | _ -> invalid_arg "cannot open fun"

  let open_repl = function
    | Repl repls -> repls
    | _ -> invalid_arg "cannot open repl"

  let repl repls = 
    if TS.for_all bvar_or_const repls then (
      Repl repls
    ) else (
      let err_msg = CCFormat.sprintf "replacements must be ground: @[%a@]" (TS.pp T.pp) repls in
      invalid_arg err_msg
    )

  let rec of_term term =
    match T.view term with
    | AppBuiltin(b, args) ->
      app_builtin b (List.map of_term args) TS.empty
    | App(hd, args) ->
      app (TS.singleton hd) (List.map of_term args) TS.empty
    | Fun(ty,body) ->
      fun_ ty (of_term body)
    | _ -> repl (TS.singleton term)

  let rec pp out =
    let sepc = CCFormat.return ",@," in
    let sepw = CCFormat.return "@ " in
    function 
    | AppBuiltin(b,args,repls) ->
      CCFormat.fprintf out "|@[%a@](@[%a@])|@[%a@]|" Builtin.pp b (Util.pp_list ~sep:"," pp) args (TS.pp ~pp_sep:sepw T.pp) repls;
    | App(hds,args,repls) ->
      CCFormat.fprintf out "|{@[%a@]}(@[%a@])|@[%a@]|" (TS.pp ~pp_sep:sepc T.pp) hds (CCList.pp ~pp_sep:sepc pp) args (TS.pp ~pp_sep:sepw T.pp) repls;
    | Fun(ty,repls) ->
      CCFormat.fprintf out "|l@[%a@].@[%a@]|" Type.pp ty pp repls;
    | Repl repls ->
      CCFormat.fprintf out "{r:@[%a@]}" (TS.pp ~pp_sep:sepw T.pp) repls

  let cover t solids : multiterm = 
    let n = List.length solids in

    let rec aux ~depth s_args t : multiterm  =
      (* All the ways in which we can represent term t using solids *)
      let sols_as_db = List.mapi (fun i t -> 
          (t,T.bvar ~ty:(T.ty t) (n-i-1+depth))) s_args in
      let matches_of_solids target = 
        (CCList.filter_map (fun (s, s_db) -> 
            if T.equal s target then Some s_db else None) 
          sols_as_db)
        |> TS.of_list in
      let db_hits = matches_of_solids t in

      match T.view t with
      | AppBuiltin (hd,args) ->
        app_builtin hd (List.map (aux ~depth s_args) args) db_hits
      | App(hd,args) ->
        assert(not (CCList.is_empty args));
        assert(bvar_or_const hd);
        let hds = TS.add hd @@ matches_of_solids hd in
        let args = List.map (aux ~depth s_args) args in
        app hds args db_hits
      | Fun _ -> 
        let ty_args, body = T.open_fun t in
        let d_inc = List.length ty_args in
        let s_args' = List.map (T.DB.shift d_inc) s_args in
        let res = aux ~depth:(depth+d_inc) s_args' body in
        fun_l ty_args res
      | DB i when i >= depth ->
        if TS.is_empty db_hits then raise SolidMatchFail
        else repl db_hits
      | _ -> repl (TS.add t db_hits) in
    aux ~depth:0 solids t

  let term_intersection s t =
    let rec aux s t =  
      match s with 
      | AppBuiltin(s_b,s_args,s_repls) ->
        let (t_b,t_args,t_repls) = open_builtin t in
        if s_b = t_b then (
          let args = List.map (fun (s,t) -> aux s t) @@ 
            List.combine s_args t_args in
          app_builtin s_b args (TS.inter s_repls t_repls)
        ) else raise SolidMatchFail
      | App(s_hds,s_args,s_repls) ->
        let (t_hds,t_args,t_repls) = open_app t in
        let i_hds = TS.inter s_hds t_hds in
        if not @@ TS.is_empty i_hds then (
          let args = List.map (fun (s,t) -> aux s t) @@ 
            List.combine s_args t_args in
          app i_hds args (TS.inter s_repls t_repls)
        ) else raise SolidMatchFail
      | Fun(s_ty,s_bodys) ->
        let t_ty,t_bodys = open_fun t in
        if Type.equal s_ty t_ty then (
          fun_ s_ty (aux s_bodys t_bodys)
        ) else raise SolidMatchFail
      | Repl(repls) ->
        let res = TS.inter repls (open_repl t) in
        if TS.is_empty res then raise SolidMatchFail
        else repl res
    in
    try 
      aux s t
    with Invalid_argument s ->
      Util.debugf 3 "Incompatible constructors: %s" (fun k -> k s);
      raise SolidMatchFail


  let refine_subst_w_term subst var t = 
    if not @@ MS.mem var subst then (
      MS.add var t subst
    ) else (
      let old = CCOpt.get_exn @@ MS.get var subst in
      MS.add var (term_intersection old t) subst 
    )

  let refine_subst_w_subst metasubst subst =
    let res = ref metasubst in
    Subst.FO.iter (fun (v,_) (t,_) ->  
        res := refine_subst_w_term !res v (of_term t);
      ) subst;
    !res

  let solid_match ~subst ~pattern ~target =
    assert(T.is_ground target);

    let rec aux subst l r =
      match T.view l with
      | AppBuiltin(b, args) ->
        begin match T.view r with 
          | AppBuiltin(b', args') 
            when Builtin.equal b b' && List.length args = List.length args' ->
            let args, args' = Unif.norm_logical_disagreements b args args' in
            List.fold_left 
              (fun subst (l',r') ->  aux subst l' r') 
              subst (List.combine args args')
          | _ -> raise SolidMatchFail end
      | App(hd, args) when T.is_var hd -> 
        refine_subst_w_term subst (T.as_var_exn hd) (cover r args)
      | App(hd, args) -> 
        assert(T.is_const hd || T.is_bvar hd);
        begin match T.view r with 
          | App(hd', args') when T.equal hd hd' ->
            assert(List.length args = List.length args');
            List.fold_left 
              (fun subst (l',r') ->  aux subst l' r') 
              subst (List.combine args args')
          | _ -> raise SolidMatchFail end
      | Fun _ ->
        let prefix, body = T.open_fun l in
        let prefix', body' = T.open_fun r in
        assert(List.length prefix = List.length prefix');
        aux subst body body'
      | Var x -> refine_subst_w_term subst x (cover r [])
      | _ -> if T.equal l r then subst else raise SolidMatchFail 
    in

    if Type.equal (T.ty pattern) (T.ty target) &&
       (*if terms are first-order we should not deal with them
         since LFHO would have already done it.  *)
       not (T.is_fo_term pattern) &&
       not (T.is_fo_term target) then aux subst pattern target
    else raise SolidMatchFail

  let normaize_clauses subsumer target =
    try 
      let eta_exp_snf ?(f=CCFun.id) =
        Ls.map (fun t -> f @@ Lambda.eta_expand @@ Lambda.snf @@ t) in

      let target' = 
        Ls.ground_lits @@ eta_exp_snf target in

      let subsumer' = eta_exp_snf ~f:(SU.solidify ~limit:false) subsumer in
      (* We populate app_var_map to contain indices of all arguments that
        should be removed *)
      subsumer', target'
    with PatternUnif.NotInFragment -> raise UnsupportedLiteralKind

  let sign l = 
    let res = 
      match l with 
      | L.Equation (_, _, sign) -> sign
      | L.False -> false
      | _ -> true 
    in
    if res then 1 else -1

  let cmp_by_sign l1 l2 =
    CCOrd.int (sign l1) (sign l2)

  let cmp_by_weight l1 l2 = 
    CCOrd.int (L.ho_weight l1) (L.ho_weight l2)

  let subsumption_cmp l1 l2 =
    let sign_res = cmp_by_sign l1 l2 in
    if sign_res != 0 then sign_res
    else if get_op PUP.k_use_weight_for_solid_subsumption
    then cmp_by_weight l1 l2 else 0

  let classic_match ~subst ~pattern ~target =
    try
      Unif.FO.matching_same_scope ~subst ~pattern ~scope:0 target
    with Unif.Fail -> raise SolidMatchFail

  let lit_matchers ~subst ~pattern ~target k  =
    begin match pattern with
      | L.Equation(lhs,rhs,sign) ->
        begin match target with
          | L.Equation(lhs', rhs',sign') ->
            assert(T.is_ground lhs');
            assert(T.is_ground rhs');
            (* let res_list = ref [] in  *)
            if sign=sign' then 
              (
                (try
                   let c_subst = classic_match ~subst:Subst.empty ~pattern:lhs ~target:lhs' in
                   let c_subst = classic_match ~subst:c_subst ~pattern:rhs ~target:rhs' in
                   k (refine_subst_w_subst subst c_subst)
                 with SolidMatchFail -> ());
                (try
                   let c_subst = classic_match ~subst:Subst.empty ~pattern:lhs ~target:rhs' in
                   let c_subst = classic_match ~subst:c_subst ~pattern:rhs ~target:lhs' in
                   k (refine_subst_w_subst subst c_subst)
                 with SolidMatchFail -> ());
                (try
                   let subst1 = (solid_match ~subst ~pattern:lhs ~target:lhs') in
                   k (solid_match ~subst:subst1 ~pattern:rhs ~target:rhs')
                 with SolidMatchFail -> ());
                (try 
                   let subst2 = (solid_match ~subst ~pattern:lhs ~target:rhs') in
                   k (solid_match ~subst:subst2 ~pattern:rhs ~target:lhs')
                 with SolidMatchFail -> ());
              );
          | _ -> () end
      | L.True -> begin match target with | L.True -> k subst | _ -> () end
      | L.False -> begin match target with | L.False -> k subst | _ -> () end
    end

  let check_subsumption_possibility subsumer target =
    let is_more_specific pattern target =
      not @@ Iter.is_empty (lit_matchers ~subst:MS.empty ~pattern ~target) in

    let neg_s, neg_t = CCPair.map_same (CCArray.fold (fun acc l -> if sign l = (-1) then acc + 1 else acc) 0) (subsumer, target) in 
    let pos_s, pos_t = CCPair.map_same (CCArray.fold (fun acc l -> if sign l = 1 then acc + 1 else acc) 0) (subsumer, target) in
    (not @@ get_op PUP.k_use_weight_for_solid_subsumption ||
     Ls.weight subsumer <= Ls.weight target) &&
    neg_s <= neg_t && pos_s <= pos_t && 
    (not (neg_t >=3 || pos_t >= 3) ||
     CCArray.for_all (fun l -> CCArray.exists (is_more_specific l) target) subsumer)

  let subsumes subsumer target =
    let n = Array.length subsumer in
    (* let subsumer_o, target_o = subsumer, target in *)

    let rec aux ?(i=0) picklist subst subsumer target_i =
      if i >= n then true
      else (
        let lit = subsumer.(i) in
        CCArray.exists (fun (j,lit') -> 
            if CCBV.get picklist j || cmp_by_sign lit lit' != 0 ||
               (get_op PUP.k_use_weight_for_solid_subsumption && cmp_by_weight lit lit' > 0) then false 
            else (
              let matchers = lit_matchers ~subst ~pattern:lit ~target:lit' in
              Iter.exists (fun subst' ->
                  CCBV.set picklist j;
                  let res = aux ~i:(i+1) picklist subst' subsumer target_i in
                  CCBV.reset picklist j;
                  res) matchers
            )) target_i
      ) in

    let subsumer,target = normaize_clauses subsumer target in

    CCArray.sort subsumption_cmp subsumer;
    CCArray.sort subsumption_cmp target;

    if check_subsumption_possibility subsumer target then (
      let picklist = CCBV.create ~size:(Array.length target) false in
      let target_i = CCArray.mapi (fun i l -> (i,l)) target in
      let res = aux picklist MS.empty subsumer target_i in
      res
    ) else false
end

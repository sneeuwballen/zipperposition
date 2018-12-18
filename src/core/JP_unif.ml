
(* This file is free software, part of Zipperposition. See file "license" for more details. *)

(** {1 Jensen-Pietrzykowski Unification} *)

module T = Term
module US = Unif_subst


type subst = US.t

module S = struct

  let apply s t = Subst.FO.apply Subst.Renaming.none (US.subst s) t

  let apply_ty s ty = Subst.Ty.apply Subst.Renaming.none (US.subst s) ty

end

let make_fresh_var fresh_var_ ~ty () = 
  let var = HVar.make ~ty !fresh_var_ in 
  incr fresh_var_; 
  var

let unif_ty ~scope t s = 
  try 
    let type_unifier = Unif.FO.unify_syn ~subst:Subst.empty (t, scope) (s, scope) in
    Some (US.of_subst type_unifier)
  with Unif.Fail -> None

(** {1 Projection rule} *)

(* find substitutions for the projection rule, given a member of the disagreement pair *)
let project_onesided ~scope ~fresh_var_:_ u =
  let head, args = T.as_app u in
  let prefix_types, return_type = Type.open_fun (T.ty head) in
  if T.is_var head 
  then
    args 
    |> OSeq.of_list
    |> OSeq.mapi (fun i arg -> List.length prefix_types - 1 - i, arg) (* Determine DB-index of the argument *)
    |> OSeq.filter_map (fun (dbindex, arg) -> 
      (* Unify type of argument and return type *)
      match unif_ty ~scope (Term.of_ty (T.ty arg)) (Term.of_ty return_type) with
        | Some type_unifier -> Some (dbindex, arg, type_unifier)
        | None -> None
    )
    |> OSeq.map (fun (dbindex, arg, type_unifier) -> 
        (* substitute x for a projection to the j-th argument *)
        US.FO.bind type_unifier (T.as_var_exn head, scope) (T.fun_l prefix_types (T.bvar ~ty:(T.ty arg) dbindex), scope)
    )
  else OSeq.empty

(* find substitutions for the projection rule, given a disagreement pair *)
let project ~scope ~fresh_var_ u v (_ : (T.var * int) list) = OSeq.append (project_onesided ~scope ~fresh_var_ u) (project_onesided ~scope ~fresh_var_ v)


(** {2 Imitation rule} *)

let imitate_onesided ~scope ~fresh_var_ u v = 
  let head_u = T.head_term_mono u in
  let head_v = T.head_term_with_mandatory_args v in
  let prefix_types_u, _ = Type.open_fun (T.ty head_u) in
  let prefix_types_v, _ = Type.open_fun (T.ty head_v) in
  if T.is_var head_u                                        (* u has a varaible head *)
    && not (T.is_bvar head_v)                               (* the head of v is not a bound variable *)
    && not (T.var_occurs ~var:(T.as_var_exn head_u) head_v) (* the head of u does not occur in the mandatory args of v *)
  then
    (* create substitution: head_u |-> λ u1 ... um. head_v (x1 u1 ... um) ... (xn u1 ... um)) *)
    let bvars = prefix_types_u |> List.rev |> List.mapi (fun i ty -> T.bvar ~ty i) |> List.rev in
    let matrix_args = 
      prefix_types_v 
      |> List.map (fun prefix_type_v ->
        let ty = Type.arrow prefix_types_u prefix_type_v in
        let var = T.var (make_fresh_var fresh_var_ ~ty ()) in
        T.app var bvars) 
    in
    let matrix = T.app head_v matrix_args in
    let subst_value = T.fun_l prefix_types_u matrix in 
    let subst = US.FO.singleton (T.as_var_exn head_u, scope) (subst_value, scope) in
    OSeq.return subst
  else OSeq.empty

(* find substitutions for the projection rule, given a disagreement pair *)
let imitate ~scope ~fresh_var_ u v (_ : (T.var * int) list) = 
  OSeq.append (imitate_onesided ~scope ~fresh_var_ u v) (imitate_onesided ~scope ~fresh_var_ v u)


(** {3 Identification rule} *)

let identify ~scope ~fresh_var_ u v (_ : (T.var * int) list) =
  let head_u = T.head_term_mono u in
  let head_v = T.head_term_mono v in
  let prefix_types_u, return_type = Type.open_fun (T.ty head_u) in
  let prefix_types_v, return_type2 = Type.open_fun (T.ty head_v) in
  assert (Type.equal return_type return_type2);
  if T.is_var head_u && T.is_var head_v (* TODO: necessary when args_u or args_v is empty? *)
  then
    (* create substitution: head_u |-> λ u1 ... um. x u1 ... um (y1 u1 ... um) ... (yn u1 ... um) 
                            head_v |-> λ v1 ... vn. x (z1 v1 ... vn) ... (zm v1 ... vn) v1 ... vn *)
    let bvars_u = prefix_types_u |> List.rev |> List.mapi (fun i ty -> T.bvar ~ty i) |> List.rev in
    let bvars_v = prefix_types_v |> List.rev |> List.mapi (fun i ty -> T.bvar ~ty i) |> List.rev in
    let matrix_args_u = 
      prefix_types_v 
      |> List.map (fun prefix_type_v ->
        let ty = Type.arrow prefix_types_u prefix_type_v in
        let var = T.var (make_fresh_var fresh_var_ ~ty ()) in
        T.app var bvars_u) 
    in
    let matrix_args_v = 
      prefix_types_u
      |> List.map (fun prefix_type_u ->
        let ty = Type.arrow prefix_types_v prefix_type_u in
        let var = T.var (make_fresh_var fresh_var_ ~ty ()) in
        T.app var bvars_v) 
    in
    let matrix_head = T.var (make_fresh_var fresh_var_ ~ty:(Type.arrow (prefix_types_u @ prefix_types_v) return_type) ()) in
    let matrix_u = T.app matrix_head (bvars_u @ matrix_args_u) in
    let matrix_v = T.app matrix_head (matrix_args_v @ bvars_v) in
    let subst_value_u = T.fun_l prefix_types_u matrix_u in 
    let subst_value_v = T.fun_l prefix_types_v matrix_v in 
    let subst = US.FO.singleton (T.as_var_exn head_u, scope) (subst_value_u, scope) in
    let subst = US.FO.bind subst (T.as_var_exn head_v, scope) (subst_value_v, scope) in
    OSeq.return subst
  else OSeq.empty


(** {4 Elimination rule} *)

let eliminate ~scope ~fresh_var_ _ _ l =
  l |> List.map (fun (v, k) -> 
    (* create substitution: v |-> λ u1 ... um. x u1 ... u{k-1} u{k+1} ... um *)
    let prefix_types, return_type = Type.open_fun (HVar.ty v) in
    let bvars = prefix_types |> List.rev |> List.mapi (fun i ty -> T.bvar ~ty i) |> List.rev in
    let prefix_types' = CCList.remove_at_idx k prefix_types in
    let bvars' = CCList.remove_at_idx k bvars in
    let matrix_head = T.var (make_fresh_var fresh_var_ ~ty:(Type.arrow prefix_types' return_type) ()) in
    let matrix = T.app matrix_head bvars' in
    let subst_value = T.fun_l prefix_types matrix in
    let subst = US.FO.singleton (v, scope) (subst_value, scope) in
    subst
  )
  |> OSeq.of_list
(* TODO: use OSeq directly? *)


(** {5 Iteration rule} *)

let iterate_one ~fresh_var_ types_w prefix_types return_type i type_ul =
  let prefix_types_ul, return_type_ul = Type.open_fun type_ul in
  (* create substitution: v |-> λ u1 ... um. x u1 ... um (λ w. ui (y1 (u1...um w)) ... (yn (u1...um w))) *)
  let inner_lambda_expr = 
    (* create term: (λ w. ui (y1 (u1...um w)) ... (yn (u1...um w)) *)
    let bvars_u_under_w = prefix_types |> List.rev |> List.mapi (fun i ty -> T.bvar ~ty (i + List.length types_w)) |> List.rev in
    let bvars_w = types_w |> List.rev |> List.mapi (fun i ty -> T.bvar ~ty i) |> List.rev in
    let bvar_ul_under_w = T.bvar ~ty:type_ul (List.length prefix_types - 1 - i + List.length types_w) in
    let vars_y = prefix_types_ul |> List.map (fun ty -> T.var (make_fresh_var fresh_var_ ~ty:(Type.arrow (prefix_types @ types_w) ty) ())) in
    let matrix = T.app bvar_ul_under_w (vars_y |> List.map (fun y -> T.app y (bvars_u_under_w @ bvars_w))) in
    T.fun_l types_w matrix
  in
  let bvars_u = prefix_types |> List.rev |> List.mapi (fun i ty -> T.bvar ~ty i) |> List.rev in
  let var_x = T.var (make_fresh_var fresh_var_ ~ty:(Type.arrow (prefix_types @ [Type.arrow types_w return_type_ul]) return_type) ()) in
  let matrix = T.app var_x (bvars_u @ [inner_lambda_expr]) in
  let subst_value = T.fun_l prefix_types matrix in
  subst_value


let iterate ~scope ~fresh_var_ u v l =
  (* The variable can be either above the disagreement pair (i.e., in l) 
     or it can be the head of either member of the disagreement pair *)
  let positions =
    l 
    |> CCList.map fst
    |> CCList.cons_maybe (T.as_var (T.head_term u))
    |> CCList.cons_maybe (T.as_var (T.head_term v))
    |> OSeq.of_list
    |> OSeq.flat_map
      (fun v ->
        let prefix_types, return_type = Type.open_fun (HVar.ty v) in
        prefix_types 
        |> List.mapi
          (fun i type_ul ->
            if Type.is_fun type_ul || Type.is_var type_ul
            then 
              Some (v, prefix_types, return_type, i, type_ul)
            else 
              None
          ) 
        |> CCList.filter_map (fun x -> x)
        |> OSeq.of_list
      )
  in
  (* The tuple `w` can be of any length. Hence we use the sequence [[alpha], [alpha, beta], [alpha, beta, gamma], ...] *)
  let types_w_seq = OSeq.iterate [] (fun types_w -> Type.var (make_fresh_var fresh_var_ ~ty:Type.tType ()) :: types_w) in
  if OSeq.is_empty positions 
  then OSeq.empty
  else 
    types_w_seq
    |> OSeq.flat_map 
      (fun types_w -> 
        positions
        |> OSeq.map
          (fun (v, prefix_types, return_type, i, type_ul) -> 
            if Type.is_fun type_ul 
            then Some (US.FO.singleton (v, scope) (iterate_one ~fresh_var_ types_w prefix_types return_type i type_ul, scope))
            else 
              (* To get a complete polymorphic algorithm, we need to consider the case that a type variable could be instantiated as a function. *)
              match Type.view type_ul with
                | Type.Var alpha -> 
                  let beta = (make_fresh_var fresh_var_ ~ty:Type.tType ()) in
                  let gamma = (make_fresh_var fresh_var_ ~ty:Type.tType ()) in
                  let alpha' = (Type.arrow [Type.var beta] (Type.var gamma)) in
                  let ty_subst = US.FO.singleton (alpha, scope) (Term.of_ty alpha', scope) in
                  let v' = HVar.cast ~ty:(S.apply_ty ty_subst (HVar.ty v, scope)) v in
                  let prefix_types' = prefix_types |> CCList.map (fun ty -> S.apply_ty ty_subst (ty, scope)) in
                  let return_type' = S.apply_ty ty_subst (return_type, scope) in
                  Some (US.FO.bind ty_subst (v', scope) (iterate_one ~fresh_var_ types_w prefix_types' return_type' i alpha', scope))
                | _ -> None
          )
        (* Append some "None"s to delay the substitutions containing long w tuples *)
        |> (fun seq -> OSeq.append seq (OSeq.take 50 (OSeq.repeat None)))
      )
  
(* TODO: use OSeq directly? *)


(** {6 Unification procedure} *)

(* apply a substitution and reduce to normal form *)
let nfapply s u = Lambda.eta_expand (Lambda.snf (S.apply s u))

(* TODO: comparison form is actually slightly different, but eta_expand also works *)

let find_disagreement s t = 
  (* TODO: preferably one that is not below a variable (to get preunification if possible) *)
  let rec find_disagreement_l ?(applied_var = None) ?(argindex=0) ss tt = 
    match ss, tt with
      | [], [] -> OSeq.empty
      | s' :: ss', t' :: tt' -> 
        find_disagreement_aux s' t'
        |> OSeq.map (fun ((u,v),l) ->
            begin match applied_var with
              | Some x -> ((u,v), (T.as_var_exn x, argindex) :: l) 
              | None -> ((u,v),l)
            end
        )
        |> (fun seq ->
          if Term.is_type s' && not (OSeq.is_empty seq) then
          (* If type arguments need to be unified, do that first and ignore disagreements in the remaining arguments
            (because the number of remaining arguments may vary in this case) *)
            seq 
          else
            OSeq.append seq 
              (find_disagreement_l ~applied_var ~argindex:(argindex + 1) ss' tt')
        )
      | _, _ -> raise (Invalid_argument "types of unified terms should be equal")
  and find_disagreement_aux s t = 
    if T.is_type s then 
      if Term.equal s t then
        OSeq.empty
      else
        OSeq.return ((s,t),[])
    else (
      match T.view s, T.view t with
        | T.App (f, ss), T.App (g, tt) when T.equal f g && not (T.is_var f) -> 
          find_disagreement_l ss tt 
        | T.App (f, ss), T.App (g, tt) when T.equal f g && T.is_var f -> 
          find_disagreement_l ~applied_var:(Some f) ss tt 
        | T.AppBuiltin (f, ss), T.AppBuiltin (g, tt) when Builtin.equal f g -> 
          find_disagreement_l ss tt 
        | T.Var x, T.Var y when x = y -> OSeq.empty
        | T.DB i, T.DB j when i = j -> OSeq.empty
        | T.Const a, T.Const b when ID.equal a b -> OSeq.empty
        | T.Fun (ty_s, s'), T.Fun (ty_t, t') -> 
          assert (Type.equal ty_s ty_t); 
          find_disagreement_aux s' t'
        | _ -> OSeq.return ((s, t),[])
    )
  in
  let s = find_disagreement_aux s t in
  match OSeq.find (fun ((_,_),l) -> CCList.is_empty l) s with
    | Some d -> Some d
    | None -> 
      begin match s () with
        | OSeq.Nil -> None
        | OSeq.Cons (d, _) -> Some d
      end

let unify ~scope ~fresh_var_ t s = 
  let rec unify_terms ?(rules = []) t s  =
    Util.debugf 1 "@[Unify@ @[(rules: %a)@]@ @[%a@]@ and@ @[%a@]@]" (fun k -> k (CCList.pp CCString.pp) rules T.pp t T.pp s); 
    match find_disagreement t s with
      | Some ((u, v), l) -> 
        let subst_seq = 
          if T.is_type t then
            OSeq.return ((unif_ty ~scope s t), "unif_ty")
          else (
            let add_some f u v l = f ~scope ~fresh_var_ u v l |> OSeq.map (fun s -> Some s) in
            [add_some project,"proj"; add_some imitate,"imit"; add_some identify,"id"; add_some eliminate,"elim"; iterate ~scope ~fresh_var_,"iter"]
            (* iterate must be last in this list because it is the only one with infinitely many child nodes *)
            |> OSeq.of_list  
            |> OSeq.flat_map
              (fun (rule,rulename) -> 
                rule u v l 
                |> OSeq.map (fun subst -> (subst, rulename)))
          )
        in
        subst_seq 
        |> OSeq.map 
          (fun (osubst,rulename) -> 
            match osubst with
            | Some subst ->
              let t_subst = nfapply subst (t, scope) in
              let s_subst = nfapply subst (s, scope) in
              let unifiers = unify_terms t_subst s_subst ~rules:(rules @ [rulename]) in
              unifiers 
              |> OSeq.map (CCOpt.map (fun unifier -> US.merge subst unifier))
              (* We actually want to compose the substitutions here, but merge will have the same effect. *)
            | None -> OSeq.empty
          )
        |> OSeq.merge 
        |> OSeq.append (OSeq.return None)
      | None -> 
        assert (t == s);
        (* Util.debugf 1 "@[...unified!@ @[(rules: %a)@]@]" (fun k -> k (CCList.pp CCString.pp) rules); *)
        OSeq.return (Some US.empty)
  in
  (* Unify types first ... *)
  match unif_ty ~scope (T.of_ty (T.ty t)) (T.of_ty (T.ty s)) with
    | Some type_unifier ->
      let t' = nfapply type_unifier (t, scope) in
      let s' = nfapply type_unifier (s, scope) in
      (* ... then terms. *)
      let term_unifiers = unify_terms t' s' ~rules:[] in
      OSeq.map (CCOpt.map (US.merge type_unifier)) term_unifiers 
    | None -> OSeq.empty

(* TODO: Remove tracking of rules for efficiency? *)

let unify_scoped (t0, scope0) (t1, scope1) =
  (* Find a scope that's different from the two given ones *)
  let unifscope = if scope0 < scope1 then scope1 + 1 else scope0 + 1 in
  let fresh_var_ = ref 0 in
  let add_renaming scope subst v =
    if HVar.id v = -134 then Util.debugf 5 "%a XXX %a" (fun k -> k T.pp t1 (CCList.pp (CCPair.pp HVar.pp Type.pp)) (List.map (fun e -> e, HVar.ty e) (Sequence.to_list (T.Seq.vars t1))));
    if US.FO.mem subst (v,scope) 
    then subst
    else 
      let newvar = T.var (make_fresh_var fresh_var_ ~ty:(S.apply_ty subst (HVar.ty v, scope)) ()) in
      US.FO.bind subst (v,scope) (newvar, unifscope) 
  in
  let subst = US.empty in
  (* Rename variables apart into scope `unifscope` *)
  let subst = T.Seq.vars t0 |> Sequence.fold (add_renaming scope0) subst in
  let subst = T.Seq.vars t1 |> Sequence.fold (add_renaming scope1) subst in
  (* Unify *)
  unify ~scope:unifscope ~fresh_var_ (S.apply subst (t0, scope0)) (S.apply subst (t1, scope1))
  (* merge with var renaming *)
  |> OSeq.map (CCOpt.map (US.merge subst))

let unify_scoped_nonterminating t s = OSeq.filter_map (fun x -> x) (unify_scoped t s)

(* TODO: operate on inner types like in `Unif`? Test for NO-TYPE terms? *)
(* TODO: `dont care` unification, i.e. stopping at flex-flex pairs because exact result does not matter? *)
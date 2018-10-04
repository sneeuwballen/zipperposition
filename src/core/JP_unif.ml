
(* This file is free software, part of Zipperposition. See file "license" for more details. *)

(** {1 Jensen-Pietrzykowski Unification} *)

module T = Term

let scope = 0 (* TODO: scopes *)

type subst = Subst.t Sequence.t

module S = struct

  let empty = Sequence.empty
  
  let add s (v:T.var) t = Sequence.snoc s ((Subst.FO.bind Subst.empty ((v,scope):>InnerTerm.t HVar.t Scoped.t)) (t,scope)) 

  let append = Sequence.append

  let singleton (v:T.var) t = add empty v t

  let of_subst s = Sequence.singleton s

  let apply s t = s |> Sequence.fold (fun term subst -> Subst.FO.apply Subst.Renaming.none subst (term, scope)) t

  let apply_ty s ty = s |> Sequence.fold (fun term subst -> Subst.Ty.apply Subst.Renaming.none subst (ty, scope)) ty

end

let unif_ty t s = 
  try 
    let type_unifier = S.of_subst (Unif.Ty.unify_syn ~subst:Subst.empty (t, scope) (s, scope)) in
    Some type_unifier
  with Unif.Fail -> None

(** {1 Projection rule} *)

(* find substitutions for the projection rule, given a member of the disagreement pair *)
let project_onesided u =
  let head, args = T.as_app u in
  let prefix_types, return_type = Type.open_fun (T.ty head) in
  if T.is_var head 
  then
    args 
    |> OSeq.of_list
    |> OSeq.mapi (fun i arg -> List.length prefix_types - 1 - i, arg) (* Determine DB-index of the argument *)
    |> OSeq.filter_map (fun (dbindex, arg) -> 
      (* Unify type of argument and return type *)
      match unif_ty (T.ty arg) (return_type) with
        | Some type_unifier -> Some (dbindex, arg, type_unifier)
        | None -> None
    )
    |> OSeq.map (fun (dbindex, arg, type_unifier) -> 
        (* substitute x for a projection to the j-th argument *)
        S.add type_unifier (T.as_var_exn head) (T.fun_l prefix_types (T.bvar ~ty:(T.ty arg) dbindex))
    )
  else OSeq.empty

(* find substitutions for the projection rule, given a disagreement pair *)
let project u v (_ : (T.var * int) list) = OSeq.append (project_onesided u) (project_onesided v)


(** {2 Imitation rule} *)

let imitate_onesided u v = 
  let head_u = T.head_term_mono u in
  let head_v = T.head_term_mono v in
  let prefix_types_u, _ = Type.open_fun (T.ty head_u) in
  let prefix_types_v, _ = Type.open_fun (T.ty head_v) in
  if T.is_var head_u && not (T.is_bvar head_v) 
  then
    (* create substitution: head_u |-> λ u1 ... um. head_v (x1 u1 ... um) ... (xn u1 ... um)) *)
    let bvars = prefix_types_u |> List.rev |> List.mapi (fun i ty -> T.bvar ~ty i) |> List.rev in
    let matrix_args = 
      prefix_types_v 
      |> List.map (fun prefix_type_v ->
        let ty = Type.arrow prefix_types_u prefix_type_v in
        let var = T.var (HVar.fresh ~ty ()) in
        T.app var bvars) 
    in
    let matrix = T.app head_v matrix_args in
    let subst_value = T.fun_l prefix_types_u matrix in 
    let subst = S.singleton (T.as_var_exn head_u) subst_value in
    OSeq.return subst
  else OSeq.empty

(* find substitutions for the projection rule, given a disagreement pair *)
let imitate u v (_ : (T.var * int) list) = OSeq.append (imitate_onesided u v) (imitate_onesided v u)


(** {3 Identification rule} *)

let identify u v (_ : (T.var * int) list) =
  let head_u = T.head_term_mono u in
  let head_v = T.head_term_mono v in
  let prefix_types_u, return_type = Type.open_fun (T.ty head_u) in
  let prefix_types_v, return_type2 = Type.open_fun (T.ty head_v) in
  assert (return_type = return_type2);
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
        let var = T.var (HVar.fresh ~ty ()) in
        T.app var bvars_u) 
    in
    let matrix_args_v = 
      prefix_types_u
      |> List.map (fun prefix_type_u ->
        let ty = Type.arrow prefix_types_v prefix_type_u in
        let var = T.var (HVar.fresh ~ty ()) in
        T.app var bvars_v) 
    in
    let matrix_head = T.var (HVar.fresh ~ty:(Type.arrow (prefix_types_u @ prefix_types_v) return_type) ()) in
    let matrix_u = T.app matrix_head (bvars_u @ matrix_args_u) in
    let matrix_v = T.app matrix_head (matrix_args_v @ bvars_v) in
    let subst_value_u = T.fun_l prefix_types_u matrix_u in 
    let subst_value_v = T.fun_l prefix_types_v matrix_v in 
    let subst = S.singleton (T.as_var_exn head_u) subst_value_u in
    let subst = S.add subst (T.as_var_exn head_v) subst_value_v in
    OSeq.return subst
  else OSeq.empty


(** {4 Elimination rule} *)

let eliminate _ _ l =
  l |> List.map (fun (v, k) -> 
    (* create substitution: v |-> λ u1 ... um. x u1 ... u{k-1} u{k+1} ... um *)
    let prefix_types, return_type = Type.open_fun (HVar.ty v) in
    let bvars = prefix_types |> List.rev |> List.mapi (fun i ty -> T.bvar ~ty i) |> List.rev in
    let prefix_types' = CCList.remove_at_idx k prefix_types in
    let bvars' = CCList.remove_at_idx k bvars in
    let matrix_head = T.var (HVar.fresh ~ty:(Type.arrow prefix_types' return_type) ()) in
    let matrix = T.app matrix_head bvars' in
    let subst_value = T.fun_l prefix_types matrix in
    let subst = S.singleton v subst_value in
    subst
  )
  |> OSeq.of_list
(* TODO: use OSeq directly? *)



(** {5 Iteration rule} *)

let iterate_one types_w v prefix_types return_type i type_ul =
  let prefix_types_ul, return_type_ul = Type.open_fun type_ul in
  (* create substitution: v |-> λ u1 ... um. x u1 ... um (λ w. ui (y1 (u1...um w)) ... (yn (u1...um w))) *)
  let inner_lambda_expr = 
    (* create term: (λ w. ui (y1 (u1...um w)) ... (yn (u1...um w)) *)
    let bvars_u_under_w = prefix_types |> List.rev |> List.mapi (fun i ty -> T.bvar ~ty (i + List.length types_w)) |> List.rev in
    let bvars_w = types_w |> List.rev |> List.mapi (fun i ty -> T.bvar ~ty i) |> List.rev in
    let bvar_ul_under_w = T.bvar ~ty:type_ul (List.length prefix_types - 1 - i + List.length types_w) in
    let vars_y = prefix_types_ul |> List.map (fun ty -> T.var (HVar.fresh ~ty:(Type.arrow (prefix_types @ types_w) ty) ())) in
    let matrix = T.app bvar_ul_under_w (vars_y |> List.map (fun y -> T.app y (bvars_u_under_w @ bvars_w))) in
    T.fun_l types_w matrix
  in
  let bvars_u = prefix_types |> List.rev |> List.mapi (fun i ty -> T.bvar ~ty i) |> List.rev in
  let var_x = T.var (HVar.fresh ~ty:(Type.arrow (prefix_types @ [Type.arrow types_w return_type_ul]) return_type) ()) in
  let matrix = T.app var_x (bvars_u @ [inner_lambda_expr]) in
  let subst_value = T.fun_l prefix_types matrix in
  subst_value


let iterate u v l =
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
  let types_w_seq = OSeq.iterate [] (fun types_w -> Type.var (HVar.fresh ~ty:Type.tType ()) :: types_w) in
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
            then Some (S.singleton v (iterate_one types_w v prefix_types return_type i type_ul))
            else 
              (* To get a complete polymorphic algorithm, we need to consider the case that a type variable could be instantiated as a function. *)
              match Type.view type_ul with
                | Type.Var alpha -> 
                  let beta = (HVar.fresh ~ty:Type.tType ()) in
                  let gamma = (HVar.fresh ~ty:Type.tType ()) in
                  let alpha' = (Type.arrow [Type.var beta] (Type.var gamma)) in
                  let ty_subst = S.singleton alpha (Term.of_ty alpha') in
                  let v' = HVar.cast ~ty:(S.apply_ty ty_subst (HVar.ty v)) v in
                  let prefix_types' = prefix_types |> CCList.map (S.apply_ty ty_subst) in
                  let return_type' = S.apply_ty ty_subst return_type in
                  Some (S.add ty_subst v' (iterate_one types_w v' prefix_types' return_type' i alpha'))
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

(* Find disagreeing subterms. 
    This function also returns a list of variables occurring above the
    disagreement pair, along with the index of the argument that the disagreement
    pair occurs in. 
    TODO: preferably one that is not below a variable (to get preunification if possible) 
*)
let find_disagreement s t = 
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
        |> (fun s -> OSeq.append s (find_disagreement_l ~applied_var ~argindex:(argindex + 1) ss' tt'))
      | _, _ -> raise (Invalid_argument "types of unified terms should be equal")
  and find_disagreement_aux s t = 
    match T.view s, T.view t with
      | T.App (f, ss), T.App (g, tt) when f = g && not (T.is_var f) -> 
        find_disagreement_l ss tt 
      | T.App (f, ss), T.App (g, tt) when f = g && T.is_var f -> 
        find_disagreement_l ~applied_var:(Some f) ss tt 
      | T.AppBuiltin (f, ss), T.AppBuiltin (g, tt) when f = g -> 
        find_disagreement_l ss tt 
      | T.Var x, T.Var y when x = y -> OSeq.empty
      | T.DB i, T.DB j when i = j -> OSeq.empty
      | T.Const a, T.Const b when a = b -> OSeq.empty
      | T.Fun (ty_s, s'), T.Fun (ty_t, t') -> 
        assert (ty_s == ty_t); 
        find_disagreement_aux s' t'
      | _ -> OSeq.return ((s, t),[])
  in
  let s = find_disagreement_aux s t in
  match OSeq.find (fun ((u,v),l) -> CCList.is_empty l) s with
    | Some d -> Some d
    | None -> 
      begin match s () with
        | OSeq.Nil -> None
        | OSeq.Cons (d, _) -> Some d
      end


(* TODO: Replace by the updated OSeq.merge *)
(** Dovetailing through a sequence of sequences:
    (0,0),(1,0),(0,1),(2,0),(1,1),(0,2),(3,0),(2,1),(1,2),(0,3),(4,0),(3,1),(2,2),(1,3),(0,4),... *)
let dovetail seqs () =
  let rec aux passive_seqs active_seqs () =
    match passive_seqs() with 
    | OSeq.Nil -> 
      (* All seqs are active now *)
      if CCList.is_empty active_seqs 
      then Seq.empty ()
      else
        let line, tail = get_line active_seqs in
        OSeq.append line (aux passive_seqs tail) ()
    | OSeq.Cons (new_seq, passive_seqs') ->
      (* Move new seq from passive to active *)
      let active_seqs = new_seq :: active_seqs in
      let line, active_seqs = get_line active_seqs in
      OSeq.append line (aux passive_seqs' active_seqs) ()
  and get_line active_seqs =
    (* Retrieve the head of each active sequence & remove empty active sequences from the list *)
    CCList.fold_right
      (fun seq (line, active_seqs) ->
        match seq() with
        | OSeq.Nil -> (line, active_seqs)
        | OSeq.Cons (read, seq') -> (OSeq.cons read line, seq' :: active_seqs)
      ) active_seqs (OSeq.empty, [])
  in
  aux seqs [] () (* Initially, all seqs are passive *)

let unify t s = 
  let rec unify_terms ?(rules = []) t s  =
    (* Util.debugf 1 "@[Unify@ @[(rules: %a)@]@ @[%a@]@ and@ @[%a@]@]" (fun k -> k (CCList.pp CCString.pp) rules T.pp t T.pp s); *)
    match find_disagreement t s with
      | Some ((u, v), l) -> 
        let add_some f u v l = f u v l |> OSeq.map (fun s -> Some s) in
        [add_some project,"proj"; add_some imitate,"imit"; add_some identify,"id"; add_some eliminate,"elim"; iterate,"iter"]
        (* iterate must be last in this list because it is the only one with infinitely many child nodes *)
        |> OSeq.of_list  
        |> OSeq.flat_map
          (fun (rule,rulename) -> 
            rule u v l 
            |> OSeq.map (fun subst -> (subst, rulename)))
        |> OSeq.map 
          (fun (osubst,rulename) -> 
            match osubst with
            | Some subst ->
              let t_subst = nfapply subst t in
              let s_subst = nfapply subst s in
              let unifiers = unify_terms t_subst s_subst ~rules:(rules @ [rulename]) in
              unifiers 
              |> OSeq.map (CCOpt.map (fun unifier -> S.append subst unifier))
            | None -> OSeq.empty
          )
        |> dovetail 
        |> OSeq.append (OSeq.return None)
      | None -> 
        assert (t == s);
        Util.debugf 1 "@[...unified!@ @[(rules: %a)@]@]" (fun k -> k (CCList.pp CCString.pp) rules); 
        OSeq.return (Some S.empty)
  in
  (* Unify types first ... *)
  match unif_ty (T.ty t) (T.ty s) with
    | Some type_unifier ->
      let t' = nfapply type_unifier t in
      let s' = nfapply type_unifier s in
      (* ... then terms. *)
      let term_unifiers = unify_terms t' s' ~rules:[] in
      OSeq.map (CCOpt.map (S.append type_unifier)) term_unifiers
    | None -> OSeq.empty

(* TODO: Remove tracking of rules for efficiency? *)

let unify_nonterminating t s = OSeq.filter_map (fun x -> x) (unify t s)


(* TODO: better solution for fresh vars? *)
(* TODO: Polymorphism? *)
(* TODO: operate on inner types like in `Unif`. Test for NO-TYPE terms. *)
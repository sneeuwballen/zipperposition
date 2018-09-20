
(* This file is free software, part of Zipperposition. See file "license" for more details. *)

(** {1 Jensen-Pietrzykowski Unification} *)

module T = Term
module S = Subst

let scope = 0 (* TODO: scopes *)

let subst_bind s (v:T.var) t = ((Subst.FO.bind s ((v,scope):>InnerTerm.t HVar.t Scoped.t)) (t,scope)) 

(** Find disagreeing subterms. 
    This function also returns a list of variables occurring above the
    disagreement pair, along with the index of the argument that the disagreement
    pair occurs in. 
    TODO: preferably one that is not below a variable (to get preunification if possible) 
*)
let rec find_disagreement s t = 
  let rec find_disagreement_l ?(applied_var = None) ?(argindex=0) ss tt = 
    match ss, tt with
      | [], [] -> None
      | s' :: ss', t' :: tt' -> 
        let d = find_disagreement s' t' in 
        begin match d with
        | Some ((u,v),l) -> 
          begin match applied_var with
            | Some x -> Some ((u,v), (T.as_var_exn x, argindex) :: l) 
            | None -> d
          end
        | None -> 
            let argindex = argindex + 1 in
            find_disagreement_l ~applied_var ~argindex ss' tt'
        end
      | _, _ -> raise (Invalid_argument "types of unified terms should be equal")
  in
  match T.view s, T.view t with
    | T.App (f, ss), T.App (g, tt) when f = g && not (T.is_var f)-> find_disagreement_l ss tt 
    | T.App (f, ss), T.App (g, tt) when f = g && T.is_var f -> find_disagreement_l ~applied_var:(Some f) ss tt 
    | T.AppBuiltin (f, ss), T.AppBuiltin (g, tt) when f = g -> find_disagreement_l ss tt 
    | T.Var x, T.Var y when x = y -> None
    | T.DB i, T.DB j when i = j -> None
    | T.Const a, T.Const b when a = b -> None
    | T.Fun (ty_s, s'), T.Fun (ty_t, t') -> find_disagreement s' t' (*TODO: what about the types?*)
    | _ -> Some ((s, t),[])


(* Projection rule *)

(* find substitutions for the projection rule, given a member of the disagreement pair *)
let project_onesided u =
  let head, args = T.as_app u in
  let prefix_types, return_type = Type.open_fun (T.ty head) in
  if T.is_var head 
  then
    args 
    |> OSeq.of_list
    |> OSeq.mapi (fun i arg -> List.length prefix_types - 1 - i, arg) (* Determine DB-index of the argument *)
    |> OSeq.filter (fun (dbindex, arg) -> T.ty arg = return_type) (* necessary to make substitution type correct *)
    |> OSeq.map (fun (dbindex, arg) -> 
        (* substitute x for a projection to the j-th argument *)
        subst_bind Subst.empty (T.as_var_exn head) (T.fun_l prefix_types (T.bvar ~ty:(T.ty arg) dbindex)) 
    )
  else OSeq.empty

(* find substitutions for the projection rule, given a disagreement pair *)
let project u v (_ : (T.var * int) list) = OSeq.append (project_onesided u) (project_onesided v)


(* Imitation rule *)

let imitate_onesided u v = 
  let head_u, args_u = T.as_app u in
  let head_v, args_v = T.as_app v in
  let prefix_types_u, _ = Type.open_fun (T.ty head_u) in
  let prefix_types_v, _ = Type.open_fun (T.ty head_v) in
  if T.is_var head_u && (T.is_var head_v || T.is_const head_v) 
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
    let subst = subst_bind Subst.empty (T.as_var_exn head_u) subst_value in
    OSeq.return subst
  else OSeq.empty

(* find substitutions for the projection rule, given a disagreement pair *)
let imitate u v (_ : (T.var * int) list) = OSeq.append (imitate_onesided u v) (imitate_onesided v u)


(* Identification rule *)

let identify u v (_ : (T.var * int) list) =
  let head_u, args_u = T.as_app u in
  let head_v, args_v = T.as_app v in
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
    let subst = Subst.empty in
    let subst = subst_bind subst (T.as_var_exn head_u) subst_value_u in
    let subst = subst_bind subst (T.as_var_exn head_v) subst_value_v in
    OSeq.return subst
  else OSeq.empty

let eliminate u v l =
  l |> List.map (fun (v, k) -> 
    (* create substitution: v |-> λ u1 ... um. x u1 ... u{k-1} u{k+1} ... um *)
    let prefix_types, return_type = Type.open_fun (HVar.ty v) in
    let bvars = prefix_types |> List.rev |> List.mapi (fun i ty -> T.bvar ~ty i) |> List.rev in
    let bvars' = CCList.remove_at_idx k bvars in
    let matrix_head = T.var (HVar.fresh ~ty:(Type.arrow (prefix_types) return_type) ()) in
    let matrix = T.app matrix_head bvars' in
    let subst_value = T.fun_l prefix_types matrix in
    let subst = subst_bind Subst.empty v subst_value in
    subst
  )
  |> OSeq.of_list
(* TODO: use OSeq directly? *)


(* apply a substitution and reduce to normal form *)
let nfapply s u = Lambda.snf (S.FO.apply S.Renaming.none s (u, scope))

(* TODO: comparison form is actually slightly different from short normal form! *)

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
  let rec unify_rec t s ?(rules = []) =
    Util.debugf 1 "Unify (rules: %a) %a and %a" (fun k -> k (CCList.pp CCString.pp) rules T.pp t T.pp s);
    match find_disagreement t s with
      | Some ((u, v), l) -> 
        [project,"proj"; imitate,"imit"; identify,"id"; eliminate,"elim"] 
        |> OSeq.of_list  
        |> OSeq.flat_map
          (fun (rule,rulename) -> 
            rule u v l 
            |> OSeq.map (fun subst -> (subst, rulename)))
        |> OSeq.map 
          (fun (subst,rulename) -> 
            let t_subst = nfapply subst t in
            let s_subst = nfapply subst s in
            let unifiers = unify_rec t_subst s_subst ~rules:(rules @ [rulename]) in
            unifiers 
            |> OSeq.map (CCOpt.map (fun unifier -> Subst.merge unifier subst))
            (* TODO: we actually want to concatenate, not merge *)
          )
        |> dovetail
        |> OSeq.append (OSeq.return None)
      | None -> 
        Util.debugf 1 "-- unified!" (fun k -> k);
        OSeq.return (Some Subst.empty)
  in
  unify_rec t s ~rules:[]

(* TODO: Remove tracking of rules for efficiency? *)

let unify_nonterminating t s = OSeq.take 10 (OSeq.filter_map (fun x -> x) (unify t s))


(* TODO: better solution for fresh vars? *)
(* TODO: Polymorphism? *)
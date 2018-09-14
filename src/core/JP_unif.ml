
(* This file is free software, part of Zipperposition. See file "license" for more details. *)

(** {1 Jensen-Pietrzykowski Unification} *)

module T = Term
module S = Subst


let scope = 0 (* TODO: scopes *)

let subst_bind s (v:T.var) t = ((Subst.FO.bind s ((v,scope):>InnerTerm.t HVar.t Scoped.t)) (t,scope)) 

(* Find disagreeing subterms, TODO: preferably one that is not below a variable (to get preunification if possible) *)
let rec find_disagreement s t = 
  let rec find_disagreement_l ss tt = 
    match ss, tt with
      | [], [] -> None
      | s' :: ss', t' :: tt' -> 
        let d = find_disagreement s' t' in 
        if d = None then find_disagreement_l ss' tt' else d
      | _, _ -> raise (Invalid_argument "types of unified terms should be equal")
  in
  match T.view s, T.view t with
    | T.App (f, ss), T.App (g, tt) when f = g -> find_disagreement_l ss tt 
    | T.AppBuiltin (f, ss), T.AppBuiltin (g, tt) when f = g -> find_disagreement_l ss tt 
    | T.Var x, T.Var y when x = y -> None
    | T.DB i, T.DB j when i = j -> None
    | T.Const a, T.Const b when a = b -> None
    | T.Fun (ty_s, s'), T.Fun (ty_t, t') -> find_disagreement s' t' (*TODO: what about the types?*)
    | _ -> Some (s, t)


(* Projection rule *)

(* find substitutions for the projection rule, given a member of the disagreement pair *)
let project_onesided u =
  let head, args = T.as_app u in
  let prefix_types, return_type = Type.open_fun (T.ty head) in
  if T.is_var head 
  then
    args 
    |> Sequence.of_list
    |> Sequence.mapi (fun i arg -> List.length prefix_types - 1 - i, arg) (* Determine DB-index of the argument *)
    |> Sequence.filter (fun (dbindex, arg) -> T.ty arg = return_type) (* necessary to make substitution type correct *)
    |> Sequence.map (fun (dbindex, arg) -> 
        (* substitute x for a projection to the j-th argument *)
        subst_bind Subst.empty (T.as_var_exn head) (T.fun_l prefix_types (T.bvar ~ty:(T.ty arg) dbindex)) 
    )
  else Sequence.empty

(* find substitutions for the projection rule, given a disagreement pair *)
let project u v = Sequence.append (project_onesided u) (project_onesided v)


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
    Sequence.singleton subst
  else Sequence.empty

(* find substitutions for the projection rule, given a disagreement pair *)
let imitate u v = Sequence.append (imitate_onesided u v) (imitate_onesided v u)


(* Identification rule *)

let identify u v =
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
    Sequence.singleton subst
  else Sequence.empty




(* apply a substitution and reduce to normal form *)
let nfapply s u = Lambda.snf (S.FO.apply S.Renaming.none s (u, scope))




let rec unify t s ?(rules=[]) = 
  Util.debugf 1 "Unify (rules: %a) %a and %a" (fun k -> k (CCList.pp CCString.print) rules T.pp t T.pp s);
  match find_disagreement t s with
    | Some (u, v) -> 
      [project,"proj"; imitate,"imit"; identify,"id"] 
      |> Sequence.of_list 
      |> Sequence.flat_map (fun (rule,rulename) -> rule u v |> Sequence.map (fun subst -> (subst, rulename)))
      |> Sequence.flat_map (fun (subst,rulename) -> 
        let t_subst = nfapply subst t in
        let s_subst = nfapply subst s in
        let unifiers = unify t_subst s_subst ~rules:(rules @ [rulename]) in
        unifiers 
        |> Sequence.map (fun unifier -> Subst.merge unifier subst)
        )
    | None -> 
      Util.debugf 1 "-- unified!" (fun k -> k);
      Sequence.singleton Subst.empty


(* TODO: better solution for fresh vars? *)
(* TODO: Polymorphism? *)
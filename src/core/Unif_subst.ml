
(* This file is free software, part of Zipperposition. See file "license" for more details. *)

(** {1 Unification Substitution} *)

(** A tuple containing:

    - the substitution itself
    - delayed constraints
*)

module H = HVar

type term = Subst.term
type var = Subst.var

type t = {
  subst: Subst.t;
  cstr_l: Unif_constr.t list;
}

let subst t = t.subst
let constr_l t = t.cstr_l

let make subst cstr_l = {subst; cstr_l}

let empty : t = make Subst.empty []

let is_empty (s:t): bool =
  Subst.is_empty (subst s) &&
  CCList.is_empty (constr_l s)

let map_subst ~f t = {t with subst=f t.subst}
let of_subst s = make s []

let tags (s:t) : _ list = CCList.flat_map Unif_constr.tags (constr_l s)

let bind t v u = {t with subst=Subst.bind t.subst v u}
let update t v u = {t with subst=Subst.update t.subst v u}
let mem t v = Subst.mem t.subst v
let deref t v = Subst.deref t.subst v
let merge t1 t2 = {subst=Subst.merge t1.subst t2.subst; cstr_l = t1.cstr_l @ t2.cstr_l} 
let compose ~scope t1 t2 = {subst=Subst.FO.compose ~scope t1.subst t2.subst; cstr_l = t1.cstr_l @ t2.cstr_l}

module FO = struct
  let bind t (v:Type.t HVar.t Scoped.t) u =
    {t with subst=Subst.FO.bind t.subst (v:>InnerTerm.t HVar.t Scoped.t) u}
  let mem t (v:Type.t HVar.t Scoped.t) =
    Subst.mem t.subst (v :> InnerTerm.t HVar.t Scoped.t)
  let deref s t = Subst.FO.deref s.subst t
  let singleton v t = bind empty v t

  (* did not use rename_l_to_new_scope for efficiency *)
  let rename_to_new_scope ~counter (t0, scope0) (t1, scope1) =
    let apply s t = Subst.FO.apply Subst.Renaming.none (subst s) t in
    let apply_ty s ty = Subst.Ty.apply Subst.Renaming.none (subst s) ty in
    let new_scope = if scope0 < scope1 then scope1 + 1 else scope0 + 1 in
    let add_renaming scope subst v =
      if mem subst (v,scope) 
      then subst
      else 
        let ty = apply_ty subst (HVar.ty v, scope) in
        let newvar = Term.var (H.fresh_cnt ~counter ~ty ()) in
        bind subst (v,scope) (newvar, new_scope) 
    in
    let subst = empty in
    let subst = Term.Seq.vars t0 |> Iter.fold (add_renaming scope0) subst in
    let subst = Term.Seq.vars t1 |> Iter.fold (add_renaming scope1) subst in
    let t0', t1' = apply subst (t0, scope0), apply subst (t1, scope1) in
    t0', t1', new_scope, subst

  let rename_l_to_new_scope ~counter (t0s, scope0) (t1s, scope1) =
    let apply s t = Subst.FO.apply Subst.Renaming.none (subst s) t in
    let apply_ty s ty = Subst.Ty.apply Subst.Renaming.none (subst s) ty in
    let new_scope = if scope0 < scope1 then scope1 + 1 else scope0 + 1 in
    let add_renaming scope subst v =
      if mem subst (v,scope) 
      then subst
      else 
        let ty = apply_ty subst (HVar.ty v, scope) in
        let newvar = Term.var (H.fresh_cnt ~counter ~ty ()) in
        bind subst (v,scope) (newvar, new_scope) 
    in
    let subst = empty in

    let collect_vars = List.fold_left (fun acc t -> Iter.append (Term.Seq.vars t) acc ) Iter.empty in

    let subst = collect_vars t0s |> Iter.fold (add_renaming scope0) subst in
    let subst = collect_vars t1s |> Iter.fold (add_renaming scope1) subst in
    let t0', t1' = List.map (fun t0 -> apply subst (t0, scope0)) t0s,
                   List.map (fun t1 -> apply subst (t1, scope1)) t1s  in
    t0', t1', new_scope, subst
end

let has_constr t: bool = constr_l t <> []

let add_constr c t = {t with cstr_l = c :: t.cstr_l}

let pp out t: unit =
  if has_constr t
  then Format.fprintf out "(@[%a@ :constr_l (@[<hv>%a@])@])"
      Subst.pp (subst t) (Util.pp_list ~sep:"" Unif_constr.pp) (constr_l t)
  else Subst.pp out (subst t)

let equal a b =
  Subst.equal (subst a) (subst b) &&
  CCList.equal Unif_constr.equal (constr_l a) (constr_l b)

let hash a =
  Hash.combine2 (Subst.hash @@ subst a)
    (Hash.list Unif_constr.hash @@ constr_l a)

let compare a b =
  let open CCOrd.Infix in
  Subst.compare (subst a)(subst b)
  <?> (CCList.compare Unif_constr.compare, constr_l a, constr_l b)

let to_string = CCFormat.to_string pp

let constr_l_subst renaming (s:t): _ list =
  constr_l s
  |> Unif_constr.apply_subst_l renaming (subst s)

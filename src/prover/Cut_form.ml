
(* This file is free software, part of Zipperposition. See file "license" for more details. *)

(** {1 Universally Quantified Conjunction of Clauses} *)

open Logtk

module Fmt = CCFormat
module T = FOTerm

type var = FOTerm.var
type term = FOTerm.t
type clause = Literals.t
type form = clause list

type t = {
  vars: T.VarSet.t;
  cs: form;
}

let trivial = {cs=[]; vars=T.VarSet.empty}

let make cs =
  if cs=[] then trivial
  else (
    let vars =
      Sequence.of_list cs
      |> Sequence.flat_map Literals.Seq.vars
      |> T.VarSet.of_seq
    and cs = CCList.sort_uniq ~cmp:Literals.compare cs in
    {cs; vars;}
  )

let vars t = t.vars
let cs t = t.cs

let hash (f:t): int = Hash.list Literals.hash f.cs
let equal f1 f2: bool = CCList.equal Literals.equal f1.cs f2.cs
let compare f1 f2 = CCList.compare Literals.compare f1.cs f2.cs

let pp out (f:t): unit =
  let pp_c = Literals.pp in
  let pp_body out () = match f.cs with
    | [c] -> pp_c out c
    | _ -> Fmt.fprintf out "@<1>∧{@[<hv>%a@]}" (Util.pp_list ~sep:"," pp_c) f.cs
  in
  if T.VarSet.is_empty f.vars then (
    pp_body out ()
  ) else (
    Fmt.fprintf out "(@[<2>forall %a.@ %a@])"
      (Util.pp_list ~sep:" " Type.pp_typed_var) (T.VarSet.to_list f.vars) pp_body ()
  )

let to_string = Fmt.to_string pp

let pp_tstp out (f:t): unit =
  let pp_c = Fmt.within "(" ")" Literals.pp_tstp in
  let pp_body out () = match f.cs with
    | [c] -> pp_c out c
    | _ -> Fmt.fprintf out "(@[%a@])" (Util.pp_list ~sep:" & " pp_c) f.cs
  in
  if T.VarSet.is_empty f.vars then (
    pp_body out ()
  ) else (
    Fmt.fprintf out "(@[<2>![%a]:@ (%a)@])"
      (Util.pp_list Type.TPTP.pp_typed_var) (T.VarSet.to_list f.vars) pp_body ()
  )

let ind_vars t =
  vars t
  |> T.VarSet.to_list
  |> List.filter
    (fun v ->
       let ty = HVar.ty v in
       (* only do induction on variables of infinite types *)
       begin match Ind_ty.as_inductive_type ty with
         | Some (ity,_) -> Ind_ty.is_recursive ity
         | None -> false
       end)

let apply_subst ~renaming subst (f,sc): t =
  let cs =
    List.map (fun lits -> Literals.apply_subst ~renaming subst (lits,sc)) f.cs
  in
  make cs

let subst1 (v:var) (t:term) (f:t): t =
  let renaming = Subst.Renaming.create () in
  let subst =
    Subst.FO.bind Subst.empty ((v:var:>InnerTerm.t HVar.t),0) (t,1)
  in
  apply_subst ~renaming subst (f,0)

(* find substitutions making [f1] and [f2] variants, if possible *)
let variant ~subst (f1,sc1)(f2,sc2): Subst.t Sequence.t =
  Unif.unif_list_com ~size:`Same subst
    ~op:(fun subst c1 c2 -> Literals.variant ~subst c1 c2)
    (f1.cs,sc1)(f2.cs,sc2)

let are_variant f1 f2: bool =
  not @@ Sequence.is_empty @@ variant ~subst:Subst.empty (f1,1)(f2,0)

let normalize (f:t): t = cs f |> Test_prop.normalize_form |> make


(* This file is free software, part of Zipperposition. See file "license" for more details. *)

(** {1 Universally Quantified Conjunction of Clauses} *)

open Logtk

module Fmt = CCFormat
module T = FOTerm

type var = FOTerm.var
type term = FOTerm.t

type t = {
  vars: T.VarSet.t;
  cs: Literals.t list;
}

let trivial = {cs=[]; vars=T.VarSet.empty}
let make cs =
  let vars =
    Sequence.of_list cs
    |> Sequence.flat_map Literals.Seq.vars
    |> T.VarSet.of_seq
  in
  {cs; vars;}

let vars t = t.vars
let cs t = t.cs

let pp out (f:t): unit =
  let pp_c = Literals.pp in
  let pp_body out () = match f.cs with
    | [c] -> pp_c out c
    | _ -> Fmt.fprintf out "∧{@[%a@]}" (Util.pp_list ~sep:"," pp_c) f.cs
  in
  if T.VarSet.is_empty f.vars then (
    pp_body out ()
  ) else (
    Fmt.fprintf out "(@[<2>forall %a.@ %a@])"
      (Util.pp_list Type.pp_typed_var) (T.VarSet.to_list f.vars) pp_body ()
  )

let ind_vars t =
  vars t
  |> T.VarSet.to_list
  |> List.filter (fun v -> Ind_ty.is_inductive_type (HVar.ty v))

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

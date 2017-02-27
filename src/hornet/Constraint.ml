
(* This file is free software, part of Zipperposition. See file "license" for more details. *)

(** {1 Constraint for a Clause} *)

open Logtk
open Hornet_types

type t = c_constraint

let empty : t = {
  constr_dismatch=[];
}

let pp = Hornet_types_util.pp_constraint
let to_string = CCFormat.to_string pp

let is_trivial (c:t): bool =
  List.for_all Dismatching_constr.is_trivial c.constr_dismatch

let is_absurd (c:t): bool =
  List.exists Dismatching_constr.is_absurd c.constr_dismatch

let is_absurd_with subst (c,sc): bool =
  List.exists
    (fun d -> Dismatching_constr.is_absurd_with subst (d,sc))
    c.constr_dismatch

let add_dismatch (d:Dismatching_constr.t) (c:t): t =
  if Dismatching_constr.is_empty d
  then c
  else (
    let constr_dismatch =
      CCList.sorted_insert ~uniq:true ~cmp:Dismatching_constr.compare
        d c.constr_dismatch;
    in
    { constr_dismatch; }
  )

let of_list (l:Dismatching_constr.t list): t =
  let constr_dismatch = CCList.sort_uniq ~cmp:Dismatching_constr.compare l in
  { constr_dismatch; }

let apply_subst ~renaming subst (c,sc): t =
  c.constr_dismatch
  |> CCList.filter_map
    (fun d ->
       let d' = Dismatching_constr.apply_subst ~renaming subst (d,sc) in
       if Dismatching_constr.is_empty d' then None else Some d')
  |> of_list

let variant ~subst (c1,sc1)(c2,sc2): Subst.t Sequence.t =
  Unif.unif_list_com subst
    (c1.constr_dismatch,sc1)(c2.constr_dismatch,sc2)
    ~op:(fun subst a b -> Dismatching_constr.variant ~subst a b)

let matching ?(subst=Subst.empty) (c1,sc1)(c2,sc2): Subst.t Sequence.t =
  Unif.unif_list_com subst
    (c1.constr_dismatch,sc1)(c2.constr_dismatch,sc2)
    ~op:(fun subst a b -> Dismatching_constr.matching ~subst a b)

let subsumes ?subst a b: bool =
  not (Sequence.is_empty (matching ?subst a b))

let combine (a:t)(b:t): t =
  { constr_dismatch =
      CCList.sorted_merge_uniq ~cmp:Dismatching_constr.compare
        a.constr_dismatch b.constr_dismatch;
  }

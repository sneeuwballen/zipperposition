
(* This file is free software, part of Zipperposition. See file "license" for more details. *)

(** {1 Labelled Clause} *)

open Libzipperposition
open Hornet_types

module T = FOTerm

type t = labelled_clause

let make c sel subst : t = {lc_clause=c; lc_sel=sel; lc_subst=subst}

let make_empty (c:clause) (sel:select_lit): t =
  (* initial subst: maps each var to itself *)
  let subst =
    IArray.to_seq c.c_lits
    |> Sequence.flat_map Lit.vars_seq
    |> T.VarSet.of_seq
    |> T.VarSet.to_seq
    |> Sequence.map (fun v -> v, T.var v)
    |> Type.VarMap.of_seq
  in
  make c sel subst

let equal = Hornet_types_util.equal_lc
let hash = Hornet_types_util.hash_lc
let compare = Hornet_types_util.compare_lc
let pp = Hornet_types_util.pp_lc
let to_string = CCFormat.to_string pp

let filter_subst = Hornet_types_util.lc_filter_subst

let apply_subst ~renaming subst (lc,sc) =
  let lc_subst =
    Type.VarMap.map (fun t -> Subst.FO.apply ~renaming subst (t,sc))
      lc.lc_subst
  in
  { lc with lc_subst }

let is_empty (lc:t) = Sequence.is_empty (filter_subst lc)

let to_subst (lc:t): Subst.t =
  let sc = 0 in
  filter_subst lc
  |> Sequence.map
    (fun (v,t) ->
       (* add scope, perform ugly casting *)
       ((v:Type.t HVar.t:>InnerTerm.t HVar.t),sc), ((t:T.t:>InnerTerm.t),sc))
  |> Subst.of_seq

let to_dismatch (lc:t): Dismatching_constr.t =
  filter_subst lc
  |> Sequence.map (fun (v,t) -> T.var v, t)
  |> Sequence.to_rev_list
  |> CCFun.tap (fun l -> assert (l<>[]))
  |> Dismatching_constr.make

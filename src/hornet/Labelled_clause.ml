
(* This file is free software, part of Zipperposition. See file "license" for more details. *)

(** {1 Labelled Clause} *)

open Libzipperposition
open Hornet_types

module T = FOTerm

type t = labelled_clause

let make c subst : t = {lc_clause=c; lc_subst=subst}

let make_empty (c:clause): t =
  (* initial subst: maps each var to itself *)
  let subst =
    IArray.to_seq c.c_lits
    |> Sequence.flat_map Lit.vars_seq
    |> T.VarSet.of_seq
    |> T.VarSet.to_seq
    |> Sequence.map (fun v -> v, T.var v)
    |> Type.VarMap.of_seq
  in
  make c subst

let equal = Hornet_types_util.equal_lc
let hash = Hornet_types_util.hash_lc
let compare = Hornet_types_util.compare_lc
let pp = Hornet_types_util.pp_lc
let to_string = CCFormat.to_string pp

let apply_subst ~renaming subst (lc,sc) =
  let lc_subst =
    Type.VarMap.map (fun t -> Subst.FO.apply ~renaming subst (t,sc))
      lc.lc_subst
  in
  { lc with lc_subst }

let to_subst (lc:t): Subst.t =
  let sc = 0 in
  Type.VarMap.to_seq lc.lc_subst
  |> Sequence.map
    (fun (v,t) ->
       (* add scope, perform ugly casting *)
       ((v:Type.t HVar.t:>InnerTerm.t HVar.t),sc), ((t:T.t:>InnerTerm.t),sc))
  |> Subst.of_seq


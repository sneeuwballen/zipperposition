
(* This file is free software, part of Zipperposition. See file "license" for more details. *)

(** {1 Labelled Clause} *)

open Logtk
open Hornet_types

module T = FOTerm

type t = labelled_clause

let make c sel subst lc_real_subst : t =
  {lc_clause=c; lc_sel=sel; lc_subst=subst; lc_real_subst}

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
  make c sel subst (Lazy.from_val Subst.empty)

let equal = Hornet_types_util.equal_lc
let hash = Hornet_types_util.hash_lc
let compare = Hornet_types_util.compare_lc
let pp = Hornet_types_util.pp_lc
let to_string = CCFormat.to_string pp

let same_clause lc1 lc2: bool = 
  Hornet_types_util.equal_clause lc1.lc_clause lc2.lc_clause

let hash_mod_alpha (lc:t): int =
  Hash.combine2
    (Hornet_types_util.hash_clause lc.lc_clause)
    (Hash.(list_comm (pair HVar.hash T.hash_mod_alpha))
       (Type.VarMap.to_list lc.lc_subst))

let filter_subst = Hornet_types_util.lc_filter_subst

let to_subst (lc:t): Subst.t = Lazy.force lc.lc_real_subst

let to_subst_real lc_subst: Subst.t =
  Type.VarMap.to_seq lc_subst
  |> Sequence.map
    (fun (v,t) ->
       (* add scope, perform ugly casting *)
       ((v:Type.t HVar.t:>InnerTerm.t HVar.t),0), ((t:T.t:>InnerTerm.t),1))
  |> Subst.of_seq

let apply_subst ~renaming subst (lc,sc) =
  let lc_subst =
    Type.VarMap.map (fun t -> Subst.FO.apply ~renaming subst (t,sc))
      lc.lc_subst
  in
  { lc with lc_subst; lc_real_subst=lazy (to_subst_real lc_subst) }

(* empty if the substitution is empty, or if it only renames binds
   variables to other variables *)
let is_empty (lc:t) =
  let subst = to_subst lc in
  Subst.is_empty subst ||
  ( Subst.codomain subst
    |> Sequence.for_all (fun (t,_) -> T.is_var (T.of_term_unsafe t)))

(* absurd if at least one constraint of the clause is absurd under
   current substitution.
   NOTE: this is not cachable, as [lc.lc_clause.c_constr] is mutable. *)
let has_no_ground_instance_ (lc:t): bool =
  Constraint.is_absurd_with
    (to_subst lc)
    (lc.lc_clause.c_constr,0)

let prof_no_instance = Util.mk_profiler "labelled_clause.has_no_instance"
let has_no_ground_instance lc = Util.with_prof prof_no_instance has_no_ground_instance_ lc

let to_dismatch (lc:t): Dismatching_constr.t =
  filter_subst lc.lc_subst
  |> Sequence.map (fun (v,t) -> T.var v, t)
  |> Sequence.to_rev_list
  |> CCFun.tap (fun l -> assert (l<>[]))
  |> Dismatching_constr.make

(* the literals corresponding to instantiating the clause with the subst *)
let lits_instance lc: Lit.t IArray.t =
  let subst = to_subst lc in
  Lit.apply_subst_arr_no_renaming subst (lc.lc_clause.c_lits,0)

(* find whether these are variants.
   We use [C.equal] to compare clauses, so it's not totally structural. *)
let variant ?(subst=Subst.empty) (lc1,sc1)(lc2,sc2): Subst.t Sequence.t =
  if Hornet_types_util.equal_clause lc1.lc_clause lc2.lc_clause
  then (
    assert (Type.VarMap.cardinal lc1.lc_subst = Type.VarMap.cardinal lc2.lc_subst);
    Unif.unif_list subst
      (Type.VarMap.values lc1.lc_subst |> Sequence.to_rev_list, sc1)
      (Type.VarMap.values lc2.lc_subst |> Sequence.to_rev_list, sc2)
      ~op:(fun subst a b ->
        try Sequence.return (Unif.FO.variant ~subst a b)
        with Unif.Fail -> Sequence.empty)
  ) else Sequence.empty

(* it is unclear what would be the exact criterion for a labelled clause
   to subsume another instance of the same clause with a difference
   substitution, so we only accept that [lc1] subsumes [lc2] if
   [lc1 = lc2] *)
let matching = variant


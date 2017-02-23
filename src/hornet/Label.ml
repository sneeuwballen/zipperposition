
(* This file is free software, part of Zipperposition. See file "license" for more details. *)

(** {1 Label for Horn Clauses} *)

open Libzipperposition
open Hornet_types

module Fmt = CCFormat
module LC = Labelled_clause

type t = label
(** Set of labelled clauses.
    Invariants: sorted; all labelled instances of a clause have same literal *)

let return l : t = [l]

let make l = CCList.sort_uniq ~cmp:LC.compare l

let is_empty = CCList.is_empty

let all_empty = List.for_all LC.is_empty

let check_inv_sorted_ (l:t): bool =
  CCList.is_sorted ~cmp:LC.compare l

(* check invariant about labelled instances *)
let check_inv_ (l:t): bool =
  List.for_all
    (fun lc ->
       List.for_all
         (fun lc' ->
            if Hornet_types_util.equal_clause lc.lc_clause lc'.lc_clause
            then lc.lc_sel.select_idx = lc'.lc_sel.select_idx
            else true)
         l)
    l

let has_no_ground_instance l=
  assert (check_inv_sorted_ l);
  assert (check_inv_ l);
  List.exists LC.has_no_ground_instance l

let merge = CCList.sorted_merge_uniq ~cmp:LC.compare

let apply_subst ~renaming subst (l,sc) =
  l
  |> List.rev_map (fun lc -> LC.apply_subst ~renaming subst (lc,sc))
  |> make

let to_list (t:t) : _ list = t
let to_seq = Sequence.of_list

let pp = Hornet_types_util.pp_label

let to_string = Fmt.to_string pp

let hash (t:t): int = Hash.list LC.hash t

let hash_mod_alpha (t:t): int = Hash.list_comm LC.hash_mod_alpha t

let equal (a:t)(b:t): bool = CCList.equal LC.equal a b

(* TODO: use LC.hash_mod_alpha to partition elements?
   maybe add this to unif_list_com as optional arg *)
let variant ?(subst=Subst.empty) (l1,sc1)(l2,sc2): Subst.t Sequence.t =
  Unif.unif_list_com subst (l1,sc1)(l2,sc2)
    ~op:(fun subst a b -> LC.variant ~subst a b)

let matching ?(subst=Subst.empty) (l1,sc1)(l2,sc2): Subst.t Sequence.t =
  Unif.unif_list_com subst (l1,sc1)(l2,sc2)
    ~op:(fun subst a b -> LC.matching ~subst a b)

let subsumes ?(subst=Subst.empty) (l1,sc1) (l2,sc2) =
  Unif.unif_list_com
    ~size:`Smaller
    ~op:(fun subst a b -> LC.matching ~subst a b)
    subst (l1,sc1)(l2,sc2)

let subsumes_pred ?subst a b: bool =
  not (Sequence.is_empty (subsumes ?subst a b))

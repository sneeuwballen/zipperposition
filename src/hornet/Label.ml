
(* This file is free software, part of Zipperposition. See file "license" for more details. *)

(** {1 Label for Horn Clauses} *)

open Libzipperposition
open Hornet_types

module Fmt = CCFormat
module LC = Labelled_clause

type t = label
(** Set of labelled clauses. Invariant: sorted *)

let return l : t = [l]

let make l = CCList.sort_uniq ~cmp:LC.compare l

let is_empty = CCList.is_empty

let all_empty = List.for_all LC.is_empty

let has_no_ground_instance = List.exists LC.has_no_ground_instance

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


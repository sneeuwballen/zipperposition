
(* This file is free software, part of Zipperposition. See file "license" for more details. *)

(** {1 Label for Horn Clauses} *)

open Hornet_types

module Fmt = CCFormat
module LC = Labelled_clause

type t = label
(** Set of labelled clauses. Invariant: sorted *)

let return l : t = [l]

let make l = CCList.sort_uniq ~cmp:LC.compare l

let all_empty = List.for_all LC.is_empty

let merge = CCList.sorted_merge ~cmp:LC.compare

let apply_subst ~renaming subst (l,sc) =
  l
  |> List.rev_map (fun lc -> LC.apply_subst ~renaming subst (lc,sc))
  |> make

let to_list (t:t) : _ list = t
let to_seq = Sequence.of_list

let pp = Hornet_types_util.pp_label

let to_string = Fmt.to_string pp


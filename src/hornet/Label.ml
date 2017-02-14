
(* This file is free software, part of Zipperposition. See file "license" for more details. *)

(** {1 Label for Horn Clauses} *)

open Hornet_types

module Fmt = CCFormat
module LC = Labelled_clause

type t = labelled_clause list
(** Set of labelled clauses. Invariant: sorted *)

let return l : t = [l]

let make l = CCList.sort_uniq ~cmp:LC.compare l

let merge = CCList.sorted_merge ~cmp:LC.compare

let apply_subst ~renaming subst (l,sc) =
  l
  |> List.rev_map (fun lc -> LC.apply_subst ~renaming subst (lc,sc))
  |> make

let pp out (l:t): unit =
  Fmt.fprintf out "{@[<hv>%a@]}" (Fmt.list LC.pp) l

let to_string = Fmt.to_string pp


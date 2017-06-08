
(* This file is free software, part of Zipperposition. See file "license" for more details. *)

(** {1 Higher-Order Unification} *)

module RW = Rewrite
module T = Term

let stat_unif_calls = Util.mk_stat "ho_unif.calls"
let stat_unif_steps = Util.mk_stat "ho_unif.steps"
let section = Util.Section.make "ho_unif"

type term = Term.t

type penalty = int
(** penalty on the search space *)

(* number of ty and non-ty arguments *)
let term_arity args =
  args
  |> Util.take_drop_while (fun t -> T.is_type t)
  |> CCPair.map List.length List.length

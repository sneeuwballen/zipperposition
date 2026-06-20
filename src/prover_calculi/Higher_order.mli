(* This file is free software, part of Zipperposition. See file "license" for more details. *)

open Logtk
(** {1 HO} *)

open Libzipperposition

val prim_enum_terms : Term.Set.t ref

type prune_kind =
  [ `NoPrune
  | `OldPrune
  | `PruneAllCovers
  | `PruneMaxCover
  ]

val k_prune_arg_fun : prune_kind Flex_state.key

(* diff const is of type ![alpha,beta]: (alpha->beta) -> (alpha->beta) -> alpha
   -- NB: EXPECTS TYPE ARGUMENTS! *)
val k_diff_const : Term.t Flex_state.key
val setup : Env.t -> unit
val prim_enum_tf : Env.t -> Clause.t -> Clause.t list
val extension : Extensions.t

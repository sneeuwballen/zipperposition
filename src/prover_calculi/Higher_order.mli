
(* This file is free software, part of Zipperposition. See file "license" for more details. *)

(** {1 HO} *)
open Logtk
open Libzipperposition

val prim_enum_terms : Term.Set.t ref

type prune_kind = [`NoPrune | `OldPrune | `PruneAllCovers | `PruneMaxCover]
val k_prune_arg_fun : prune_kind Flex_state.key
(* diff const is of type ![alpha,beta]: (alpha->beta) -> (alpha->beta) -> alpha
   -- NB: EXPECTS TYPE ARGUMENTS! *)
val k_diff_const : Term.t Flex_state.key


module type S = sig
  module Env : Env.S
  module C : module type of Env.C

  (** {5 Registration} *)

  val setup : unit -> unit
  (** Register rules in the environment *)

  val prim_enum_tf : Env.C.t -> Env.C.t list

end

module Make(E : Env.S) : S with module Env = E

(** {2 As Extension} *)

val extension : Extensions.t


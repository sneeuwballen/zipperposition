
(* This file is free software, part of Zipperposition. See file "license" for more details. *)

(** {1 State} *)

(** The global state of a proof attempt *)

type t

val create :
  conf:Flex_state.t ->
  ord:Ordering.t ->
  unit ->
  t

val conf : t -> Flex_state.t
val ord : t -> Ordering.t


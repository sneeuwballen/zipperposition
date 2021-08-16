
(* This file is free software, part of Zipperposition. See file "license" for more details. *)

(** {1 Priority Queue of ho-streams} *)

(** Heuristic selection of ho-streams, using a priority queue.
    Only one queue is used, but the priority of a stream is determined by
    a combination of criteria that {b should} include at least one fair
    criterion (e.g. the age of the clause, so that older clauses are more
    likely to be chosen). *)

open Logtk

module type S = StreamQueue_intf.S

module type ARG = sig
  module Stm : Stream.S
  val state: unit -> Flex_state.t
end

val k_guard : int Flex_state.key
val k_ratio : int Flex_state.key
val k_clause_num : int Flex_state.key


module Make(A : ARG) : S with module Stm = A.Stm

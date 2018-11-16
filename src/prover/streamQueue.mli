
(* This file is free software, part of Zipperposition. See file "license" for more details. *)

(** {1 Priority Queue of ho-streams} *)

(** Heuristic selection of ho-streams, using a priority queue.
    Only one queue is used, but the priority of a stream is determined by
    a combination of criteria that {b should} include at least one fair
    criterion (e.g. the age of the clause, so that older clauses are more
    likely to be chosen). *)

module type S = StreamQueue_intf.S

module Make(Stm : Stream.S) : S with module Stm = Stm

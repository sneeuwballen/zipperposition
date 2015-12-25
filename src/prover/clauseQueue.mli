
(* This file is free software, part of Zipperposition. See file "license" for more details. *)

(** {1 Priority Queue of clauses} *)

(** Heuristic selection of clauses, using queues. Note that some
    queues do not need accept all clauses, as long as one of them does
    (for completeness). Anyway, a fifo queue should always be present,
    and presents this property. *)

open Libzipperposition

val profile : unit -> string
val set_profile : string -> unit

module type S = ClauseQueue_intf.S

module Make(C : Clause.S) : S with module C = C

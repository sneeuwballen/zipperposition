
(* This file is free software, part of Zipperposition. See file "license" for more details. *)

(** {1 Priority Queue of clauses} *)

(** Heuristic selection of clauses, using a priority queue.
    Only one queue is used, but the priority of a clause is determined by
    a combination of criteria that {b should} include at least one fair
    criterion (e.g. the age of the clause, so that older clauses are more
    likely to be chosen). *)

type profile = ClauseQueue_intf.profile

val profile_of_string : string -> profile
(** @raise Invalid_argument if the string is not recognized *)

val get_profile : unit -> profile
val set_profile : profile -> unit

module type S = ClauseQueue_intf.S

module Make(C : Clause.S) : S with module C = C

(* This file is free software, part of Zipperposition. See file "license" for more details. *)

(** {1 Priority Queue of clauses} *)

(** Heuristic selection of clauses, using a priority queue. Only one queue is
    used, but the priority of a clause is determined by a combination of
    criteria that {b should} include at least one fair criterion (e.g. the age
    of the clause, so that older clauses are more likely to be chosen). *)

type profile = ClauseQueue_intf.profile

val profile_of_string : string -> profile
(** @raise Invalid_argument if the string is not recognized *)

val get_profile : unit -> profile
val set_profile : profile -> unit
val ignoring_orphans : unit -> bool
val disable_ignoring_orphans : unit -> unit

(** {1 A priority queue of clauses, purely functional} *)

val register_conjecture_clause : Clause.t -> unit
val on_proof_state_init : Clause.t Iter.t Logtk.Signal.t

(** {6 Weight functions} *)
module WeightFun : sig
  type t = Clause.t -> int

  val of_string : string -> t
  val default : t
  val penalty : t
  val favor_all_neg : t
  val favor_non_all_neg : t
  val favor_ground : t
  val favor_horn : t
  val favor_goal : t

  val conj_relative :
    ?distinct_vars_mul:float ->
    ?parameters_magnitude:[< `Large | `Small > `Large ] ->
    ?goal_penalty:bool ->
    t

  val combine : (t * int) list -> t
end

module PriorityFun : sig
  type t = Clause.t -> int

  val of_string : string -> t
end

type t

val add : t -> Clause.t -> bool
val add_seq : t -> Clause.t Iter.t -> unit
val length : t -> int
val is_empty : t -> bool
val take_first : t -> Clause.t
val name : t -> string
val bfs : unit -> t
val almost_bfs : unit -> t
val explore : unit -> t
val ground : unit -> t
val goal_oriented : unit -> t
val default : unit -> t
val of_profile : profile -> t
val all_clauses : t -> Clause.t Iter.t
val mem_cl : t -> Clause.t -> bool
val remove : t -> Clause.t -> bool
val pp : t CCFormat.printer
val to_string : t -> string

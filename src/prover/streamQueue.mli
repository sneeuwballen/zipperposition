(* This file is free software, part of Zipperposition. See file "license" for more details. *)

(** {1 Priority Queue of ho-streams} *)

(** Heuristic selection of ho-streams, using a priority queue. Only one queue is
    used, but the priority of a stream is determined by a combination of
    criteria that {b should} include at least one fair criterion (e.g. the age
    of the clause, so that older clauses are more likely to be chosen). *)

open Logtk

module type S = StreamQueue_intf.S

val k_guard : int Flex_state.key
val k_ratio : int Flex_state.key
val k_clause_num : int Flex_state.key

(* StreamQueue is now concrete; Make removed *)

type t

module WeightFun : sig
  type t = Stream.t -> int

  val penalty : t
  val combine : (t * int) list -> t
end

val make : guard:int -> ratio:int -> weight:(Stream.t -> int) -> string -> t
val is_empty : t -> bool
val length : t -> int
val add : t -> Stream.t -> unit
val add_lst : t -> Stream.t list -> unit
val take_first : t -> Clause.t option
val take_fair_anyway : t -> Clause.t option list
val take_stm_nb : t -> Clause.t option list
val take_stm_nb_fix_stm : t -> Clause.t option list
val name : t -> string
val default : unit -> t
val pp : t CCFormat.printer
val to_string : t -> string

(* heuristic selection of clauses *)

open Types

(* a queue of clauses, as a pure object *)
class type queue =
  object
    method add : hclause -> queue
    method is_empty: bool
    method take_first : (queue * hclause)
    method remove : hclause list -> queue  (* slow *)
    method name : string
  end

(* select by increasing age (for fairness) *)
val fifo : ord:ordering -> queue

(* select by increasing weight of clause *)
val clause_weight : ord:ordering -> queue

(* default combination of heuristics *)
val default_queues : ord:ordering -> (queue * int) list

val pp_queue : Format.formatter -> queue -> unit
val pp_queues : Format.formatter -> (queue * int) list -> unit


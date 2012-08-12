(* heuristic selection of clauses *)

open Types

(* a queue of clauses, as a pure object *)
class type queue =
  object
    method add : hclause -> queue
    method is_empty: bool
    method take_first : (queue * hclause)
  end

(* select by increasing age (for fairness) *)
class fifo : queue

(* select by increasing weight of clause *)
class clause_weight : queue

(* default combination of heuristics *)
val default_queues : (queue * int) list

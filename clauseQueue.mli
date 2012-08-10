(* heuristic selection of clauses *)

(* a queue of clauses, as a pure object *)
class type queue =
  object
    method add : Terms.clause -> queue
    method is_empty: bool
    method take_first : (queue * Terms.clause)
  end

(* select by increasing age (for fairness) *)
class fifo : queue

(* select by increasing weight of clause *)
class clause_weight : queue

(* default combination of heuristics *)
val default_queues : (queue * int) list

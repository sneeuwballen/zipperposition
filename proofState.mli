(* the state of a proof *)

(* set of active clauses *)
type active_set = {
  active_clauses : Terms.bag;
  idx : Index.t;
}

(* set of passive clauses *)
type passive_set = {
  queues : (ClauseQueue.queue * int) list;
}

(* state of a superposition calculus instance.
   It contains a set of active clauses, a set of passive clauses,
   and is parametrized by an ordering. *)
type state = {
  ord : Orderings.ordering;
  active_set : active_set;  (* active clauses, indexed *)
  passive_set : passive_set; (* passive clauses *)
}

(* create a state from the given ordering *)
val make_state : Orderings.ordering -> (ClauseQueue.queue * int) list -> state

(* parse CLI arguments to create a state *)
val parse_args : unit -> state

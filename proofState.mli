(* the state of a proof *)

open Types

(** set of active clauses *)
type active_set = {
  a_ord : ordering;
  active_clauses : Clauses.bag;
  idx : Index.t;
}

(** set of passive clauses *)
type passive_set = {
  p_ord : ordering;
  passive_clauses : Clauses.bag;
  queues : (ClauseQueue.queue * int) list;
  queue_state : int * int;  (** position in the queue/weight *)
}

(** state of a superposition calculus instance.
    It contains a set of active clauses, a set of passive clauses,
    and is parametrized by an ordering. *)
type state = {
  ord : ordering;
  active_set : active_set;    (* active clauses, indexed *)
  passive_set : passive_set;  (* passive clauses *)
}

(** create a state from the given ordering *)
val make_state : ordering -> (ClauseQueue.queue * int) list -> state

(** add clauses to the active set *)
val add_active : active_set -> clause -> active_set * hclause
val add_actives : active_set -> clause list -> active_set

(** add clauses to the passive set *)
val add_passive : passive_set -> clause -> passive_set * hclause
val add_passives : passive_set -> clause list -> passive_set

(** pop the next passive clause, if any *)
val next_passive_clause : passive_set -> (passive_set * hclause option)

(* parse CLI arguments to create a state (TODO move somewhere else) *)
val parse_args : unit -> state

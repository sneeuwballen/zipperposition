(* the state of a proof *)

open Types

module I = Index
module C = Clauses

(** set of active clauses *)
type active_set = {
  a_ord : ordering;
  active_clauses : C.bag;
  idx : Index.t;
}

(** set of passive clauses *)
type passive_set = {
  p_ord : ordering;
  passive_clauses : C.bag;
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

let make_state ord queue_list =
  let passive_set = {p_ord=ord; passive_clauses=C.empty_bag;
                     queues=queue_list; queue_state=(0,0)}
  and active_set = {a_ord=ord; active_clauses=C.empty_bag; idx=I.empty} in
  {ord=ord; active_set=active_set;
   passive_set=passive_set}

let next_clause state = (state, None)  (* TODO *)

(* hashtable string -> ordering module *)
let ords = Hashtbl.create 7
let _ =
  Hashtbl.add ords "lpo" (new Orderings.lpo);
  Hashtbl.add ords "kbo" (new Orderings.kbo);
  Hashtbl.add ords "nrkbo" (new Orderings.nrkbo)

let ord = ref Orderings.default
let set_ord s = (* select ordering *)
  try
    ord := Hashtbl.find ords s
  with
    Not_found -> Printf.printf "unknown ordering: %s\n" s

let options =
  [ ("-ord", Arg.String set_ord, "choose ordering (lpo,kbo,nrkbo)") ]
  (* TODO parse something about heuristics *)
let args_fun s = ()

let parse_args () =
  Arg.parse options args_fun "solve problem in first file";
  make_state !ord ClauseQueue.default_queues


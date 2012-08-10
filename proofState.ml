(* the state of a proof *)

module T = Terms
module I = Index

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

(* create a state *)
let make_state ord queue_list =
  let passive_set = {queues=queue_list}
  and active_set = {active_clauses=T.empty_bag; idx=I.empty} in
  {ord=ord; active_set=active_set;
   passive_set=passive_set}

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


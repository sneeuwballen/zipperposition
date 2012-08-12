(* the state of a proof *)

open Types
open Hashcons

module I = Index
module C = Clauses
module CQ = ClauseQueue
module U = FoUtils

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

let next_passive_clause passive_set =
  (* index of the first queue to consider *)
  let first_idx, first_weight = passive_set.queue_state
  and len = List.length passive_set.queues
  and queues = passive_set.queues in
  (* try to get one from the idx-th queue *)
  let rec try_queue idx weight =
    assert (idx < len);
    let q, w = U.list_get queues idx in
    if weight >= w || q#is_empty
      then next_idx (idx+1) (* queue has been used enough time, or is empty *)
      else 
        let new_q, hc = q#take_first in (* pop from this queue *)
        let new_q_state = (idx, weight+1) (* increment weight *)
        and new_clauses = C.remove_from_bag passive_set.passive_clauses hc.hkey in
        {passive_set with passive_clauses=new_clauses;
                          queues=U.list_set queues idx (new_q, w);
                          queue_state=new_q_state}, Some hc
  (* lookup for a non-empty queue to pop *)
  and next_idx idx = 
    if idx >= len
      then try_queue 0 0  (* back to the first queue *)
      else if idx = first_idx
      then (passive_set, None)  (* ran through all queues *)
      else try_queue (idx+1) 0
  (* start at current index and weight *)
  in try_queue first_idx first_weight

let add_active active_set c =
  let new_bag, hc = C.add_to_bag active_set.active_clauses c in
  let new_idx = I.index_clause active_set.idx hc in
  {active_set with active_clauses=new_bag; idx=new_idx}, hc

let add_actives active_set l =
  List.fold_left (fun b c -> fst (add_active b c)) active_set l
  
let add_passive passive_set c =
  let new_bag, hc = C.add_to_bag passive_set.passive_clauses c in
  let new_queues = List.map
    (fun (q,weight) -> q#add hc, weight)
    passive_set.queues in
  {passive_set with passive_clauses=new_bag; queues=new_queues}, hc

let add_passives passive_set l =
  List.fold_left (fun b c -> fst (add_passive b c)) passive_set l

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


(*
Zipperposition: a functional superposition prover for prototyping
Copyright (C) 2012 Simon Cruanes

This is free software; you can redistribute it and/or
modify it under the terms of the GNU General Public License
as published by the Free Software Foundation; either version 2
of the License, or (at your option) any later version.

This is distributed in the hope that it will be useful,
but WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
GNU General Public License for more details.

You should have received a copy of the GNU General Public License
along with this program; if not, write to the Free Software
Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA
02110-1301 USA.
*)

(* the state of a proof *)

open Types

module I = Index
module FV = FeatureVector
module C = Clauses
module U = FoUtils
module CQ = ClauseQueue
module CD = ClauseDag

(** Default indexing on terms *)
let cur_index =
  ref (I.mk_clause_index (Fingerprint.mk_index Fingerprint.fp6m))
  (* ref (I.mk_clause_index Discrimination_tree.index) *)

let _indexes =
  let table = Hashtbl.create 11 in
  Hashtbl.add table "discr_tree" Discrimination_tree.index;
  Hashtbl.add table "fp" (Fingerprint.mk_index Fingerprint.fp6m);
  table

let choose_index name =
  try Hashtbl.find _indexes name
  with Not_found -> failwith ("no such index name: " ^ name)

let names_index () =
  let names = ref [] in
  Hashtbl.iter (fun n _ -> names := n :: !names) _indexes;
  !names

(** set of active clauses *)
type active_set = {
  a_ord : ordering;
  active_clauses : Clauses.bag;       (** set of active clauses *)
  idx : Index.clause_index;           (** term index *)
  fv_idx : FeatureVector.fv_index;    (** feature index, for subsumption *)
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
  state_select : selection_fun;
  active_set : active_set;      (** active clauses, indexed *)
  axioms_set : active_set;      (** set of support, indexed *)
  passive_set : passive_set;    (** passive clauses *)
  dag : CD.clause_dag;          (** DAG of clauses *)
}

let mk_active_set ~ord =
  let signature = ord#symbol_ordering#signature in
  (* feature vector index *)
  let fv_idx = FV.mk_fv_index_signature signature in
  {a_ord=ord; active_clauses=C.empty_bag; idx= !cur_index; fv_idx=fv_idx}

let make_state ord queue_list select =
  let passive_set = {p_ord=ord; passive_clauses=C.empty_bag;
                     queues=queue_list; queue_state=(0,0)}
  and active_set = mk_active_set ~ord in
  {ord=ord;
   state_select=select;
   active_set=active_set;
   axioms_set=active_set;
   passive_set=passive_set;
   dag=CD.empty;}

let next_passive_clause passive_set =
  (* index of the first queue to consider *)
  let first_idx, first_weight = passive_set.queue_state
  and len = List.length passive_set.queues in
  assert (len > 0);
  (* try to get one from the idx-th queue *)
  let rec try_queue passive_set idx weight =
    assert (idx < len);
    let queues = passive_set.queues in
    let q, w = U.list_get queues idx in
    if weight >= w || q#is_empty
      then next_idx passive_set (idx+1) (* queue has been used enough time, or is empty *)
      else 
        let new_q, hc = q#take_first in (* pop from this queue *)
        if C.is_in_bag passive_set.passive_clauses hc.ctag
          then (* the clause is still in the passive set, return it *)
            let new_q_state = (idx, weight+1) (* increment weight for the clause *)
            and new_clauses = C.remove_from_bag passive_set.passive_clauses hc.ctag in
            U.debug 3 (lazy (U.sprintf "taken clause from %s" q#name));
            let passive_set = {passive_set with passive_clauses=new_clauses;
                queues=U.list_set queues idx (new_q, w);
                queue_state=new_q_state}
            in
            passive_set, Some hc
          else (* we must find another clause, this one has already been pop'd *)
            let passive_set = {passive_set with queues=U.list_set queues idx (new_q, w)} in
            try_queue passive_set idx weight (* try again *)
  (* lookup for a non-empty queue to pop *)
  and next_idx passive_set idx = 
    if idx >= len
      then next_idx passive_set 0 (* back to the first queue *)
      else if idx = first_idx
      then (passive_set, None)  (* ran through all queues, stop *)
      else try_queue passive_set idx 0
  (* start at current index and weight *)
  in try_queue passive_set first_idx first_weight

let add_active active_set c =
  let hc = C.hashcons_clause c in
  if C.is_in_bag active_set.active_clauses hc.ctag
    then active_set, hc  (* already in active set *)
    else
      let new_bag = C.add_hc_to_bag active_set.active_clauses hc
      and new_idx = active_set.idx#index_clause hc
      and new_fv_idx = FV.index_clause active_set.fv_idx hc in
      {active_set with active_clauses=new_bag; idx=new_idx; fv_idx=new_fv_idx}, hc

let add_actives active_set l =
  List.fold_left (fun b sc -> fst (add_active b sc)) active_set l

let remove_active active_set hc =
  if C.is_in_bag active_set.active_clauses hc.ctag
    then
      let new_bag = C.remove_from_bag active_set.active_clauses hc.ctag
      and new_idx = active_set.idx#remove_clause hc
      and new_fv_idx = FV.remove_clause active_set.fv_idx hc in
      {active_set with active_clauses=new_bag; idx=new_idx; fv_idx=new_fv_idx}
    else
      active_set

let remove_actives active_set l =
  List.fold_left remove_active active_set l

let remove_active_bag active_set bag =
  let active = ref active_set in
  C.iter_bag bag (fun _ hc -> active := remove_active !active hc);
  !active

let singleton_active_set ~ord clause =
  let active_set = mk_active_set ~ord in
  let active_set, _ = add_active active_set clause in
  active_set
  
let add_passive passive_set c =
  let hc = C.hashcons_clause c in
  if C.is_in_bag passive_set.passive_clauses hc.ctag
    then passive_set, hc  (* already in passive set *)
    else
      let new_bag = C.add_hc_to_bag passive_set.passive_clauses hc in
      let new_queues = List.map
        (fun (q,weight) -> q#add hc, weight)
        passive_set.queues in
      {passive_set with passive_clauses=new_bag; queues=new_queues}, hc

let add_passives passive_set l =
  List.fold_left (fun b c -> fst (add_passive b c)) passive_set l

let remove_passive passive_set c =
  let hc = C.hashcons_clause_noselect c in
  (* just remove from the set of passive clauses *)
  let new_passive_clauses = 
    C.remove_from_bag passive_set.passive_clauses hc.ctag in
  {passive_set with passive_clauses = new_passive_clauses}

let remove_passives passive_set l =
  List.fold_left (fun set c -> remove_passive set c) passive_set l

let relocate_active active_set c =
  let maxvar = active_set.active_clauses.C.bag_maxvar in
  fst (C.fresh_clause ~ord:active_set.a_ord (maxvar+1) c)

(** statistics on the state *)
type state_stats = {
  stats_active_clauses : int;
  stats_sos_clauses: int;
  stats_passive_clauses : int;
}

let stats state =
  {
    stats_active_clauses = C.size_bag state.active_set.active_clauses;
    stats_sos_clauses = C.size_bag state.axioms_set.active_clauses;
    stats_passive_clauses = C.size_bag state.passive_set.passive_clauses;
  }

let pp_state formatter state =
  Format.fprintf formatter "@[<h>state {%d active clauses; %d passive_clauses;@;%a}@]"
    (C.size_bag state.active_set.active_clauses)
    (C.size_bag state.passive_set.passive_clauses)
    CQ.pp_queues state.passive_set.queues

let debug_state formatter state =
  Format.fprintf formatter
    "@[<v 2>state {%d active clauses; %d passive_clauses;@;%a@;active:%a@;passive:%a@]@;"
    (C.size_bag state.active_set.active_clauses)
    (C.size_bag state.passive_set.passive_clauses)
    CQ.pp_queues state.passive_set.queues
    C.pp_bag state.active_set.active_clauses
    C.pp_bag state.passive_set.passive_clauses


module DotState = Dot.Make(
  struct
    type vertex = clause
    type edge = string

    let equal = C.eq_clause
    let hash = C.hash_clause
    let print_vertex c = U.sprintf "@[<h>%a@]" !C.pp_clause#pp c
    let print_edge s = s
  end)

(** print to dot *)
let pp_dot ?(name="state") formatter state =
  let graph = DotState.mk_graph ~name
  and explored = C.CHashSet.create ()
  and queue = Queue.create () in
  C.iter_bag state.active_set.active_clauses (fun _ c -> Queue.push c queue);
  (* breadth first exploration of clauses and their parents *)
  while not (Queue.is_empty queue) do
    let c = Queue.pop queue in
    if C.CHashSet.member explored c then ()
    else begin
      (* node for this clause *)
      let n = DotState.get_node graph c in
      C.CHashSet.add explored c;
      DotState.add_node_attribute n (DotState.Style "filled");
      DotState.add_node_attribute n (DotState.Shape "box");
      (if c.clits = [] then DotState.add_node_attribute n (DotState.Color "red"));
      match Lazy.force c.cproof with
      | Axiom (file, axiom) ->
        DotState.add_node_attribute n (DotState.Color "yellow");
      | Proof (rule, clauses) ->
        List.iter
          (fun (parent, _, _) ->
            Queue.push parent queue;  (* explore this parent *)
            let n_parent = DotState.get_node graph parent in
            ignore (DotState.add_edge graph n_parent n rule))
          clauses
    end
  done;
  DotState.pp_graph formatter graph

(** print to dot into a file *)
let pp_dot_file filename state =
  let out = open_out filename in
  try
    (* write on the opened out channel *)
    let formatter = Format.formatter_of_out_channel out in
    Format.printf "%% print state to %s@." filename;
    pp_dot formatter state;
    close_out out
  with _ -> close_out out

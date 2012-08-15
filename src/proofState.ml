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
        if C.is_in_bag passive_set.passive_clauses hc.tag
          then (* the clause is still in the passive set, return it *)
            let new_q_state = (idx, weight+1) (* increment weight for the clause *)
            and new_clauses = C.remove_from_bag passive_set.passive_clauses hc.tag in
            {passive_set with passive_clauses=new_clauses;
                              queues=U.list_set queues idx (new_q, w);
                              queue_state=new_q_state}, Some hc
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

let relocate_active active_set c =
  let maxvar = active_set.active_clauses.C.bag_maxvar in
  fst (C.fresh_clause ~ord:active_set.a_ord maxvar c)

(** statistics on the state *)
type state_stats = {
  stats_active_clauses : int;
  stats_passive_clauses : int;
  stats_root_index_keys : int;
  stats_root_index_elems : int;
  stats_subterm_index_keys : int;
  stats_subterm_index_elems : int;
  stats_unit_root_index_keys : int;
  stats_unit_root_index_elems : int;
}

let stats state =
  {
    stats_active_clauses = C.size_bag state.active_set.active_clauses;
    stats_passive_clauses = C.size_bag state.passive_set.passive_clauses;
    stats_root_index_keys = I.DT.num_keys state.active_set.idx.I.root_index;
    stats_root_index_elems = I.DT.num_elems state.active_set.idx.I.root_index;
    stats_subterm_index_keys = I.DT.num_keys state.active_set.idx.I.subterm_index;
    stats_subterm_index_elems = I.DT.num_elems state.active_set.idx.I.subterm_index;
    stats_unit_root_index_keys = I.DT.num_keys state.active_set.idx.I.unit_root_index;
    stats_unit_root_index_elems = I.DT.num_elems state.active_set.idx.I.unit_root_index;
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

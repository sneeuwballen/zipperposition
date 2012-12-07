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

let prof_add_passive = HExtlib.profile ~enable:true "add_passives"
let prof_next_passive = HExtlib.profile ~enable:true "next_passive"

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

(* ----------------------------------------------------------------------
 * main definitions
 * ---------------------------------------------------------------------- *)

(** set of active clauses *)
type active_set = {
  a_ord : ordering;
  active_clauses : C.CSet.t;          (** set of active clauses *)
  idx : Index.clause_index;           (** term index *)
  fv_idx : FeatureVector.fv_index;    (** feature index, for subsumption *)
}

(** set of passive clauses *)
type passive_set = {
  p_ord : ordering;
  passive_clauses : C.CSet.t;
  queues : (ClauseQueue.queue * int) list;
  queue_state : int * int;  (** position in the queue/weight *)
}

(** state of a superposition calculus instance.
    It contains a set of active clauses, a set of passive clauses,
    and is parametrized by an ordering. *)
type state = {
  ord : ordering;
  state_select : selection_fun;
  state_index : Index.unit_index; (** index used for unit simplification *)
  active_set : active_set;        (** active clauses, indexed *)
  passive_set : passive_set;      (** passive clauses *)
}

let mk_active_set ~ord =
  let signature = ord#symbol_ordering#signature in
  (* feature vector index *)
  let fv_idx = FV.mk_fv_index_signature signature in
  {a_ord=ord; active_clauses=C.CSet.empty; idx= !cur_index; fv_idx=fv_idx}

let make_state ord queue_list select unit_index =
  let passive_set = {p_ord=ord; passive_clauses=C.CSet.empty;
                     queues=queue_list; queue_state=(0,0)}
  and active_set = mk_active_set ~ord in
  {ord=ord;
   state_select=select;
   state_index = unit_index;
   active_set=active_set;
   passive_set=passive_set; }

(* ----------------------------------------------------------------------
 * simplification rules (unit clauses)
 * ---------------------------------------------------------------------- *)

let add_rule state hc =
  if C.is_unit_clause hc
    then {state with state_index= state.state_index#add_clause hc}
    else state

let add_rules state hcs = List.fold_left add_rule state hcs

let remove_rule state hc =
  if C.is_unit_clause hc
    then {state with state_index= state.state_index#remove_clause hc}
    else state

let remove_rules state hcs = List.fold_left remove_rule state hcs

(* ----------------------------------------------------------------------
 * selection of next active
 * ---------------------------------------------------------------------- *)

let next_passive_clause_ passive_set =
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
        if C.CSet.mem passive_set.passive_clauses hc
          then (* the clause is still in the passive set, return it *)
            let new_q_state = (idx, weight+1) (* increment weight for the clause *)
            and new_clauses = C.CSet.remove passive_set.passive_clauses hc in
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

let next_passive_clause = prof_next_passive.HExtlib.profile next_passive_clause_

(* ----------------------------------------------------------------------
 * active set
 * ---------------------------------------------------------------------- *)

let add_active active_set hc =
  if C.CSet.mem active_set.active_clauses hc
    then active_set  (* already in active set *)
    else
      let new_set = C.CSet.add active_set.active_clauses hc
      and new_idx = active_set.idx#index_clause hc
      and new_fv_idx = FV.index_clause active_set.fv_idx hc in
      {active_set with active_clauses=new_set; idx=new_idx; fv_idx=new_fv_idx}

let add_actives active_set l =
  List.fold_left (fun b sc -> add_active b sc) active_set l

let relocate_active active_set hc =
  let ord = active_set.a_ord
  and maxvar = active_set.active_clauses.C.CSet.maxvar in
  C.fresh_clause ~ord maxvar hc

let relocate_rules ~ord idx hc =
  C.fresh_clause ~ord idx#maxvar hc

let remove_active active_set hc =
  if C.CSet.mem active_set.active_clauses hc
    then
      let new_set = C.CSet.remove active_set.active_clauses hc
      and new_idx = active_set.idx#remove_clause hc
      and new_fv_idx = FV.remove_clause active_set.fv_idx hc in
      {active_set with active_clauses=new_set; idx=new_idx; fv_idx=new_fv_idx}
    else
      active_set

let remove_actives active_set l =
  List.fold_left remove_active active_set l

let remove_active_set active_set set =
  let active = ref active_set in
  C.CSet.iter set (fun hc -> active := remove_active !active hc);
  !active

let singleton_active_set ~ord clause =
  let active_set = mk_active_set ~ord in
  let active_set = add_active active_set clause in
  active_set

(* ----------------------------------------------------------------------
 * passive set
 * ---------------------------------------------------------------------- *)
  
let add_passive_ passive_set hc =
  if C.CSet.mem passive_set.passive_clauses hc
    then passive_set  (* already in passive set *)
    else
      let new_set = C.CSet.add passive_set.passive_clauses hc in
      let new_queues = List.map
        (fun (q,weight) -> q#add hc, weight)
        passive_set.queues in
      {passive_set with passive_clauses=new_set; queues=new_queues}

let add_passive = prof_add_passive.HExtlib.profile add_passive_

let add_passives passive_set l =
  List.fold_left (fun b c -> add_passive b c) passive_set l

let remove_passive passive_set hc =
  (* just remove from the set of passive clauses *)
  let new_passive_clauses = 
    C.CSet.remove passive_set.passive_clauses hc in
  {passive_set with passive_clauses = new_passive_clauses}

let remove_passives passive_set l =
  List.fold_left (fun set c -> remove_passive set c) passive_set l

let remove_passives_set passive_set s =
  let passive_clauses =
    Ptset.fold (fun id clauses -> C.CSet.remove_id clauses id)
      s passive_set.passive_clauses
  in {passive_set with passive_clauses;}

let clean_passive passive_set =
  (* remove all clauses that are no longer in this set from queues *)
  let set = passive_set.passive_clauses in
  let queues =
    List.map (fun (q, w) -> (q#clean set, w)) passive_set.queues in
  {passive_set with queues;}

(* ----------------------------------------------------------------------
 * utils
 * ---------------------------------------------------------------------- *)

let maxvar_active active_set = active_set.active_clauses.C.CSet.maxvar

(** statistics on the state *)
type state_stats = int * int (* num passive, num active *)

let stats state =
  ( C.CSet.size state.active_set.active_clauses
  , C.CSet.size state.passive_set.passive_clauses)

let pp_state formatter state =
  Format.fprintf formatter "@[<h>state {%d active clauses; %d passive_clauses;@;%a}@]"
    (C.CSet.size state.active_set.active_clauses)
    (C.CSet.size state.passive_set.passive_clauses)
    CQ.pp_queues state.passive_set.queues

let debug_state formatter state =
  Format.fprintf formatter
    "@[<v 2>state {%d active clauses; %d passive_clauses;@;%a@;active:%a@;passive:%a@]@;"
    (C.CSet.size state.active_set.active_clauses)
    (C.CSet.size state.passive_set.passive_clauses)
    CQ.pp_queues state.passive_set.queues
    C.pp_set state.active_set.active_clauses
    C.pp_set state.passive_set.passive_clauses


module DotState = Dot.Make(
  struct
    type vertex = hclause
    type edge = string

    let equal = C.eq_hclause
    let hash hc = C.hash_hclause hc
    let print_vertex hc = U.sprintf "@[<h>%a@]" !C.pp_clause#pp_h hc
    let print_edge s = s
  end)

(** print to dot (if empty clause is present, only print a proof,
    otherwise print the active set and its proof) *)
let pp_dot ?(name="state") formatter state =
  let graph = DotState.mk_graph ~name
  and explored = ref C.CSet.empty
  and queue = Queue.create () in
  (* start from empty clause if present, all active clauses otherwise *)
  let empty_clause = ref None in
  try C.CSet.iter state.active_set.active_clauses
    (fun hc' -> if hc'.hclits = [||] then (empty_clause := Some hc'; raise Exit));
  with Exit -> ();
  (match !empty_clause with
  | Some hc -> Queue.push hc queue
  | None ->
    C.CSet.iter state.active_set.active_clauses (fun hc -> Queue.push hc queue));
  (* breadth first exploration of clauses and their parents *)
  while not (Queue.is_empty queue) do
    let hc = Queue.pop queue in
    if C.CSet.mem !explored hc then ()
    else begin
      (* node for this clause *)
      let n = DotState.get_node graph hc in
      explored := C.CSet.add !explored hc;
      DotState.add_node_attribute n (DotState.Style "filled");
      DotState.add_node_attribute n (DotState.Shape "box");
      (if hc.hclits = [||] then DotState.add_node_attribute n (DotState.Color "red"));
      match Lazy.force hc.hcproof with
      | Axiom (file, axiom) ->
        DotState.add_node_attribute n (DotState.Color "yellow");
      | Proof (rule, clauses) ->
        List.iter
          (fun (parent, _, _) ->
            Queue.push parent.cref queue;  (* explore this parent *)
            let n_parent = DotState.get_node graph parent.cref in
            ignore (DotState.add_edge graph n_parent n rule))
          clauses
    end
  done;
  DotState.pp_graph formatter graph

(** print to dot into a file *)
let pp_dot_file ?name filename state =
  let out = open_out filename in
  try
    (* write on the opened out channel *)
    let formatter = Format.formatter_of_out_channel out in
    Format.printf "%% print state to %s@." filename;
    pp_dot ?name formatter state;
    close_out out
  with _ -> close_out out

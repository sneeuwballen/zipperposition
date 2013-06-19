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

(** Heuristic selection of clauses, using queues. Note that some
    queues do not need accept all clauses, as long as one of them does
    (for completeness). Anyway, a fifo queue should always be present,
    and presents this property. *)

open Basic

module C = Clauses
module O = Orderings
module Utils = FoUtils

(* TODO get rid of GPL implementation of heap *)
(** A heap that compares (int,clauses) by the int *)
module LH = Leftistheap.Make(
  struct
    type t = int * hclause
    let le (i, hci) (j, hcj) = i <= j || (i = j && hci.hctag <= hcj.hctag)
  end)

type t = {
  heap : LH.t;
  functions : functions;
} (** A priority queue of clauses, purely functional *)
and functions = {
  weight : hclause -> int;
  accept : hclause -> bool;
  name : string;
}

(** generic clause queue based on some ordering on clauses, given
    by a weight function *)
let mk_queue ?(accept=(fun _ -> true)) ~weight name =
  let functions = {
    weight;
    accept;
    name;
  } in
  { heap = LH.empty;
    functions;
  }

let is_empty q =
  LH.is_empty q.heap

let add q hc =
  if q.functions.accept hc
    then
      let w = q.functions.weight hc in
      let heap = LH.insert (w, hc) q.heap in
      { q with heap; }
    else q

let adds q hcs =
  let heap =
    Sequence.fold
      (fun heap hc ->
        if q.functions.accept hc
          then
            let w = q.functions.weight hc in
            LH.insert (w,hc) heap
          else heap)
      q.heap hcs in
  { q with heap; }

let take_first q =
  (if is_empty q then raise Not_found);
  let (_, hc), new_h = LH.extract_min q.heap in
  let q' = { q with heap=new_h; } in
  q', hc 

(** Keep only the clauses that are in the set *)
let clean q set =
  let new_heap = LH.filter q.heap (fun (_, hc) -> C.CSet.mem set hc) in
  { q with heap=new_heap; }

let name q = q.functions.name

let fifo =
  let name = "fifo_queue" in
  mk_queue ~weight:(fun hc -> hc.hctag) name

let clause_weight =
  let name = "clause_weight" in
  mk_queue ~weight:(fun hc -> hc.hcweight) name
  
let goals =
  (* check whether a literal is a goal *)
  let is_goal_lit lit = match lit with
  | Equation (_, _, sign, _) -> not sign in
  let is_goal_clause hc = Utils.array_forall is_goal_lit hc.hclits in
  let name = "prefer_goals" in
  mk_queue ~accept:is_goal_clause ~weight:(fun hc -> hc.hcweight) name

let ground =
  let is_ground hc = hc.hcvars = [] in
  let name = "prefer_ground" in
  mk_queue ~accept:is_ground ~weight:(fun hc -> hc.hcweight) name

let non_goals =
  (* check whether a literal is a goal *)
  let is_goal_lit lit = match lit with
  | Equation (_, _, sign, _) -> not sign in
  let is_non_goal_clause hc = Utils.array_forall (fun x -> not (is_goal_lit x)) hc.hclits in
  let name = "prefer_non_goals" in
  mk_queue ~accept:is_non_goal_clause ~weight:(fun hc -> hc.hcweight) name

let pos_unit_clauses =
  let is_unit_pos hc = match hc.hclits with
  | [|Equation (_,_,true,_)|] -> true
  | _ -> false
  in
  let name = "prefer_pos_unit_clauses" in
  mk_queue ~accept:is_unit_pos ~weight:(fun hc -> hc.hcweight) name

let horn =
  let name = "prefer_horn" in
  mk_queue ~accept:C.is_horn ~weight:(fun hc -> hc.hcweight) name

let lemmas =
  let name = "lemmas" in
  let accept hc = C.get_flag C.flag_lemma hc in
  (* use a fifo on lemmas *)
  mk_queue ~accept ~weight:(fun _ -> 1) name

let default_queues =
  [ fifo, 2;
    clause_weight, 2;
    lemmas, 1;
  ]
  (*
  [ (clause_weight, 4);
    (ground, 1);
    (*
    (non_goals, 1);
    (goals, 1);
    *)
    (fifo, 2);
    (horn, 1);  (* FIXME: if just before "fifo", incompleteness on pelletier_problems/pb64.p *)
    (lemmas, 1);
  ]
  *)

let pp_queue formatter q =
  Format.fprintf formatter "@[<h>queue %s@]" (name q)

let pp_queue_weight formatter (q, w) =
  Format.fprintf formatter "@[<h>queue %s, %d@]" (name q) w

let pp_queues formatter qs =
  Format.fprintf formatter "@[<hov>%a@]"
    (Sequence.pp_seq ~sep:"; " pp_queue_weight) qs


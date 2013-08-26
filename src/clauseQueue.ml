
(*
Zipperposition: a functional superposition prover for prototyping
Copyright (c) 2013, Simon Cruanes
All rights reserved.

Redistribution and use in source and binary forms, with or without
modification, are permitted provided that the following conditions are met:

Redistributions of source code must retain the above copyright notice, this
list of conditions and the following disclaimer.  Redistributions in binary
form must reproduce the above copyright notice, this list of conditions and the
following disclaimer in the documentation and/or other materials provided with
the distribution.

THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS "AS IS" AND
ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED
WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE ARE
DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT HOLDER OR CONTRIBUTORS BE LIABLE
FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL
DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR
SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER
CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY,
OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE
OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
*)

(** {1 Priority Queue of clauses} *)

(** Heuristic selection of clauses, using queues. Note that some
    queues do not need accept all clauses, as long as one of them does
    (for completeness). Anyway, a fifo queue should always be present,
    and presents this property. *)

open Logtk

module C = Clause
module O = Ordering

let empty_heap =
  let leq (i, hci) (j, hcj) = i <= j || (i = j && hci.C.hctag <= hcj.C.hctag) in
  Leftistheap.empty_with ~leq

type t = {
  heap : (int * Clause.t) Leftistheap.t;
  functions : functions;
} (** A priority queue of clauses, purely functional *)
and functions = {
  weight : Clause.t -> int;
  accept : Clause.t -> bool;
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
  { heap = empty_heap; functions; }

let is_empty q =
  Leftistheap.is_empty q.heap

let add q hc =
  if q.functions.accept hc
    then
      let w = q.functions.weight hc in
      let heap = Leftistheap.insert q.heap (w, hc) in
      { q with heap; }
    else q

let adds q hcs =
  let heap =
    Sequence.fold
      (fun heap hc ->
        if q.functions.accept hc
          then
            let w = q.functions.weight hc in
            Leftistheap.insert heap (w,hc)
          else heap)
      q.heap hcs in
  { q with heap; }

let take_first q =
  (if is_empty q then raise Not_found);
  let new_h, (_, hc) = Leftistheap.extract_min q.heap in
  let q' = { q with heap=new_h; } in
  q', hc 

(** Keep only the clauses that are in the set *)
let clean q set =
  let new_heap = Leftistheap.filter q.heap (fun (_, hc) -> C.CSet.mem set hc) in
  { q with heap=new_heap; }

let name q = q.functions.name

let fifo =
  let name = "fifo_queue" in
  mk_queue ~weight:(fun hc -> hc.C.hctag) name

let clause_weight =
  let name = "clause_weight" in
  mk_queue ~weight:(fun hc -> hc.C.hcweight) name
  
let goals =
  (* check whether a literal is a goal *)
  let is_goal_lit lit = match lit with
  | Literal.Equation (_, _, sign, _) -> not sign in
  let is_goal_clause hc = Util.array_forall is_goal_lit hc.C.hclits in
  let name = "prefer_goals" in
  mk_queue ~accept:is_goal_clause ~weight:(fun hc -> hc.C.hcweight) name

let ground =
  let is_ground hc = hc.C.hcvars = [] in
  let name = "prefer_ground" in
  mk_queue ~accept:is_ground ~weight:(fun hc -> hc.C.hcweight) name

let non_goals =
  (* check whether a literal is a goal *)
  let is_goal_lit lit = match lit with
  | Literal.Equation (_, _, sign, _) -> not sign in
  let is_non_goal_clause hc = Util.array_forall (fun x -> not (is_goal_lit x)) hc.C.hclits in
  let name = "prefer_non_goals" in
  mk_queue ~accept:is_non_goal_clause ~weight:(fun hc -> hc.C.hcweight) name

let pos_unit_clauses =
  let is_unit_pos hc = match hc.C.hclits with
  | [|Literal.Equation (_,_,true,_)|] -> true
  | _ -> false
  in
  let name = "prefer_pos_unit_clauses" in
  mk_queue ~accept:is_unit_pos ~weight:(fun hc -> hc.C.hcweight) name

let horn =
  let accept c = Literal.is_horn c.C.hclits in
  let name = "prefer_horn" in
  mk_queue ~accept ~weight:(fun hc -> hc.C.hcweight) name

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

let pp buf q =
  Printf.bprintf buf "queue %s" (name q)

let to_string q =
  let buf = Buffer.create 15 in
  pp buf q;
  Buffer.contents buf

let pp_list buf qs =
  let pp_pair buf (c, i) = Printf.bprintf buf "%a (w=%d)" pp c i in
  Buffer.add_char buf '[';
  Util.pp_list pp_pair buf qs;
  Buffer.add_char buf ']';
  ()

let fmt fmt q =
  Format.pp_print_string fmt (to_string q)

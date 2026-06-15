(* This file is free software, part of Zipperposition. See file "license" for more details. *)

(** {1 The state of a proof, contains a set of active clauses (processed), a set
    of passive clauses (to be processed), and an ordering that is used for
    redundancy elimination.} *)

open Logtk
module T = Term
module C = Clause
module S = Subst.FO
module Lit = Literal
module Lits = Literals
module Pos = Position
module PB = Position.Build
module CQ = ClauseQueue

module type S = ProofState_intf.S
(** {2 Set of active clauses} *)

(* module TermIndex = NPDtree.MakeTerm(Clause.WithPos) *)
module TermIndex = Fingerprint.Make (Clause.WithPos)

module UnitIndex =
(* NPDtree *)
Dtree.Make (struct
  type t = T.t * T.t * bool * Clause.t
  type rhs = T.t

  let compare (t11, t12, s1, c1) (t21, t22, s2, c2) =
    let open CCOrd.Infix in
    T.compare t11 t21 <?> (T.compare, t12, t22) <?> (compare, s1, s2)
    <?> (Clause.compare, c1, c2)

  let extract (t1, t2, sign, _) = t1, t2, sign

  let priority (_, _, _, c) =
    if Clause.is_oriented_rule c then
      2
    else
      1
end)

module SubsumptionIndex = FV_tree.Make (struct
  type t = Clause.t

  let compare = Clause.compare
  let to_lits c = Clause.to_forms c |> Iter.of_list
  let labels c = Clause.trail c |> Trail.labels
end)

(* XXX: no customization of indexing for now
     let _indexes =
     let table = Hashtbl.create 2 in
     let mk_fingerprint fp =
      Fingerprint.mk_index ~cmp:Clauses.compare_clause_pos fp in
     Hashtbl.add table "fp" (mk_fingerprint Fingerprint.fp6m);
     Hashtbl.add table "fp7m" (mk_fingerprint Fingerprint.fp7m);
     Hashtbl.add table "fp16" (mk_fingerprint Fingerprint.fp16);
     table
  *)

(** {5 Common Interface for Sets} *)

module type CLAUSE_SET = sig
  val on_add_clause : Clause.t Signal.t
  (** signal triggered when a clause is added to the set *)

  val on_remove_clause : Clause.t Signal.t
  (** signal triggered when a clause is removed from the set *)

  val add : Clause.t Iter.t -> unit
  (** Add clauses to the set *)

  val remove : Clause.t Iter.t -> unit
  (** Remove clauses from the set *)
end

module MakeClauseSet (X : sig end) = struct
  let clauses_ = ref Clause.ClauseSet.empty
  let on_add_clause = Signal.create ()
  let on_remove_clause = Signal.create ()
  let clauses () = !clauses_
  let num_clauses () = Clause.ClauseSet.cardinal !clauses_

  let add seq =
    seq (fun c ->
        if not (Clause.ClauseSet.mem c !clauses_) then (
          clauses_ := Clause.ClauseSet.add c !clauses_;
          Signal.send on_add_clause c
        ));
    ()

  let remove seq =
    seq (fun c ->
        if Clause.ClauseSet.mem c !clauses_ then (
          clauses_ := Clause.ClauseSet.remove c !clauses_;
          Signal.send on_remove_clause c
        ));
    ()
end

(** {2 Sets} *)

module ActiveSet = MakeClauseSet (struct end)

module SimplSet = struct
  let on_add_clause = Signal.create ()
  let on_remove_clause = Signal.create ()

  open struct
    let n_clauses = Atomic.make 0
  end

  let add seq =
    seq (fun c ->
        Atomic.incr n_clauses;
        Signal.send on_add_clause c);
    Trace.counter_int "simpl.n-clauses" (Atomic.get n_clauses)

  let remove seq =
    seq (fun c ->
        Atomic.decr n_clauses;
        Signal.send on_remove_clause c);
    Trace.counter_int "passive.n-clauses" (Atomic.get n_clauses)
end

module PassiveSet = struct
  include MakeClauseSet (struct end)

  let queue =
    let p = ClauseQueue.get_profile () in
    ClauseQueue.of_profile p

  let next () =
    let@ _sp = Trace.with_span ~__FILE__ ~__LINE__ "proof-state.next" in
    if ClauseQueue.is_empty queue then
      None
    else (
      try
        let x = ClauseQueue.take_first queue in
        Signal.send on_remove_clause x;
        clauses_ := Clause.ClauseSet.remove x !clauses_;
        Some x
      with Not_found -> None
    )

  let num_clauses () = ClauseQueue.length queue

  let remove seq =
    seq (fun c ->
        if ClauseQueue.remove queue c then Signal.send on_remove_clause c);
    Trace.counter_int "passive.n-clauses" (num_clauses ())

  let add seq =
    seq (fun c -> if ClauseQueue.add queue c then Signal.send on_add_clause c);
    Trace.counter_int "passive.n-clauses" (num_clauses ())

  let is_passive cl = ClauseQueue.mem_cl queue cl

  let clauses () =
    ClauseQueue.all_clauses queue |> Iter.to_list |> Clause.ClauseSet.of_list
end

type stats = int * int * int
(* num passive, num active, num simplification *)

let stats () =
  ( Clause.ClauseSet.cardinal (ActiveSet.clauses ()),
    Clause.ClauseSet.cardinal (PassiveSet.clauses ()),
    0 )

let pp out state =
  let num_active, num_passive, num_simpl = stats state in
  Format.fprintf out
    "state {%d active clauses; %d passive clauses; %d simplification_rules; %a}"
    num_active num_passive num_simpl ClauseQueue.pp PassiveSet.queue

let debug out state =
  let num_active, num_passive, num_simpl = stats state in
  Format.fprintf out
    "@[<v2>state {%d active clauses;@ %d passive clauses;@ %d \
     simplification_rules;@ queues@[<hv>%a@] @,\
     active:@[<hv>%a@]@,\
     passive:@[<hv>%a@]@,\
     }@]"
    num_active num_passive num_simpl ClauseQueue.pp PassiveSet.queue
    Clause.pp_set (ActiveSet.clauses ()) Clause.pp_set (PassiveSet.clauses ())

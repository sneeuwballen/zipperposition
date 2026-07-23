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

(** {2 Common clause-set class type} *)

module ClauseSet = struct
  class type t = object
    method on_add_clause : Clause.t Signal.t
    method on_remove_clause : Clause.t Signal.t
    method add : Clause.t Iter.t -> unit
    method remove : Clause.t Iter.t -> unit
    method clauses : unit -> Clause.ClauseSet.t
    method iter_clauses : Clause.t Iter.t
    method num_clauses : unit -> int
  end
end

(** {2 Sets} *)

module ActiveSet = struct
  include ClauseSet

  let create () : t =
    let set = ref Clause.ClauseSet.empty in
    let on_add = Signal.create () in
    let on_remove = Signal.create () in
    object
      method on_add_clause = on_add
      method on_remove_clause = on_remove

      method add seq =
        seq (fun c ->
            if not (Clause.ClauseSet.mem c !set) then (
              set := Clause.ClauseSet.add c !set;
              Signal.send on_add c
            ));
        ()

      method remove seq =
        seq (fun c ->
            if Clause.ClauseSet.mem c !set then (
              set := Clause.ClauseSet.remove c !set;
              Signal.send on_remove c
            ));
        ()

      method clauses () = !set
      method iter_clauses = Clause.ClauseSet.to_seq !set |> Iter.of_seq
      method num_clauses () = Clause.ClauseSet.cardinal !set
    end
end

module SimplSet = struct
  include ClauseSet

  let create () : t =
    let n = Atomic.make 0 in
    let on_add = Signal.create () in
    let on_remove = Signal.create () in
    object
      method on_add_clause = on_add
      method on_remove_clause = on_remove

      method add seq =
        seq (fun c ->
            Atomic.incr n;
            Signal.send on_add c);
        Trace.counter_int "simpl.n-clauses" (Atomic.get n)

      method remove seq =
        seq (fun c ->
            Atomic.decr n;
            Signal.send on_remove c);
        Trace.counter_int "passive.n-clauses" (Atomic.get n)

      method clauses () = Clause.ClauseSet.empty
      method iter_clauses = Iter.empty
      method num_clauses () = Atomic.get n
    end
end

module PassiveSet = struct
  class type t = object
    inherit ClauseSet.t
    method next : unit -> Clause.t option
    method is_passive : Clause.t -> bool
    method queue : ClauseQueue.t
  end

  let create () : t =
    let set = ref Clause.ClauseSet.empty in
    let queue = ClauseQueue.of_profile (ClauseQueue.get_profile ()) in
    let on_add = Signal.create () in
    let on_remove = Signal.create () in
    object (self)
      method on_add_clause = on_add
      method on_remove_clause = on_remove

      method add seq =
        seq (fun c ->
            if not (Clause.ClauseSet.mem c !set) then (
              set := Clause.ClauseSet.add c !set;
              Signal.send on_add c
            );
            if ClauseQueue.add queue c then
              Signal.send on_add c
            else
              ());
        Trace.counter_int "passive.n-clauses" (self#num_clauses ())

      method remove seq =
        seq (fun c ->
            if Clause.ClauseSet.mem c !set then (
              set := Clause.ClauseSet.remove c !set;
              Signal.send on_remove c
            );
            if ClauseQueue.remove queue c then Signal.send on_remove c);
        Trace.counter_int "passive.n-clauses" (self#num_clauses ())

      method clauses () =
        ClauseQueue.all_clauses queue
        |> Iter.to_list |> Clause.ClauseSet.of_list

      method iter_clauses = ClauseQueue.all_clauses queue
      method num_clauses () = ClauseQueue.length queue

      method next () =
        let@ _sp = Trace.with_span ~__FILE__ ~__LINE__ "proof-state.next" in
        if ClauseQueue.is_empty queue then
          None
        else (
          try
            let x = ClauseQueue.take_first queue in
            Signal.send on_remove x;
            set := Clause.ClauseSet.remove x !set;
            Some x
          with Not_found -> None
        )

      method is_passive cl = ClauseQueue.mem_cl queue cl
      method queue = queue
    end
end

type t = {
  active: ActiveSet.t;
  passive: PassiveSet.t;
  simpl: SimplSet.t;
}

let create () =
  {
    active = ActiveSet.create ();
    passive = PassiveSet.create ();
    simpl = SimplSet.create ();
  }

type stats = int * int * int
(* num passive, num active, num simplification *)

let stats self = self.active#num_clauses (), self.passive#num_clauses (), 0

let pp out self =
  let num_active, num_passive, num_simpl = stats self in
  Format.fprintf out
    "state {%d active clauses; %d passive clauses; %d simplification_rules; %a}"
    num_active num_passive num_simpl ClauseQueue.pp self.passive#queue

let debug out self =
  let num_active, num_passive, num_simpl = stats self in
  Format.fprintf out
    "@[<v2>state {%d active clauses;@ %d passive clauses;@ %d \
     simplification_rules;@ queues@[<hv>%a@] @,\
     active:@[<hv>%a@]@,\
     passive:@[<hv>%a@]@,\
     }@]"
    num_active num_passive num_simpl ClauseQueue.pp self.passive#queue
    Clause.pp_set (self.active#clauses ()) Clause.pp_set
    (self.passive#clauses ())

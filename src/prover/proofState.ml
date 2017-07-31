
(* This file is free software, part of Zipperposition. See file "license" for more details. *)

(** {1 The state of a proof, contains a set of active clauses (processed),
    a set of passive clauses (to be processed), and an ordering
    that is used for redundancy elimination.} *)

open Logtk

module T = Term
module C = Clause
module S = Subst.FO
module Lit = Literal
module Lits = Literals
module Pos = Position
module PB = Position.Build
module CQ = ClauseQueue

let prof_next_passive = Util.mk_profiler "proofState.next_passive"

(** {2 Set of active clauses} *)
module type S = ProofState_intf.S

module Make(C : Clause.S) : S with module C = C and module Ctx = C.Ctx = struct
  module Ctx = C.Ctx
  module C = C

  module CQueue = ClauseQueue.Make(C)

  (* module TermIndex = Fingerprint.Make(C.WithPos) *)
  module TermIndex = NPDtree.MakeTerm(C.WithPos)

  module UnitIndex =
    (* NPDtree *)
    Dtree
    .Make(struct
      type t = T.t * T.t * bool * C.t
      type rhs = T.t
      let compare (t11,t12,s1,c1) (t21,t22,s2,c2) =
        let open CCOrd in
        T.compare t11 t21
        <?> (T.compare, t12, t22)
        <?> (compare, s1, s2)
        <?> (C.compare, c1, c2)
      let extract (t1,t2,sign,_) = t1, t2, sign
      let priority (_,_,_,c) =
        if C.is_oriented_rule c then 2 else 1
    end)

  module SubsumptionIndex = FV_tree.Make(struct
      type t = C.t
      let compare = C.compare
      let to_lits c = C.to_forms c |> Sequence.of_list
      let labels c = C.trail c |> Trail.labels
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

  (** {6 Common Interface for Sets} *)

  module type CLAUSE_SET = sig
    val on_add_clause : C.t Signal.t
    (** signal triggered when a clause is added to the set *)

    val on_remove_clause : C.t Signal.t
    (** signal triggered when a clause is removed from the set *)

    val add : C.t Sequence.t -> unit
    (** Add clauses to the set *)

    val remove : C.t Sequence.t -> unit
    (** Remove clauses from the set *)
  end

  module MakeClauseSet(X : sig end) = struct
    let clauses_ = ref C.ClauseSet.empty

    let on_add_clause = Signal.create ()

    let on_remove_clause = Signal.create ()

    let clauses () = !clauses_
    let num_clauses () = C.ClauseSet.cardinal !clauses_

    let add seq =
      seq
        (fun c ->
           if not (C.ClauseSet.mem c !clauses_)
           then (
             clauses_ := C.ClauseSet.add c !clauses_;
             Signal.send on_add_clause c
           ));
      ()

    let remove seq =
      seq (fun c ->
        if C.ClauseSet.mem c !clauses_
        then (
          clauses_ := C.ClauseSet.remove c !clauses_;
          Signal.send on_remove_clause c
        ));
      ()
  end

  (** {2 Sets} *)

  module ActiveSet = MakeClauseSet(struct end)

  module SimplSet = struct
    let on_add_clause = Signal.create ()
    let on_remove_clause = Signal.create ()
    let add seq =
      seq (fun c -> Signal.send on_add_clause c)
    let remove seq =
      seq (fun c -> Signal.send on_remove_clause c)
  end

  module PassiveSet = struct
    include MakeClauseSet(struct end)

    let queue =
      let p = ClauseQueue.get_profile () in
      CQueue.of_profile p

    let next_ () =
      if CQueue.is_empty queue
      then None
      else (
        let x = CQueue.take_first queue in
        Some x
      )

    let next () = Util.with_prof prof_next_passive next_ ()

    (* register to signal *)
    let () =
      Signal.on_every on_add_clause
        (fun c -> CQueue.add queue c);
      ()
  end

  type stats = int * int * int
  (* num passive, num active, num simplification *)

  let stats () =
    C.ClauseSet.cardinal (ActiveSet.clauses ()),
    C.ClauseSet.cardinal (PassiveSet.clauses ()),
    0

  let pp out state =
    let num_active, num_passive, num_simpl = stats state in
    Format.fprintf out
      "state {%d active clauses; %d passive clauses; \
       %d simplification_rules; %a}"
      num_active num_passive num_simpl
      CQueue.pp PassiveSet.queue

  let debug out state =
    let num_active, num_passive, num_simpl = stats state in
    Format.fprintf out
      "@[<v2>state {%d active clauses;@ %d passive clauses;@ \
       %d simplification_rules;@ queues@[<hv>%a@] \
       @,active:@[<hv>%a@]@,passive:@[<hv>%a@]@,}@]"
      num_active num_passive num_simpl
      CQueue.pp PassiveSet.queue
      C.pp_set (ActiveSet.clauses ())
      C.pp_set (PassiveSet.clauses ())
end


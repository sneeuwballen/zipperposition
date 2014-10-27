
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

(** {1 The state of a proof, contains a set of active clauses (processed),
    a set of passive clauses (to be processed), and an ordering
    that is used for redundancy elimination.} *)

open Logtk

module T = FOTerm
module C = Clause
module S = Substs.FO
module Lit = Literal
module Lits = Literals
module Pos = Position
module PB = Position.Build
module CQ = ClauseQueue
module TO = Theories.TotalOrder

let prof_next_passive = Util.mk_profiler "proofState.next_passive"
let prof_clean_passive = Util.mk_profiler "proofState.clean_passive"

let stat_passive_cleanup = Util.mk_stat "cleanup of passive set"

(** {2 Set of active clauses} *)
module type S = ProofState_intf.S

module Make(C : Clause.S) : S with module C = C and module Ctx = C.Ctx = struct
  module Ctx = C.Ctx
  module C = C

  module CQueue = ClauseQueue.Make(C)

  (* module TermIndex = Fingerprint.Make(C.WithPos) *)
  module TermIndex = NPDtree.MakeTerm(C.WithPos)

  module UnitIndex = NPDtree.Make(struct
    type t = T.t * T.t * bool * C.t
    type rhs = T.t
    let compare (t11,t12,s1,c1) (t21,t22,s2,c2) =
      Util.lexicograph_combine [T.cmp t11 t21; T.cmp t12 t22;
                                compare s1 s2; C.compare c1 c2]
    let extract (t1,t2,sign,_) = t1, t2, sign
    let priority (_,_,_,c) =
      if C.is_oriented_rule c then 2 else 1
  end)

  module SubsumptionIndex = FeatureVector.Make(struct
    type t = C.t
    let cmp = C.compare
    let to_lits = C.Seq.abstract
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
    let _clauses = ref C.CSet.empty

    let on_add_clause = Signal.create ()

    let on_remove_clause = Signal.create ()

    let clauses () = !_clauses

    let add seq =
      seq (fun c ->
        if not (C.CSet.mem !_clauses c)
        then begin
          _clauses := C.CSet.add !_clauses c;
          Signal.send on_add_clause c
        end);
      ()

    let remove seq =
      seq (fun c ->
        if C.CSet.mem !_clauses c
        then begin
          _clauses := C.CSet.remove !_clauses c;
          Signal.send on_remove_clause c
        end);
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

    let _queues = ref (Array.of_list CQueue.default_queues)
    let _state = ref (0,0)

    let () =
      assert (Array.length !_queues > 0);
      ()

    let remove_by_id seq =
      _clauses := C.CSet.remove_id_seq !_clauses seq;
      ()

    let queues k =
      Array.iter (fun (q,coeff) -> k (q,coeff)) !_queues

    let add_queue q coeff =
      (* add all clauses to the queue *)
      let q = C.CSet.fold !_clauses q (fun q _ c -> CQueue.add q c) in
      _queues := Array.of_list ((q, coeff) :: Array.to_list !_queues)

    let clean () =
      Util.enter_prof prof_clean_passive;
      Util.incr_stat stat_passive_cleanup;
      for i = 0 to Array.length (!_queues) - 1 do
        let q, w = (!_queues).(i) in
        (!_queues).(i) <- CQueue.clean q !_clauses, w
      done;
      Util.exit_prof prof_clean_passive

    let next () =
      Util.enter_prof prof_next_passive;
      let first_idx, w = !_state in
      (* search in the idx-th queue *)
      let rec search idx weight =
        let q, w = (!_queues).(idx) in
        if Array.length !_queues=1 && CQueue.is_empty q then None
        else
          if (Array.length !_queues > 1 && weight >= w) || CQueue.is_empty q
          then next_idx (idx+1) (* empty queue, go to the next one *)
        else begin
          let new_q, c = CQueue.take_first q in (* pop from this queue *)
          (!_queues).(idx) <- new_q, w;
          if C.CSet.mem !_clauses c
            then begin (* done, found a still-valid clause *)
              Util.debug 3 "taken clause from %s" (CQueue.name q);
              remove (Sequence.singleton c);
              _state := (idx, weight+1);
              Some c
            end
            else search idx weight
        end
      (* search the next non-empty queue *)
      and next_idx idx =
        if idx = first_idx then None (* all queues are empty *)
        else if idx = Array.length !_queues then next_idx 0 (* cycle *)
        else search idx 0 (* search in this queue *)
      in
      let res = search first_idx w in
      Util.exit_prof prof_next_passive;
      res

    (* register to signal *)
    let () =
      Signal.on on_add_clause
        (fun c ->
          for i = 0 to Array.length (!_queues) - 1 do
            (* add to i-th queue *)
            let q, w = !_queues.(i) in
            (!_queues).(i) <- (CQueue.add q c, w)
          done;
          Signal.ContinueListening);
      ()
  end

  type stats = int * int * int
  (* num passive, num active, num simplification *)

  let stats () =
    ( C.CSet.size (ActiveSet.clauses ())
    , C.CSet.size (PassiveSet.clauses ())
    , 0)

  let pp buf state =
    let num_active, num_passive, num_simpl = stats state in
    Printf.bprintf buf
      "state {%d active clauses; %d passive_clauses; %d simplification_rules; %a}"
      num_active num_passive num_simpl
      CQueue.pp_list (PassiveSet.queues |> Sequence.to_list)

  let debug buf state =
    let num_active, num_passive, num_simpl = stats state in
    Printf.bprintf buf
      ("state {%d active clauses; %d passive_clauses; %d simplification_rules; %a" ^^
        "\nactive:%a\npassive:%a\n}")
      num_active num_passive num_simpl
      CQueue.pp_list (PassiveSet.queues |> Sequence.to_list)
      C.pp_set (ActiveSet.clauses ())
      C.pp_set (PassiveSet.clauses ())

end


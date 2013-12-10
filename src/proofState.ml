
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
module Lits = Literal.Arr
module Pos = Position
module PB = Position.Build
module CQ = ClauseQueue
module TO = Theories.TotalOrder

let prof_add_active = Util.mk_profiler "proofState.add_active"
let prof_remove_active = Util.mk_profiler "proofState.remove_active"
let prof_add_passive = Util.mk_profiler "proofState.add_passive"
let prof_next_passive = Util.mk_profiler "proofState.next_passive"
let prof_clean_passive = Util.mk_profiler "proofState.clean_passive"
let prof_add_simpl = Util.mk_profiler "proofState.add_simpl"
let prof_remove_simpl = Util.mk_profiler "proofState.remove_simpl"

let stat_passive_cleanup = Util.mk_stat "cleanup of passive set"

(* module TermIndex = Fingerprint.Make(C.WithPos) *)
module TermIndex = NPDtree.MakeTerm(C.WithPos)

module UnitIndex = NPDtree.Make(struct
  type t = T.t * T.t * bool * C.t
  type rhs = T.t
  let compare (t11,t12,s1,c1) (t21,t22,s2,c2) =
    Util.lexicograph_combine [T.compare t11 t21; T.compare t12 t22;
                              compare s1 s2; C.compare c1 c2]
  let extract (t1,t2,sign,_) = t1, t2, sign
  let priority (_,_,_,c) =
    if C.is_oriented_rule c then 2 else 1
end)

module SubsumptionIndex = FeatureVector.Make(struct
  type t = C.t
  let cmp = C.compare
  let to_lits = C.to_seq
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

(** {2 Set of active clauses} *)

module ActiveSet = struct
  type t = 
    < ctx : Ctx.t;
      clauses : Clause.CSet.t;          (** set of active clauses *)
      idx_sup_into : TermIndex.t;       (** index for superposition into the set *)
      idx_sup_from : TermIndex.t;       (** index for superposition from the set *)
      idx_back_demod : TermIndex.t;     (** index for backward demodulation/simplifications *)
      idx_fv : SubsumptionIndex.t;      (** index for subsumption *)

      add : Clause.t Sequence.t -> unit;   (** add clauses *)
      remove : Clause.t Sequence.t -> unit;(** remove clauses *)
    >

  let create ~ctx signature =
    (* create a FeatureVector index from the current signature *)
    let idx = SubsumptionIndex.of_signature signature in
    (object (self)
      val mutable m_clauses = C.CSet.empty
      val mutable m_sup_into = TermIndex.empty ()
      val mutable m_sup_from = TermIndex.empty ()
      val mutable m_back_demod = TermIndex.empty ()
      val mutable m_fv = idx
      method ctx = ctx
      method clauses = m_clauses
      method idx_sup_into = m_sup_into
      method idx_sup_from = m_sup_from
      method idx_back_demod = m_back_demod
      method idx_fv = m_fv

      (* apply operation [f] to some parts of [c] *)
      method update f c =
        (* index subterms that can be rewritten by superposition *)
        m_sup_into <- Lits.fold_terms ~which:`Max ~subterms:true
          ~eligible:(C.Eligible.res c) c.C.hclits m_sup_into
          (fun tree t pos ->
            let with_pos = C.WithPos.({term=t; pos; clause=c;}) in
            f tree t with_pos);
        (* index terms that can rewrite into other clauses *)
        m_sup_from <- Lits.fold_eqn ~both:true ~sign:true
          ~eligible:(C.Eligible.param c) c.C.hclits m_sup_from
          (fun tree l r sign pos ->
            assert sign;
            let with_pos = C.WithPos.({term=l; pos; clause=c;}) in
            f tree l with_pos);
        (* terms that can be demodulated: all subterms *)
        m_back_demod <- Lits.fold_terms ~subterms:true ~which:`Both
          ~eligible:C.Eligible.always c.C.hclits m_back_demod
          (fun tree t pos ->
            let with_pos = C.WithPos.( {term=t; pos; clause=c} ) in
            f tree t with_pos);
        ()

      (** add clauses (only process the ones not present in the set) *)
      method add cs =
        Util.enter_prof prof_add_active;
        let cs = Sequence.filter (fun c -> not (C.CSet.mem m_clauses c)) cs in
        let cs = Sequence.persistent cs in
        m_clauses <- C.CSet.of_seq m_clauses cs;
        let op tree = TermIndex.add tree in
        Sequence.iter (self#update op) cs;
        m_fv <- SubsumptionIndex.add_seq m_fv cs;
        Util.exit_prof prof_add_active

      (** remove clauses (only process the ones present in the set) *)
      method remove cs =
        Util.enter_prof prof_remove_active;
        let cs = Sequence.filter (C.CSet.mem m_clauses) cs in
        let cs = Sequence.persistent cs in
        m_clauses <- C.CSet.remove_seq m_clauses cs;
        let op tree = TermIndex.remove tree in
        Sequence.iter (self#update op) cs;
        m_fv <- SubsumptionIndex.remove_seq m_fv cs;
        Util.exit_prof prof_remove_active
    end :> t)
end

(** {2 Set of simplifying (unit) clauses} *)

module SimplSet = struct
  type t =
    < ctx : Ctx.t;
      idx_simpl : UnitIndex.t;      (** index for forward simplifications *)

      add : Clause.t Sequence.t -> unit;
      remove : Clause.t Sequence.t -> unit;
    >

  let apply op idx c = match c.C.hclits with
    | [| Lit.Equation (l,r,true,Comparison.Gt) |] ->
      op idx (l,r,true,c)
    | [| Lit.Equation (l,r,true,Comparison.Lt) |] ->
      op idx (r,l,true,c)
    | [| Lit.Equation (l,r,true,Comparison.Incomparable) |] ->
      let idx = op idx (l,r,true,c) in
      op idx (r,l,true,c)
    | [| Lit.Equation (l,r,false,_) |] ->
      op idx (l,r,false,c)
    | [| Lit.Prop (p, sign) |] ->
      op idx (p,T.true_term,sign,c)
    | _ -> idx

  (** Create a simplification set *)
  let create ~ctx =
    object
      val mutable m_simpl = UnitIndex.empty ()
      method ctx = ctx
      method idx_simpl = m_simpl

      method add cs =
        Util.enter_prof prof_add_simpl;
        let cs = Sequence.filter C.is_unit_clause cs in
        m_simpl <- Sequence.fold (apply UnitIndex.add) m_simpl cs;
        Util.exit_prof prof_add_simpl

      method remove cs =
        Util.enter_prof prof_remove_simpl;
        let cs = Sequence.filter C.is_unit_clause cs in
        m_simpl <- Sequence.fold (apply UnitIndex.remove) m_simpl cs;
        Util.exit_prof prof_remove_simpl
    end
end

(** {2 Set of passive clauses} *)

module PassiveSet = struct
  type t =
    < ctx : Ctx.t;
      clauses : Clause.CSet.t;           (** set of clauses *)
      queues : (ClauseQueue.t * int) list;

      add : Clause.t Sequence.t -> unit;   (** add clauses *)
      remove : int -> unit;               (** remove clause by ID *)
      next : unit -> Clause.t option;      (** next passive clause, if any *)
      clean : unit -> unit;               (** cleanup internal queues *)
    >

  let create ~ctx queues =
    assert (queues != []);
    let length = List.length queues in
    object
      val mutable m_clauses = C.CSet.empty
      val m_queues = Array.of_list queues
      val mutable m_state = (0,0)
      method ctx = ctx
      method clauses = m_clauses
      method queues = Array.to_list m_queues

      (** add clauses (not already present in set) to the set *)
      method add cs =
        Util.enter_prof prof_add_passive;
        let cs = Sequence.filter (fun c -> not (C.CSet.mem m_clauses c)) cs in
        let cs = Sequence.persistent cs in
        m_clauses <- C.CSet.of_seq m_clauses cs;
        for i = 0 to length - 1 do
          (* add to i-th queue *)
          let (q, w) = m_queues.(i) in
          m_queues.(i) <- (CQ.adds q cs, w)
        done;
        Util.exit_prof prof_add_passive

      (** remove clauses (not from the queues) *)
      method remove id =
        m_clauses <- C.CSet.remove_id m_clauses id

      (** next clause *)
      method next () =
        Util.enter_prof prof_next_passive;
        let first_idx, w = m_state in
        (* search in the idx-th queue *)
        let rec search idx weight =
          let q, w = m_queues.(idx) in
          if weight >= w || CQ.is_empty q
          then next_idx (idx+1) (* empty queue, go to the next one *)
          else begin
            let new_q, c = CQ.take_first q in (* pop from this queue *)
            m_queues.(idx) <- new_q, w;
            if C.CSet.mem m_clauses c
              then begin (* done, found a still-valid clause *)
                Util.debug 3 "taken clause from %s" (CQ.name q);
                m_clauses <- C.CSet.remove m_clauses c;
                m_state <- (idx, weight+1);
                Some c
              end else search idx weight
          end
        (* search the next non-empty queue *)
        and next_idx idx =
          if idx = first_idx then None (* all queues are empty *)
          else if idx = length then next_idx 0 (* cycle *)
          else search idx 0 (* search in this queue *)
        in
        let res = search first_idx w in
        Util.exit_prof prof_next_passive;
        res

      (* cleanup the clause queues *)
      method clean () =
        Util.enter_prof prof_clean_passive;
        Util.incr_stat stat_passive_cleanup;
        for i = 0 to length - 1 do
          let q, w = m_queues.(i) in
          m_queues.(i) <- CQ.clean q m_clauses, w
        done;
        Util.exit_prof prof_clean_passive
    end
end

(** {2 Proof State} *)

type t =
  < ctx : Ctx.t;
    params : Params.t;
    simpl_set : SimplSet.t;              (** index for forward demodulation *)
    active_set : ActiveSet.t;            (** active clauses *)
    passive_set : PassiveSet.t;          (** passive clauses *)
    meta_prover : MetaProverState.t option;
    experts : Experts.Set.t;            (** Set of current experts *)

    add_expert : Experts.t -> unit;     (** Add an expert *)
  >

let create ~ctx ?meta params signature =
  let queues = ClauseQueue.default_queues
  and _experts = ref (Experts.Set.empty ~ctx) in
  object
    val m_active = (ActiveSet.create ~ctx signature :> ActiveSet.t)
    val m_passive = (PassiveSet.create ~ctx queues :> PassiveSet.t)
    val m_simpl = (SimplSet.create ~ctx :> SimplSet.t)
    method ctx = ctx
    method params = params
    method active_set = m_active
    method passive_set = m_passive
    method simpl_set = m_simpl
    method meta_prover = meta
    method experts = !_experts
    method add_expert e =
      let es = Experts.update_ctx e ~ctx in
      _experts := Experts.Set.add_list !_experts es;
      (* add clauses of each expert to the set of passive clauses *)
      let clauses = Sequence.flatMap
        (fun e -> Sequence.of_list (Experts.clauses e))
        (Sequence.of_list es) in
      m_passive#add clauses
  end

type stats = int * int * int (* num passive, num active, num simplification *)

let stats (state : t) =
  ( C.CSet.size state#active_set#clauses
  , C.CSet.size state#passive_set#clauses
  , UnitIndex.size state#simpl_set#idx_simpl)

let pp buf state =
  let num_active, num_passive, num_simpl = stats state in
  Printf.bprintf buf
    "state {%d active clauses; %d passive_clauses; %d simplification_rules; %a}"
    num_active num_passive num_simpl
    CQ.pp_list state#passive_set#queues

let debug buf state =
  let num_active, num_passive, num_simpl = stats state in
  Printf.bprintf buf
    ("state {%d active clauses; %d passive_clauses; %d simplification_rules; %a" ^^
      "\nactive:%a\npassive:%a\n}")
    num_active num_passive num_simpl
    CQ.pp_list state#passive_set#queues
    C.pp_set state#active_set#clauses
    C.pp_set state#passive_set#clauses

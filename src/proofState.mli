
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

module TermIndex : Index.TERM_IDX with type elt = Clause.WithPos.t
module UnitIndex : Index.UNIT_IDX with type E.t = (FOTerm.t * FOTerm.t * bool * Clause.t)
                                  and type E.rhs = FOTerm.t
module SubsumptionIndex : Index.SUBSUMPTION_IDX with type C.t = Clause.t

(** {2 Set of active clauses} *)

module ActiveSet : sig
  type t = 
    < ctx : Ctx.t;
      clauses : Clause.CSet.t;          (** set of active clauses *)
      idx_sup_into : TermIndex.t;       (** index for superposition into the set *)
      idx_sup_from : TermIndex.t;       (** index for superposition from the set *)
      idx_ord_left : TermIndex.t;       (** terms at LHS of inequality *)
      idx_ord_right : TermIndex.t;      (** terms at RHS of inequality *)
      idx_ord_subterm : TermIndex.t;    (** subterms of inequality literals *)
      idx_back_demod : TermIndex.t;     (** index for backward demodulation/simplifications *)
      idx_fv : SubsumptionIndex.t;      (** index for subsumption *)

      add : Clause.t Sequence.t -> unit;   (** add clauses *)
      remove : Clause.t Sequence.t -> unit;(** remove clauses *)
    >

  val create : ctx:Ctx.t -> Signature.t -> t
end

(** {2 Set of simplifying (unit) clauses} *)

module SimplSet : sig
  type t =
    < ctx : Ctx.t;
      idx_simpl : UnitIndex.t;      (** index for forward simplifications *)

      add : Clause.t Sequence.t -> unit;
      remove : Clause.t Sequence.t -> unit;
    >

  val create : ctx:Ctx.t -> t
end

(** {2 Set of passive clauses} *)

module PassiveSet : sig
  type t =
    < ctx : Ctx.t;
      clauses : Clause.CSet.t;           (** set of clauses *)
      queues : (ClauseQueue.t * int) list;

      add : Clause.t Sequence.t -> unit;   (** add clauses *)
      remove : int -> unit;               (** remove clause by ID *)
      next : unit -> Clause.t option;      (** next passive clause, if any *)
      clean : unit -> unit;               (** cleanup internal queues *)
    >

  val create : ctx:Ctx.t -> (ClauseQueue.t * int) list -> t
end

(** {2 Proof State} *)

(** state of a superposition calculus instance.
    It contains a set of active clauses, a set of passive clauses,
    and is parametrized by an ordering. *)

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

val create : ctx:Ctx.t -> ?meta:MetaProverState.t ->
             Params.t -> Signature.t -> t
  (** create a state from the given ordering, and parameters *)

type stats = int * int * int
  (** statistics on the state (num active, num passive, num simplification) *)

val stats : t -> stats
  (** Compute statistics *)

val pp : Buffer.t -> t -> unit
  (** pretty print the content of the state *)

val debug : Buffer.t -> t -> unit
  (** debug functions: much more detailed printing *)


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

(** {2 Set of active clauses} *)
module type S = sig
  module Ctx : Ctx.S
  module C : Clause.S

  (** {6 Useful Index structures} *)

  module TermIndex : Index.TERM_IDX with type elt = C.WithPos.t
  module UnitIndex : Index.UNIT_IDX
    with type E.t = (FOTerm.t * FOTerm.t * bool * C.t)
    and type E.rhs = FOTerm.t
  module SubsumptionIndex : Index.SUBSUMPTION_IDX with type C.t = C.t

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

    val clauses : unit -> C.CSet.t
    (** Current set of clauses *)
  end

  module ActiveSet : CLAUSE_SET

  module SimplSet : CLAUSE_SET

  module PassiveSet : sig
    include CLAUSE_SET

    module CQueue : ClauseQueue.S with module C = C
    (** Priority queues on clauses *)


    val queues : unit -> (CQueue.t * int) list
    (** Current state of the clause queues *)

    val add_queue : CQueue.t -> int -> unit
    (** Add a new queue to the set of queues *)

    val clean : unit -> unit
    (** Clean clause queues (remove clauses that are no longer passive, but
       still in the queue) *)
  end

  (** {6 Misc} *)

  type stats = int * int * int
    (** statistics on the state (num active, num passive, num simplification) *)

  val stats : unit -> stats
    (** Compute statistics *)

  val pp : Buffer.t -> unit -> unit
    (** pretty print the content of the state *)

  val debug : Buffer.t -> unit -> unit
    (** debug functions: much more detailed printing *)
end

(** {2 Create a Proof State} *)

module Make(X : sig
  module C : Clause.S
  val params : Params.t  (** global parameters *)
end) : S with module C = X.C and module Ctx = X.C.Ctx

(* TODO: move indices to Superposition


module ActiveSet : sig
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
*)

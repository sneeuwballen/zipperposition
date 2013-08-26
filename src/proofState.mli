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

(** {1 The state of a proof, contains a set of active clauses (processed),
    a set of passive clauses (to be processed), and an ordering
    that is used for redundancy elimination.} *)

open Logtk

type clause_pos = Clause.t * Position.t * Term.t

module TermIndex : Index.TERM_IDX with type elt = clause_pos
module UnitIndex : Index.UNIT_IDX with type E.t = Clause.t
module SubsumptionIndex : Index.SUBSUMPTION_IDX with type C.t = Clause.t

(** {2 Set of active clauses} *)

module ActiveSet : sig
  type t = 
    < ctx : Clause.context;
      clauses : Clause.CSet.t;          (** set of active clauses *)
      idx_sup_into : TermIndex.t;       (** index for superposition into the set *)
      idx_sup_from : TermIndex.t;       (** index for superposition from the set *)
      idx_back_demod : TermIndex.t;     (** index for backward demodulation/simplifications *)
      idx_fv : SubsumptionIndex.t;      (** index for subsumption *)

      add : Clause.t Sequence.t -> unit;   (** add clauses *)
      remove : Clause.t Sequence.t -> unit;(** remove clauses *)
    >

  val create : ctx:Clause.context -> Signature.t -> t
end

(** {2 Set of simplifying (unit) clauses} *)

module SimplSet : sig
  type t =
    < ctx : Clause.context;
      idx_simpl : UnitIndex.t;      (** index for forward simplifications *)

      add : Clause.t Sequence.t -> unit;
      remove : Clause.t Sequence.t -> unit;
    >

  val create : ctx:Clause.context -> t
end

(** {2 Set of passive clauses} *)

module PassiveSet : sig
  type t =
    < ctx : Clause.context;
      clauses : Clause.CSet.t;           (** set of clauses *)
      queues : (ClauseQueue.t * int) list;

      add : Clause.t Sequence.t -> unit;   (** add clauses *)
      remove : int -> unit;               (** remove clause by ID *)
      next : unit -> Clause.t option;      (** next passive clause, if any *)
      clean : unit -> unit;               (** cleanup internal queues *)
    >

  val create : ctx:Clause.context -> (ClauseQueue.t * int) list -> t
end

(** {2 Proof State} *)

(** state of a superposition calculus instance.
    It contains a set of active clauses, a set of passive clauses,
    and is parametrized by an ordering. *)

type t =
  < ctx : Clause.context;
    params : Params.t;
    simpl_set : SimplSet.t;              (** index for forward demodulation *)
    active_set : ActiveSet.t;            (** active clauses *)
    passive_set : PassiveSet.t;          (** passive clauses *)
    meta_prover : MetaProverState.t option;
    experts : Experts.Set.t;            (** Set of current experts *)

    add_expert : Experts.t -> unit;     (** Add an expert *)
  >

val create : ctx:Clause.context -> ?meta:MetaProverState.t ->
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

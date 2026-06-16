(* This file is free software, part of Zipperposition. See file "license" for more details. *)

(** {1 The state of a proof, contains a set of active clauses (processed), a set
    of passive clauses (to be processed), and an ordering that is used for
    redundancy elimination.} *)

open Logtk

module type S = ProofState_intf.S

(** {2 Index modules} *)

module TermIndex : Index.TERM_IDX with type elt = Clause.WithPos.t

module UnitIndex :
  Index.UNIT_IDX
    with type E.t = Term.t * Term.t * bool * Clause.t
     and type E.rhs = Term.t

module SubsumptionIndex : Index.SUBSUMPTION_IDX with type C.t = Clause.t

(** {2 Sets of clauses} *)

module ActiveSet : sig
  val on_add_clause : Clause.t Signal.t
  val on_remove_clause : Clause.t Signal.t
  val add : Clause.t Iter.t -> unit
  val remove : Clause.t Iter.t -> unit
  val clauses : unit -> Clause.ClauseSet.t
  val num_clauses : unit -> int
end

module SimplSet : sig
  val on_add_clause : Clause.t Signal.t
  val on_remove_clause : Clause.t Signal.t
  val add : Clause.t Iter.t -> unit
  val remove : Clause.t Iter.t -> unit
end

module PassiveSet : sig
  val on_add_clause : Clause.t Signal.t
  val on_remove_clause : Clause.t Signal.t
  val add : Clause.t Iter.t -> unit
  val remove : Clause.t Iter.t -> unit
  val clauses : unit -> Clause.ClauseSet.t
  val is_passive : Clause.t -> bool
  val queue : ClauseQueue.t
  val next : unit -> Clause.t option
  val num_clauses : unit -> int
end

type stats = int * int * int

val stats : unit -> stats
val pp : unit CCFormat.printer
val debug : unit CCFormat.printer

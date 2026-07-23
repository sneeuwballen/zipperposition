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

(** {2 Common clause-set class type} *)

module ClauseSet : sig
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

module ActiveSet : sig
  type t = ClauseSet.t

  val create : unit -> t
end

module SimplSet : sig
  type t = ClauseSet.t

  val create : unit -> t
end

module PassiveSet : sig
  class type t = object
    inherit ClauseSet.t
    method next : unit -> Clause.t option
    method is_passive : Clause.t -> bool
    method queue : ClauseQueue.t
  end

  val create : unit -> t
end

type t = {
  active: ActiveSet.t;
  passive: PassiveSet.t;
  simpl: SimplSet.t;
}

val create : unit -> t

type stats = int * int * int

val stats : t -> stats
val pp : t CCFormat.printer
val debug : t CCFormat.printer


(* This file is free software, part of Zipperposition. See file "license" for more details. *)

open Libzipperposition

(** {2 Set of active clauses} *)
module type S = sig
  module Ctx : Ctx.S
  module C : Clause.S

  module CQueue : ClauseQueue.S with module C = C and type C.t = C.t
  (** Priority queues on clauses *)

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
  end

  module ActiveSet : sig
    include CLAUSE_SET

    val clauses : unit -> C.CSet.t
    (** Current set of clauses *)
  end

  module SimplSet : CLAUSE_SET

  module PassiveSet : sig
    include CLAUSE_SET

    val remove_by_id : int Sequence.t -> unit
    (** Remove clauses by their ID. This will {b NOT} trigger
        the signal {!on_remove_clause}. *)

    val clauses : unit -> C.CSet.t
    (** Current set of clauses *)

    val queues : (CQueue.t * int) Sequence.t
    (** Current state of the clause queues *)

    val add_queue : CQueue.t -> int -> unit
    (** Add a new queue to the set of queues *)

    val clean : unit -> unit
    (** Clean clause queues (remove clauses that are no longer passive, but
        still in the queue) *)

    val next : unit -> C.t option
    (** Get-and-remove the next passive clause to process *)
  end

  (** {6 Misc} *)

  type stats = int * int * int
  (** statistics on the state (num active, num passive, num simplification) *)

  val stats : unit -> stats
  (** Compute statistics *)

  val pp : unit CCFormat.printer
  (** pretty print the content of the state *)

  val debug : unit CCFormat.printer
  (** debug functions: much more detailed printing *)
end

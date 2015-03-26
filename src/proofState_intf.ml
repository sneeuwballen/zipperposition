
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

open Logtk

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

  val pp : Buffer.t -> unit -> unit
    (** pretty print the content of the state *)

  val debug : Format.formatter -> unit -> unit
    (** debug functions: much more detailed printing *)
end

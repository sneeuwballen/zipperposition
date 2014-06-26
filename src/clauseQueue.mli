
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

(** {1 Priority Queue of clauses} *)

(** Heuristic selection of clauses, using queues. Note that some
    queues do not need accept all clauses, as long as one of them does
    (for completeness). Anyway, a fifo queue should always be present,
    and presents this property. *)

open Logtk

val profile : unit -> string
val set_profile : string -> unit

(** {2 A priority queue of clauses, purely functional} *)
module type S = sig
  module C : Clause.S

  type t

  val add : t -> C.t -> t
    (** Add a clause to the Queue *)

  val adds : t -> C.t Sequence.t -> t
    (** Add clauses to the queue *)

  val is_empty : t -> bool
    (** check whether the queue is empty *)

  val take_first : t -> (t * C.t)
    (** Take first element of the queue, or raise Not_found *)

  val clean : t -> C.CSet.t -> t
    (** remove all clauses that are not in the set *)

  val name : t -> string
    (** Name of the implementation/role of the queue *)

  (** {6 Available Queues} *)

  val fifo : t
    (** select by increasing age (for fairness) *)

  val clause_weight : t
    (** select by increasing weight of clause *)

  val goals : t
    (** only select goals (clauses with only negative lits) *)

  val non_goals : t
    (** only select non-goals *)

  val ground : t
    (** only select ground clauses *)

  val pos_unit_clauses : t
    (** only select positive unit clauses *)

  val horn : t
    (** select horn clauses *)

  val lemmas : t
    (** only select lemmas *)

  val mk_queue : ?accept:(C.t -> bool) -> weight:(C.t -> int) -> string -> t
    (** Bring your own implementation of queue *)

  (** {6 Combination of queues} *)

  type queues = (t * int) list

  module Profiles : sig
    val bfs : queues
      (** Strong orientation toward FIFO *)

    val explore : queues
      (** Use heuristics for selecting "small" clauses *)

    val ground : queues
      (** Favor positive unit clauses and ground clauses *)
  end

  val default_queues : queues
    (** default combination of heuristics *)

  (** {6 IO} *)

  val pp : Buffer.t -> t -> unit
  val to_string : t -> string
  val pp_list : Buffer.t -> (t * int) list -> unit
  val fmt : Format.formatter -> t -> unit
end

module Make(C : Clause.S) : S with module C = C

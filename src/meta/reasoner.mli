
(*
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

(** {1 Meta-Level reasoner} *)

open Logtk

(** {2 Meta-level property}
A meta-level statement is just a higher-order term. *)

type term = HOTerm.t
type property = term
type fact = term

val property_ty : Type.t
  (** Type of meta-level statements. All terms used within
      the meta-prover should have this type *)

(** {2 Meta-Level clause}
A Horn clause about meta-level properties *)

module Clause : sig
  type t = {
    head : term;
    body : term list;
  }

  val fact : term -> t
  val rule : term -> term list -> t

  val is_fact : t -> bool

  include Interfaces.PRINT with type t := t
  include Interfaces.HASH with type t := t
  include Interfaces.ORD with type t := t

  module Seq : sig
    val terms : t -> term Sequence.t
    val vars : t -> term Sequence.t
  end
end

type clause = Clause.t

(** {2 Proofs}

Unit-resolution proofs *)

module Proof : sig
  type t =
    | Axiom
    | Resolved of fact with_proof * clause with_proof

  and 'a with_proof = {
    conclusion : 'a;
    proof : t;
  }

  val facts : t -> fact Sequence.t
    (** Iterate on the facts that have been used *)

  val rules : t -> clause Sequence.t
    (** Iterate on the rules that have been used *)
end

type proof = Proof.t

(** {2 Consequences}
What can be deduced when the Database is updated with new rules
and facts. *)

type consequence = fact * proof

(** {2 Fact and Rules Database}

A database contains a set of Horn-clauses about properties, that allow
to reason about them by forward-chaining. *)

type t
  (** A DB that holds a saturated set of Horn clauses and facts *)

val empty : t
  (** Empty DB *)

val add : t -> clause -> t * consequence Sequence.t
  (** Add a clause to the DB, and propagate.
     See {!Seq.of_seq} for an efficient batch insertion. *)

val add_fact : t -> fact -> t * consequence Sequence.t
  (** Add a fact to the DB. Sugar for {!add} *)

module Seq : sig
  val of_seq : t -> clause Sequence.t -> t * consequence Sequence.t
  val to_seq : t -> clause Sequence.t  (** Only iterate on axioms *)
  val facts : t -> fact Sequence.t
end

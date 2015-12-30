
(* This file is free software, part of Libzipperposition. See file "license" for more details. *)

(** {1 Meta-Level reasoner} *)

open Libzipperposition

(** {2 Meta-level property}
    A meta-level statement is just a higher-order term. *)

type term = HOTerm.t
type property = term
type fact = term

val property_ty : Type.t
(** Type of meta-level statements. All terms used within
    the meta-prover should have this type *)

val property_id : ID.t

(** {2 Meta-Level clause}
    A Horn clause about meta-level properties *)

exception Error of string

module Clause : sig
  type t = {
    head : term;
    body : term list;
  }

  val rule : term -> term list -> t
  (** Build a rule.
      @raise Error if the rule is unsafe, i.e. if some free variables
        occur in the consequence but not in the body *)

  val fact : term -> t
  (** [fact t] is a shortcut for [rule t []] *)

  val is_fact : t -> bool

  include Interfaces.PRINT with type t := t
  include Interfaces.HASH with type t := t
  include Interfaces.ORD with type t := t

  module Seq : sig
    val terms : t -> term Sequence.t
    val vars : t -> Type.t HVar.t Sequence.t
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


(* This file is free software, part of Zipperposition. See file "license" for more details. *)

open Libzipperposition

type spec = {
  sym : ID.t;
  ty : Type.t;
}

module type S = sig
  module Env : Env.S
  module C : module type of Env.C

  val on_add : spec Signal.t

  val add : ?proof:Proof.t list -> ID.t -> Type.t -> unit
  (** Declare that the given symbol is AC, and update the Env subsequently
      by adding clauses, etc. *)

  val is_ac : ID.t -> bool

  val find_proof : ID.t -> Proof.t list
  (** Recover the proof for the AC-property of this symbol.
      @raise Not_found if the symbol is not AC *)

  val symbols : unit -> ID.Set.t
  (** set of AC symbols *)

  val symbols_of_terms : FOTerm.t Sequence.t -> ID.Set.t
  (** set of AC symbols occurring in the given term *)

  val proofs : unit -> Proof.t list
  (** All proofs for all AC axioms *)

  val exists_ac : unit -> bool
  (** Is there any AC symbol? *)

  val axioms : ID.t -> Type.t -> C.t list
  (** List of (persistent) axioms that are needed for simplifications to
      be complete for the given symbol. The [ctx] is required for type inference
      and building clauses . *)

  (** {2 Rules} *)

  val is_trivial_lit : Literal.t -> bool
  (** Is the literal AC-trivial? *)

  val is_trivial : C.t -> bool
  (** Check whether the clause is AC-trivial *)

  val simplify : Env.simplify_rule
  (** Simplify the clause modulo AC *)

  val setup : unit -> unit
  (** Register on Env *)
end

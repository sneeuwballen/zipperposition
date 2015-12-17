
(* This file is free software, part of Zipperposition. See file "license" for more details. *)

(** {1 AC redundancy} *)

open Logtk

type spec = Theories.AC.t

module type S = sig
  module Env : Env.S
  module C : module type of Env.C

  val axioms : ID.t -> Type.t -> C.t list
  (** List of (persistent) axioms that are needed for simplifications to
      be complete for the given symbol. The [ctx] is required for type inference
      and building clauses . *)

  (** {2 Rules} *)

  val is_trivial_lit : Literal.t -> bool
  (** Is the literal AC-trivial? *)

  val is_trivial : C.t -> bool
  (** Check whether the clause is AC-trivial *)

  val simplify : C.t -> C.t
  (** Simplify the clause modulo AC *)

  val add_ac : ?proof:Proof.t list -> ID.t -> Type.t -> unit
  (** Declare that the given symbol is AC, and update the Env subsequently
      by adding clauses, etc. *)

  val setup : unit -> unit
  (** Register on Env *)
end

module Make(Env: Env.S) : S with module Env = Env

val extension : Extensions.t

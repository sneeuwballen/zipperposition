
(* This file is free software, part of Zipperposition. See file "license" for more details. *)

(** {1 Inference and simplification rules for Algebraic types} *)

open Libzipperposition

type term = FOTerm.t

(** {2 Inference rules} *)

module type S = sig
  module Env : Env.S
  module C : module type of Env.C

  val declare_type : proof:C.t ProofStep.of_ -> ty:Type.t -> var:Type.t HVar.t -> term list -> unit
  (** Declare that the given type's domain is the given list of cases
      for the given variable [var] (whose type must be [ty].
      Will be ignored if the type already has a enum declaration. *)

  val instantiate_vars : Env.multi_simpl_rule
  (** Instantiate variables whose type is a known enumerated type,
      with all variables of this type. *)

  (** {6 Registration} *)

  val register : unit -> unit
  (** Register rules in the environment *)
end

module Make(E : Env.S) : S with module Env = E

(** {2 As Extension} *)

val extension : Extensions.t

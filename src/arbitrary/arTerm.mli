
(* This file is free software, part of Zipperposition. See file "license" for more details. *)

(** {1 Arbitrary Typed Terms and Formulas} *)

open Logtk

type 'a arbitrary = 'a QCheck.arbitrary
type 'a gen = 'a QCheck.Gen.t

val shrink : FOTerm.t QCheck.Shrink.t

val default_g : FOTerm.t gen

val default_fuel : int -> FOTerm.t gen

val default : FOTerm.t arbitrary
(** Default polymorphic term *)

val ground_g : FOTerm.t gen

val ground : FOTerm.t arbitrary
(** Default ground monomorphic term *)

val pred : FOTerm.t arbitrary
(** predicates (type "o") *)

val pos : FOTerm.t -> Position.t gen
(** Random valid position in the term *)

(** {2 S Terms} *)

module PT : sig
  val shrink : TypedSTerm.t QCheck.Shrink.t

  val default_fuel : int -> TypedSTerm.t gen
  val default_g : TypedSTerm.t gen
  val default : TypedSTerm.t arbitrary
  (** Default polymorphic term *)

  val ground_g : TypedSTerm.t gen
  val ground : TypedSTerm.t arbitrary
  (** Default ground monomorphic term *)

  val pred_g : TypedSTerm.t gen
  val pred : TypedSTerm.t arbitrary
  (** predicates (type "prop") *)
end

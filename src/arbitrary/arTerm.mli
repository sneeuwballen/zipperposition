
(* This file is free software, part of Zipperposition. See file "license" for more details. *)

(** {1 Arbitrary Typed Terms and Formulas} *)

open Logtk

type 'a arbitrary = 'a QCheck.arbitrary
type 'a gen = 'a QCheck.Gen.t

val shrink : Term.t QCheck.Shrink.t

val default_g : Term.t gen

val default_fuel : int -> Term.t gen

val default : Term.t arbitrary
(** Default polymorphic term *)

val default_ho_g : Term.t gen

val default_ho : Term.t arbitrary
(** Default polymorphic term, with lambdas *)

val default_ho_fuel : int -> Term.t gen

val ground_g : Term.t gen

val ground : Term.t arbitrary
(** Default ground monomorphic term *)

val pred : Term.t arbitrary
(** predicates (type "o") *)

val pos : Term.t -> Position.t gen
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

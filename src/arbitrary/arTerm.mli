
(* This file is free software, part of Zipperposition. See file "license" for more details. *)

(** {1 Arbitrary Typed Terms and Formulas} *)

open Libzipperposition

type 'a arbitrary = 'a QCheck.Arbitrary.t

val default : FOTerm.t arbitrary
(** Default polymorphic term *)

val default_fuel : int -> FOTerm.t arbitrary

val ground : FOTerm.t arbitrary
(** Default ground monomorphic term *)

val pred : FOTerm.t arbitrary
(** predicates (type "o") *)

val pos : FOTerm.t -> Position.t arbitrary
(** Random valid position in the term *)

module HO : sig
  val ground : HOTerm.t arbitrary
  (** Ground HO terms *)

  val default : HOTerm.t arbitrary
  (** HO polymorphic term *)
end

(** {2 S Terms} *)

module PT : sig
  val default : TypedSTerm.t arbitrary
  (** Default polymorphic term *)

  val ground : TypedSTerm.t arbitrary
  (** Default ground monomorphic term *)

  val pred : TypedSTerm.t arbitrary
  (** predicates (type "prop") *)
end

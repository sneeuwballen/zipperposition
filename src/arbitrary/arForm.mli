
(* This file is free software, part of Zipperposition. See file "license" for more details. *)

(** {1 Arbitrary Basic Terms} *)

open Logtk

type 'a arbitrary = 'a QCheck.arbitrary
type 'a gen = 'a QCheck.Gen.t
type form = TypedSTerm.t

val atom_g : form gen

val atom : form arbitrary
(** Atomic formula *)

val clause_g : form list gen

val clause : form list arbitrary
(** clause *)

val default : form arbitrary
(** polymorphic formula with connectives (DB-closed) *)

val default_fuel : int -> form gen

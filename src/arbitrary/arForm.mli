
(* This file is free software, part of Zipperposition. See file "license" for more details. *)

(** {1 Arbitrary Basic Terms} *)

open Libzipperposition

type 'a arbitrary = 'a QCheck.Arbitrary.t
type form = TypedSTerm.t

val atom : form arbitrary
(** Atomic formula *)

val clause : form list arbitrary
(** clause *)

val default : form arbitrary
(** polymorphic formula with connectives (DB-closed) *)

val default_fuel : int -> form arbitrary

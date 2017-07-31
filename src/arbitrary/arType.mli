
(* This file is free software, part of Zipperposition. See file "license" for more details. *)

(** {1 Arbitrary Generation of Types} *)

open Logtk

type 'a arbitrary = 'a QCheck.arbitrary
type 'a gen = 'a QCheck.Gen.t

val base_g : Type.t gen
val base : Type.t arbitrary
(** Random base symbol *)

val ground_g : Type.t gen
val ground : Type.t arbitrary
(** Ground type *)

val default_g : Type.t gen
val default : Type.t arbitrary
(** Any type (polymorphic) *)

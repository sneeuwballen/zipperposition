
(* This file is free software, part of Zipperposition. See file "license" for more details. *)

(** {1 Arbitrary generation of Identifiers} *)

open Logtk

type 'a arbitrary = 'a QCheck.arbitrary
type 'a gen = 'a QCheck.Gen.t

val default_g : ID.t gen
val default : ID.t arbitrary
(** Set of symbols *)

val set_g : ID.Set.t gen
val set : ID.Set.t arbitrary
(** Arbitrary set of various symbols *)

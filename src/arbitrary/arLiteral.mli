
(* This file is free software, part of Zipperposition. See file "license" for more details. *)

(** {1 Arbitrary Literals} *)

open Logtk

type 'a arbitrary = 'a QCheck.arbitrary
type 'a gen = 'a QCheck.Gen.t
type lit = Literal.t
type clause = Literals.t

val lit_g : lit gen

val lit : lit arbitrary

val clause_g : clause gen

val clause : clause arbitrary
(** clause *)

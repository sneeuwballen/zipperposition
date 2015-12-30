
(* This file is free software, part of Zipperposition. See file "license" for more details. *)

(** {1 Arbitrary generation of symbols} *)

open Libzipperposition

type 'a arbitrary = 'a QCheck.Arbitrary.t

val default : ID.t arbitrary
(** Set of symbols *)

val set : ID.Set.t arbitrary
(** Arbitrary set of various symbols *)

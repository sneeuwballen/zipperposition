
(* This file is free software, part of Zipperposition. See file "license" for more details. *)

(** {1 Test Properties} *)

(** Test universal properties that use defined functions,
    within some "reasonable" bound (e.g. smallcheck/quickcheck) *)

type term = FOTerm.t
type lit = term SLiteral.t
type form = lit list list

type res =
  | R_ok
  | R_fail of Subst.t (* counter-example *)

val small_check : ?depth:int -> form -> res
(** [smallcheck form] returns [R_ok] if the property seems to hold
    up to [depth], or [R_fail subst] if [subst] makes [form]
    evaluate to [false] *)


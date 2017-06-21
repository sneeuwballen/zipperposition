
(* This file is free software, part of Zipperposition. See file "license" for more details. *)

(** {1 Test Properties} *)

(** Test universal properties that use defined functions,
    within some "reasonable" bound (e.g. smallcheck/quickcheck).

    This is used to test a property semantically (by evaluation)
    before trying the costly proof by induction; if the property is
    false there is no need to try to prove it.
*)

type term = Term.t
type lit = Literal.t
type form = Literals.t list

type res =
  | R_ok
  | R_fail of Subst.t (* counter-example *)

val normalize_form : form -> form
(** Use rewriting to normalize the formula *)

val check_form : ?limit:int -> form -> res
(** [check_form rules form] returns [R_ok] if the property seems to hold
    up to [depth], or [R_fail subst] if [subst] makes [form]
    evaluate to [false]
    @param limit a limit on how many "steps" are done (for some notion of steps).
    The higher, the more expensive, but also the more accurate this is. *)

val default_limit: int


(* This file is free software, part of Zipperposition. See file "license" for more details. *)

(** {1 Test Properties} *)

(** Test universal properties that use defined functions,
    within some "reasonable" bound (e.g. smallcheck/quickcheck) *)

type term = FOTerm.t
type lit = term SLiteral.t
type form = lit list list

(* TODO: replace lit = Literal.t, form = Literals.t list
   (use corresponding functions for trivial/absurd/subst) *)

(* TODO: check_form should take a Lit_rewrite.set *)

type res =
  | R_ok
  | R_fail of Subst.t (* counter-example *)

val check_form : ?limit:int -> form -> res
(** [check_form form] returns [R_ok] if the property seems to hold
    up to [depth], or [R_fail subst] if [subst] makes [form]
    evaluate to [false]
    @param limit a limit on how many "steps" are done (for some notion of steps).
    The higher, the more expensive, but also the more accurate this is. *)

val default_limit: int

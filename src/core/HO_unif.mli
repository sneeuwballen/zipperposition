
(* This file is free software, part of Zipperposition. See file "license" for more details. *)

(** {1 Higher-Order Unification} *)

type term = Term.t

type penalty = int
(** penalty on the search space *)

val enum_prop :
  Term.var Scoped.t ->
  offset:int ->
  (Subst.t * penalty) list
(** Given a variable of type [τ1…τn -> prop], enumerate possible shapes
    for it
    @param v the variable to refine + its scope. Must return [prop].
    @param offset to create fresh variables (should be unused elsewhere)
*)

val unif_pairs :
  ?fuel:int ->
  (term * term) list Scoped.t ->
  offset:int ->
  ((term * term) list * Subst.t * penalty) list
(** [unif_pairs pairs ~scope_new_vars] returns a list of (partial) solutions
    to the HO unification problem [pairs].
    Each solution is a list of remaining constraints, a substitution,
    and some penalty to influence the search space *)


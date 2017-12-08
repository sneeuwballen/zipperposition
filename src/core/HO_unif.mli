
(* This file is free software, part of Zipperposition. See file "license" for more details. *)

(** {1 Higher-Order Unification} *)

type term = Term.t

type penalty = int
(** penalty on the search space *)

val enum_prop :
  ?mode:[`Full | `Neg | `None] ->
  Term.var Scoped.t ->
  offset:int ->
  (Subst.t * penalty) list
(** Given a variable of type [τ1…τn -> prop], enumerate possible shapes
    for it
    @param v the variable to refine + its scope. Must return [prop].
    @param offset to create fresh variables (should be unused elsewhere)
    @param mode if [`Neg], only tries negation; [`None], do nothing;
      otherwise do all connectives
*)

type pair = Type.t list * term * term
(** unification pair *)

val pp_pair : pair CCFormat.printer

val unif_pairs :
  ?fuel:int ->
  pair list Scoped.t ->
  offset:int ->
  (pair list * Unif_subst.t * penalty * Subst.Renaming.t) list
(** [unif_pairs pairs ~scope_new_vars] returns a list of (partial) solutions
    to the HO unification problem [pairs].
    Each solution is a list of remaining constraints (with the substitution already applied),
    a substitution, some penalty to influence the search space,
    and a renaming used for the substitution *)

val default_fuel : int ref
(** Default amount of fuel for {!unif_pairs} *)

val enable_norm_subst : bool ref
(** If true, substitutions obtained with {!unif_pairs}
    are normalized and β-reduced *)

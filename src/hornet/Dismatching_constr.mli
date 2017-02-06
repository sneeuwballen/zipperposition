
(* This file is free software, part of Zipperposition. See file "license" for more details. *)

(** {1 Dismatching Constraint} *)

(** A constraint that specifies that a list of terms [t1, …, tn]
    must not match terms [u1, …, un].
    Variables in the [u_i] live in a distinct scope than variables
    in the [t_i]. *)

open Libzipperposition

type term = FOTerm.t

type t

val make : (term * term) list -> t
(** [make [t_1,u_1; …; t_n,u_n]]
    makes a dismatching constraint that is satisfied for every
    substitution [sigma] such that at least one [t_i\sigma] does not
    match [u_i]. *)

val combine : t -> t -> t

val apply_subst :
  renaming:Subst.Renaming.t ->
  Subst.t ->
  t Scoped.t ->
  t
(** Apply a substitution [sigma] to the constraints. The constraint
    might become trivial as a result. *)

val find_solution : t -> Subst.t option
(** Find a solution to the problem, if possible.
    The substitution binds variables of the LHS terms in scope 0 *)

val is_sat : t -> bool
(** Is the constraint still satisfiable? *)

val is_trivial : t -> bool
(** Is the constraint trivially satisfied? (i.e. always true) *)

include Interfaces.PRINT with type t := t


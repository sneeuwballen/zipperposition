
(* This file is free software, part of Zipperposition. See file "license" for more details. *)

(** {1 Higher-Order Unification} *)

type term = Term.t

type penalty = int
(** penalty on the search space *)

val enum_prop : Term.var Scoped.t -> scope_new_vars:Scoped.scope -> (Subst.t * penalty) list
(** Given a variable of type [τ1…τn -> prop], enumerate possible shapes
    for it
    @param v the variable to refine + its scope. Must return [prop].
    @param scope_new_vars the scope for fresh variables (should be unused elsewhere)
*)

(* TODO *)

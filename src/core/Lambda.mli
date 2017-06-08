
(* This file is free software, part of Libzipperposition. See file "license" for more details. *)

(** {1 Lambda-Calculus} *)

type term = Term.t

val whnf : term -> term
(** Beta-reduce the term into its weak head normal form *)

val whnf_list : term -> term list -> term
(** Apply a lambda to a list of arguments.
    The type of the lambda must be a generalization of a function
    that takes the list's types as arguments.

    @raise Type.ApplyError if the first term doesn't have a function type or
      if the types are not compatible *)

val snf : term -> term
(** Strong normal form, computing under lambdas and subterms *)

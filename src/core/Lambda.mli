
(* This file is free software, part of Libzipperposition. See file "license" for more details. *)

(** {1 Lambda-Calculus} *)

type term = Term.t

val whnf : term -> term
(** Beta-reduce the term into its weak head normal form *)

val beta_red_head : term -> term

val whnf_list : term -> term list -> term
(** Apply a lambda to a list of arguments.
    The type of the lambda must be a generalization of a function
    that takes the list's types as arguments.

    @raise Type.ApplyError if the first term doesn't have a function type or
      if the types are not compatible *)

val snf : term -> term
(** Strong normal form, computing under lambdas and subterms *)

val eta_expand : term -> term
(** Traverse the term, eta-expanding all sub-terms.
    A term [t : a -> b] becomes [fun (x:a). t x] *)

val eta_reduce : ?full:bool -> term -> term
(** Traverse the term, eta-reducing all sub-terms.
    A term [fun x. t x] where [x ∉ vars(t)] becomes [t].
    If full is false, it eta-reduces only at the top level (default: true) *)

val is_lambda_pattern : term -> bool

val is_properly_encoded : term -> bool


(** Low level interface *)
module Inner : sig
  type term = InnerTerm.t

  val whnf : term -> term

  val snf : term -> term

  val eta_expand : term -> term

  val eta_reduce : ?full:bool -> term -> term

  val beta_red_head : term -> term
end

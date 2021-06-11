
(* This file is free software, part of Zipperposition. See file "license" for more details. *)

(** {1 Term Orderings} *)

(** Term orderings are well-founded orderings on terms, that
    (usually) have some nice properties for Superposition, such as being
    total on ground terms, stable by substitution,
    and stable by context (monotonic).

    We provide several classic orderings, such as RPO and KBO.
*)

(* TODO: compute orderings modulo a substitution *)

(** {2 Type definitions} *)

type term = Term.t

type t
(** Partial ordering on terms *)

type ordering = t

val compare : t -> term -> term -> Comparison.t
(** Compare two terms using the given ordering *)

val might_flip : t -> term -> term -> bool
(** Returns false for two terms t and s if for any ground substitution θ the ordering
    of tθ vs sθ cannot change when appending arguments. This function is allowed
    to overapproximate, i.e. we get no information if it returns true. *)

val monotonic : t -> bool	
(** Is the ordering fully monotonic? Is it in particular compatible with arguments,	
    i.e., t > s ==> t a > s a *)	

val precedence : t -> Precedence.t
(** Current precedence *)

val add_list : signature:Signature.t -> t -> ID.t list -> unit
(** Update precedence with symbols *)

val name : t -> string
(** Name that describes this ordering *)

val clear_cache : t -> unit

include Interfaces.PRINT with type t := t

(** {2 Ordering implementations}
    An ordering is a partial ordering on terms. Several implementations
    are simplification orderings (compatible with substitution,
    with the subterm property, and monotonic), some other are not. *)

val lambda_kbo : ignore_quans_under_lam:bool -> Precedence.t -> t
(** Knuth-Bendix simplification ordering *)

val lambda_rpo : Precedence.t -> t
(** Efficient implementation of RPO (recursive path ordering) *)

val compose : (term -> term -> (Comparison.t*term*term)) -> t -> t
(** Takes a function that is going to be run before the chosen order and the order.
    If the first argument returns Comparison.Eq, then the order determined by second arg.
    Otherwise, the result of the first argument is returned. *) 
val lambdafree_kbo : Precedence.t -> t
(** Knuth-Bendix simplification ordering - lambdafree version *)

val lambdafree_rpo : Precedence.t -> t
(** Efficient implementation of RPO (recursive path ordering) - lambdafree version *)

val epo : Precedence.t -> t
(** Embedding path order *)

val none : t
(** All terms are incomparable (equality still works).
    Not a simplification ordering. *)

val subterm : t
(** Subterm ordering. Not a simplification ordering. *)

val map : (term -> term) -> t -> t

(** {2 Global table of Orders} *)

val default_of_list : ID.t list -> t
(** default ordering on terms (RPO6) using default precedence *)

val default_of_prec : Precedence.t -> t

val by_name : string -> Precedence.t -> t
(** Choose ordering by name among registered ones, or
    @raise Invalid_argument if no ordering with the given name are registered. *)

val names : unit -> string list

val register : string -> (Precedence.t -> t) -> unit
(** Register a new ordering, which can depend on a precedence.
    The name must not be registered already.
    @raise Invalid_argument if the name is already used. *)

(* Type-1 combinator is a combinator that is not ground
     (see Ahmed's combinator KBO paper) *)
val ty1comb_to_var : Term.t -> Term.t Term.Tbl.t -> Term.t

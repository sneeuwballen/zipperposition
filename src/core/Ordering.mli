
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

val precedence : t -> Precedence.t
(** Current precedence *)

val add_list : t -> ID.t list -> unit
(** Update precedence with symbols *)

val add_seq : t -> ID.t Sequence.t -> unit
(** Update precedence with signature *)

val name : t -> string
(** Name that describes this ordering *)

val clear_cache : t -> unit

include Interfaces.PRINT with type t := t

(** {2 Ordering implementations}
    An ordering is a partial ordering on terms. Several implementations
    are simplification orderings (compatible with substitution,
    with the subterm property, and monotonic), some other are not. *)

val kbo : Precedence.t -> t
(** Knuth-Bendix simplification ordering (Blanchette's lambda-free higher-order version) *)

val lfhokbo_arg_coeff : Precedence.t -> t
(** Blanchette's lambda-free higher-order KPO with argument coefficients *)

val rpo6 : Precedence.t -> t
(** Efficient implementation of RPO (recursive path ordering) (Blanchette's lambda-free higher-order version)  *)

val none : t
(** All terms are incomparable (equality still works).
    Not a simplification ordering. *)

val subterm : t
(** Subterm ordering. Not a simplification ordering. *)

(** {2 Global table of Orders} *)

val default_of_list : ID.t list -> t
(** default ordering on terms (RPO6) using default precedence *)

val default_of_prec : Precedence.t -> t

val by_name : string -> Precedence.t -> t
(** Choose ordering by name among registered ones, or
    @raise Invalid_argument if no ordering with the given name are registered. *)

val names : unit -> string list
val default_name : string

val register : string -> (Precedence.t -> t) -> unit
(** Register a new ordering, which can depend on a precedence.
    The name must not be registered already.
    @raise Invalid_argument if the name is already used. *)

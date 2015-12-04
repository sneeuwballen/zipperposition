
(* This file is free software, part of Logtk. See file "license" for more details. *)

module type S = sig
  module Prec : Precedence.S with type symbol = ID.t

  type term

  type t
  (** Partial ordering on terms *)

  type ordering = t

  val compare : t -> term -> term -> Comparison.t
  (** Compare two terms using the given ordering *)

  val precedence : t -> Prec.t
  (** Current precedence *)

  val set_precedence : t -> Prec.t -> t
  (** Change the precedence. The new one must be a superset of the old one.
      @raise Invalid_argument if the new precedence is not compatible
        with the old one *)

  val update_precedence : t -> (Prec.t -> Prec.t) -> t
  (** Update the precedence with a function.
      @raise Invalid_argument if the new precedence is not compatible
        with the previous one (see {!set_precedence}). *)

  val add_list : t -> ID.t list -> t
  (** Update precedence with symbols *)

  val add_seq : t -> ID.t Sequence.t -> t
  (** Update precedence with signature *)

  val name : t -> string
  (** Name that describes this ordering *)

  val clear_cache : t -> unit

  include Interfaces.PRINT with type t := t

  (** {2 Ordering implementations}
      An ordering is a partial ordering on terms. Several implementations
      are simplification orderings (compatible with substitution,
      with the subterm property, and monotonic), some other are not. *)

  val kbo : Prec.t -> t
  (** Knuth-Bendix simplification ordering *)

  val rpo6 : Prec.t -> t
  (** Efficient implementation of RPO (recursive path ordering) *)

  val none : t
  (** All terms are incomparable (equality still works).
      Not a simplification ordering. *)

  val subterm : t
  (** Subterm ordering. Not a simplification ordering. *)

  (** {2 Global table of Orders} *)

  val default_of_list : ID.t list -> t
  (** default ordering on terms (RPO6) using default precedence *)

  val default_of_prec : Prec.t -> t

  val by_name : string -> Prec.t -> t
  (** Choose ordering by name among registered ones, or
      @raise Invalid_argument if no ordering with the given name are registered. *)

  val register : string -> (Prec.t -> t) -> unit
  (** Register a new ordering, which can depend on a precedence.
      The name must not be registered already.
      @raise Invalid_argument if the name is already used. *)
end

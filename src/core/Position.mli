(* This file is free software, part of Logtk. See file "license" for more details. *)

(** {1 Positions in terms, clauses...} *)

(** Positions are used to indicate a given occurrence of an object
    in a tree-like structure.

    Typically, we use positions to refer to a particular occurrence of
    a term in another (bigger) term, or in a literal, or in a clause.

    A pair of {term,clause,literal} + position represents a  context,
    that is, a {term,clause,literal} with a hole at the given position,
    where we can put a different term.
*)

(** A position is a path in a tree *)
type t =
  | Stop
  | Type of t (** Switch to type *)
  | Left of t (** Left term in curried application *)
  | Right of t (** Right term in curried application, and subterm of binder *)
  | Head of t (** Head of uncurried term *)
  | Arg of int * t (** argument term in uncurried term, or in multiset *)
  | Body of t (** Body of binder or horn clause *)

type position = t

val stop : t
val type_ : t -> t
val left : t -> t
val right : t -> t
val head : t -> t
val body : t -> t
val arg : int -> t -> t

val opp : t -> t
(** Opposite position, when it makes sense (left/right) *)

val rev : t -> t
(** Reverse the position *)

val append : t -> t -> t
(** Append two positions *)

val is_prefix : t -> t -> bool
(** [is_prefix a b] is true iff [a] is a prefix of [b] *)

val is_strict_prefix : t -> t -> bool
(** [is_prefix a b] is true iff [a] is a prefix of [b] and [a != b] *)

val compare : t -> t -> int
val equal : t -> t -> bool
val hash : t -> int

include Interfaces.PRINT with type t := t

module Map : sig
  include CCMap.S with type key = t

  val prune_subsumed : 'a t -> 'a t
  (** Remove the keys that are below other keys in the map *)
end

(** {2 Position builder} *)

module Build : sig
  type t

  val empty : t
  (** Empty builder (position=[Stop]) *)

  val to_pos : t -> position
  (** Extract current position *)

  val of_pos : position -> t
  (** Start from a given position *)

  val prefix : position -> t -> t
  (** Prefix the builder with the given position *)

  val suffix : t -> position -> t
  (** Append position at the end *)

  (** All the following builders add elements to the {b end}
      of the builder. This is useful when a term is traversed and
      positions of subterms are needed, since positions are
      easier to build in the wrong order (leaf-to-root). *)

  val type_ : t -> t

  val left : t -> t
  (** Add [left] at the end *)

  val right : t -> t
  (** Add [left] at the end *)

  val body : t -> t

  val head : t -> t

  val arg : int -> t -> t
  (** Arg position at the end *)

  include Interfaces.PRINT with type t := t
end

(** {2 Pairing of value with Pos} *)

(** Positions act a bit like lenses, in the sense that they compose
    nicely and designat paths in objects *)

module With : sig
  type 'a t = 'a * position
  (** A pair of ['a] and position (builder). *)

  val get : 'a t -> 'a
  val pos : _ t -> position

  val make : 'a -> position -> 'a t
  val of_pair : 'a * position -> 'a t

  val map_pos : (position -> position) -> 'a t -> 'a t

  val map : ('a -> 'b) -> 'a t -> 'b t

  module Infix : sig
    val (>|=) : 'a t -> ('a -> 'b) -> 'b t
  end
  include module type of Infix

  val equal : ('a -> 'a -> bool) -> 'a t -> 'a t -> bool
  val compare : ('a -> 'a -> int) -> 'a t -> 'a t -> int
  val hash : ('a -> int) -> 'a t -> int
  val pp : 'a CCFormat.printer -> 'a t CCFormat.printer
end

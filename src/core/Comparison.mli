
(* This file is free software, part of Logtk. See file "license" for more details. *)

(** {1 Partial Ordering values} *)

type t = Lt | Eq | Gt | Incomparable
(** partial order *)

type comparison = t

include Interfaces.PRINT with type t := t

val combine : t -> t -> t
(** combine two partial comparisons, that are assumed to be
    compatible, ie they do not order differently if
    Incomparable is not one of the values.
    @raise Invalid_argument if the comparisons are inconsistent. *)

val opp : t -> t
(** Opposite of the relation: a R b becomes b R a *)

val to_total : t -> int
(** Conversion to a total ordering. Incomparable is translated
    to 0 (equal). *)

val of_total : int -> t
(** Conversion from a total order *)

val lexico : t -> t -> t
(** Lexicographic combination (the second is used only if the first
    is [Incomparable] *)

val (++) : t -> t -> t
(** Infix version of {!lexico} *)

type 'a comparator = 'a -> 'a -> t

val (@>>) : 'a comparator -> 'a comparator -> 'a comparator
(** Combination of comparators that work on the same values.
    Its behavior is the following:
    [(f1 @>> f2) x y] is the same as [f1 x y] if
    [f1 x y] is not [Eq]; otherwise it is the same as [f2 x y] *)

type ('a, 'b) combination
(** Lexicographic combination of comparators. It is, roughly,
    equivalent to ['a -> 'a -> 'b] *)

val (>>>) : 'a comparator -> ('b, 'c) combination -> ('a, 'b -> 'b -> 'c) combination
(** Lexicographic combination starting with the given function *)

val last : 'a comparator -> ('a, t) combination
(** Last comparator *)

val call : ('a, 'b) combination -> 'a -> 'a -> 'b
(** Call a lexicographic combination on arguments *)

val dominates : ('a -> 'b -> t) -> 'a list -> 'b list -> bool
(** [dominates f l1 l2] returns [true] iff for all element [y] of [l2],
    there is some [x] in [l1] with [f x y = Gt] *)

module type PARTIAL_ORD = sig
  type t

  val partial_cmp : t -> t -> comparison
end

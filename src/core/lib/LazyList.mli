
(* This file is free software, part of Zipperposition. See file "license" for more details. *)

(** {1 Lazy List} *)

(** Used for iterating lazily over a stream (possibly infinite).
    The point of using ['a lazy_t] for the tail is that once a list node
    is forced (evaluated), it is memoized and will not be re-computed
    if accessed again. This is important for lazy lists of elements
    that are expensive to compute. *)

type 'a node =
  | Nil
  | Cons of 'a * 'a t
and 'a t = 'a node Lazy.t

val nil : 'a t

val cons : 'a -> 'a t -> 'a t

val take : int -> 'a t -> 'a t

val of_fun : (int -> 'a option) -> 'a t

val to_list : 'a t -> 'a list
(** Gather all values into a list *)

val to_seq : 'a t -> 'a Sequence.t
(** Iterate on values *)

val fold : ('a -> 'b -> 'a) -> 'a -> 'b t -> 'a
(** Fold on values *)

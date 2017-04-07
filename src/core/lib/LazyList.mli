
(* This file is free software, part of Zipperposition. See file "license" for more details. *)

(** {1 Lazy List} *)

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


(* This file is free software, part of Zipperposition. See file "license" for more details. *)

(** {1 Simplification Monad}

    This monad is used to combine simplifications in a way that allows
    to know if at least one simplification was performed, or not. *)

type +'a t = 'a * [ `Same | `New]

val return_same : 'a -> 'a t
val return_new : 'a -> 'a t

val return : 'a -> 'a t
(** Alias to {!return_same} *)

val return_opt : old:'a -> 'a option -> 'a t
(** [return_opt ~old t] returns [return_new u] if [t=Some u], else returns [same old]. *)

val get : 'a t -> 'a

val is_new : _ t -> bool
val is_same : _ t -> bool

val (>>=) : 'a t -> ('a -> 'b t) -> 'b t
(** Monadic bind *)

val map : ('a -> 'b) -> 'a t -> 'b t

val app_list : ('a -> 'a t) list -> 'a -> 'a t

val map_l : ('a -> 'b t) -> 'a list -> 'b list t

val fold_l : ('a -> 'b -> 'a t) -> 'a -> 'b list -> 'a t

module Infix : sig
  val (>>=) : 'a t -> ('a -> 'b t) -> 'b t
  val (>|=) : 'a t -> ('a -> 'b) -> 'b t
end

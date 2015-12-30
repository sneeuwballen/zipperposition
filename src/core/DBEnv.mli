
(* This file is free software, part of Zipperposition. See file "license" for more details. *)

(** {1 De Bruijn environments} *)

type +'a t
(** An environment that maps De Bruijn indices to values of
    type 'a. *)

val empty : 'a t
(** Empty environment *)

val is_empty : 'a t -> bool
(** Are there bindings? *)

val make : int -> 'a t
(** Empty environment of the given size *)

val singleton : 'a -> 'a t
(** Single binding *)

val push : 'a t -> 'a -> 'a t
(** Create a new environment, when entering a scope, where the De Bruijn
    index 0 is bound to the given value *)

val push_none : 'a t -> 'a t
(** Create a new environment, when entering a scope, where
    the De Bruijn index 0 is bound to nothing. *)

val push_none_multiple : 'a t -> int -> 'a t
(** Call [push_none] [n] times (after we've entered [n] scopes, for
    instances) *)

val pop : 'a t -> 'a t
(** Exit a scope, removing the top binding.
    @raise Invalid_argument if the env is empty *)

val pop_many : 'a t -> int -> 'a t
(** [pop_many env n] calls [pop env] [n] times *)

val size : 'a t -> int
(** Number of scopes (number of times {!push} or {!push_none} were
    called to produce the given environement) *)

val find : 'a t -> int -> 'a option
(** Find to which value the given De Bruijn index is bound to, or
    return None *)

val find_exn : 'a t -> int -> 'a
(** Unsafe version of {!find}.
    @raise Failure if the index is not bound within [env] *)

val mem : _ t -> int -> bool
(** [mem env i] returns [true] iff [find env i] returns [Some _]
    rather than [None], ie. whether the [i]-th De Bruijn variable
    is bound within [env] *)

val set : 'a t -> int -> 'a -> 'a t
(** Set the [n]-th variable to the given objects.
    @raise Invalid_argument if the index isn't in the range [0... size-1] *)

val num_bindings : _ t -> int
(** How many variables are actually bound? *)

val map : ('a -> 'b) -> 'a t -> 'b t
(** Map bound objects to other bound objects *)

val of_list : (int * 'a) list -> 'a t
(** Map indices to objects *)

include Interfaces.PRINT1 with type 'a t := 'a t

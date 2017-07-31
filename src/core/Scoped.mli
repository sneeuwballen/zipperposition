

(* This file is free software, part of Logtk. See file "license" for more details. *)

(** {1 Scoped Value} *)

(** A scoped value is a pair of the value and an (integer) scope.
    The value usually contains variables, but a value in a scope A
    share no variables with the same value in any scope B ≠ A.

    This makes it possible to use an object (e.g. a clause) in
    two distinct scopes. Each scoped version of the object shares
    no variable with the other version; hence, there is no need
    for copying the object.
*)

type scope = int
type +'a t = 'a * scope

val make : 'a -> int -> 'a t

val get : 'a t -> 'a
val scope : _ t -> int

val set : 'a t -> 'b -> 'b t
(** [set v x] is [x] with the same scope as [v] *)

val same_scope : _ t -> _ t -> bool

val equal : ('a -> 'a -> bool) -> 'a t -> 'a t -> bool
val compare : 'a CCOrd.t -> 'a t CCOrd.t
val hash : 'a Hash.t -> 'a t -> int

val map : ('a -> 'b) -> 'a t -> 'b t

val on : ('a -> 'b) -> 'a t -> 'b
val on2 : ('a -> 'b -> 'c) -> 'a t -> 'b t -> 'c

val pp : 'a CCFormat.printer -> 'a t CCFormat.printer
val to_string : 'a CCFormat.printer -> 'a t -> string





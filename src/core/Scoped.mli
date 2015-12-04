

(* This file is free software, part of Logtk. See file "license" for more details. *)

(** {1 Scoped Value}

  A scoped value is a pair of the value and an (integer) scope. 
  The value contains variables, but a value in a scope A
  share no variables with the same value in any scope B ≠ A.

*)

type 'a t = private {
  value: 'a;
  scope: int;
}

val make : int -> 'a -> 'a t

val get : 'a t -> 'a
val scope : _ t -> int

val set : 'a t -> 'a -> 'a t
(** [set v x] is [x] with the same scope as [v] *)

val same_scope : _ t -> _ t -> bool
val equal : ('a -> 'a -> bool) -> 'a t -> 'a t -> bool

val map : ('a -> 'b) -> 'a t -> 'b t

val on : ('a -> 'b) -> 'a t -> 'b
val on2 : ('a -> 'b -> 'c) -> 'a t -> 'b t -> 'c

val pp : 'a CCFormat.printer -> 'a t CCFormat.printer
val to_string : 'a CCFormat.printer -> 'a t -> string





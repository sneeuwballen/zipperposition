
(* This file is free software, part of Logtk. See file "license" for more details. *)

(** {1 Hashconsed Variable}

    A variable for hashconsed terms, paired with a type.
    Such a ['ty HVar.t] is really a pair [(int, 'ty)]: the integer
    is used to be able to have several variables in the same clause,
    the type is because in typed logic we must know the type of variables
    before unifying/binding them.
*)

type +'a t = private {
  id: int;
  ty: 'a;
}
type 'a hvar = 'a t

val make : ty:'a -> int -> 'a t
val id : _ t -> int
val ty : 'a t -> 'a

val cast : 'a t -> ty:'b -> 'b t
val update_ty : 'a t -> f:('a -> 'b) -> 'b t

val compare : ('a -> 'a -> int) -> 'a t -> 'a t -> int
val equal : ('a -> 'a -> bool) -> 'a t -> 'a t -> bool
val hash : _ t -> int

val max : 'a t -> 'a t -> 'a t
val min : 'a t -> 'a t -> 'a t

val pp : _ t CCFormat.printer
val pp_tstp : _ t CCFormat.printer
val to_string : _ t -> string
val to_string_tstp : _ t -> string

(**/**)
val make_unsafe : ty:'a -> int -> 'a t
(** skip checks *)

val fresh : ty:'a -> unit -> 'a t
(** Magic: create a variable with a negative index, mostly for
    unification purpose *)

val is_fresh : _ t -> bool
(** Magic: check if the variable is a fresh one (with negative index) *)


(**/**)


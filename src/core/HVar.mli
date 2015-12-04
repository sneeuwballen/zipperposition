
(* This file is free software, part of Logtk. See file "license" for more details. *)

(** {1 Hashconsed Variable}

  A variable for hashconsed terms, paired with a type.
*)

type 'a t = private {
  id: int;
  ty: 'a;
}
type 'a hvar = 'a t

val make : ty:'a -> int -> 'a t
val id : _ t -> int
val ty : 'a t -> 'a

val cast : 'a t -> ty:'b -> 'b t

val compare : _ t -> _ t -> int
val equal : _ t -> _ t -> bool
val hash : _ t -> int
val hash_fun : _ t CCHash.hash_fun

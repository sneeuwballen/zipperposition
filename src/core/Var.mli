
(* This file is free software, part of Logtk. See file "license" for more details. *)

(** {1 Variable}

  A Variable is a pair of a unique Name, and a type.

  @since NEXT_RELEASE *)

type 'a t = private {
  id: ID.t;
  ty: 'a;
}
type 'a var = 'a t

val make : ty:'a -> ID.t -> 'a t

val of_string : ty:'a -> string -> 'a t
(** Make a fresh ID before calling {!make} *)

val gensym : ty:'a -> unit -> 'a t

val copy : 'a t -> 'a t
(** [copy v] is similar to [v] but with a fresh ID *)

val update_ty : 'a t -> f:('a -> 'b) -> 'b t
(** Make a fresh variable with a new type and same ID *)

val id : _ t -> ID.t
val ty: 'a t -> 'a

val compare : _ t -> _ t -> int
val equal : _ t -> _ t -> bool
val hash : _ t -> int
val hash_fun : _ t CCHash.hash_fun

val pp : _ t CCFormat.printer
val to_string : _ t -> string

module Set : sig
  type +'a t
  val empty : 'a t
  val add : 'a t -> 'a var -> 'a t
  val mem : 'a t -> 'a var -> bool
  val find : 'a t -> ID.t -> 'a var option
  val find_exn : 'a t -> ID.t -> 'a var
  val of_seq : 'a var Sequence.t -> 'a t
  val to_seq : 'a t -> 'a var Sequence.t
  val to_list : 'a t -> 'a var list
  val cardinal : _ t -> int
  val pp : _ t CCFormat.printer
end


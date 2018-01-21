
(* This file is free software, part of Logtk. See file "license" for more details. *)

(** {1 Variable} *)

(** A Variable is a pair of a unique {!ID.t}, and a type.

    It is used in {!TypedSTerm.t} for manipulating free and bound variables.

    @since 1.5 *)

type +'a t = private {
  id: ID.t;
  ty: 'a;
}
type 'a var = 'a t

val make : ty:'a -> ID.t -> 'a t

val of_string : ty:'a -> string -> 'a t
(** Make a fresh ID before calling {!make} *)

val makef : ty:'a -> ('b, Format.formatter, unit, 'a t) format4 -> 'b

val gensym : ty:'a -> unit -> 'a t

val copy : 'a t -> 'a t
(** [copy v] is similar to [v] but with a fresh ID *)

val update_ty : 'a t -> f:('a -> 'b) -> 'b t
(** Make a fresh variable with a new type and same ID *)

val id : _ t -> ID.t
val ty: 'a t -> 'a
val name : _ t -> string

val compare : _ t -> _ t -> int
val equal : _ t -> _ t -> bool
val hash : _ t -> int

val pp : _ t CCFormat.printer
val to_string : _ t -> string
val pp_full : _ t CCFormat.printer
val pp_fullc : _ t CCFormat.printer (** With color *)

module Set : sig
  type +'a t
  val is_empty : _ t -> bool
  val empty : 'a t
  val add : 'a t -> 'a var -> 'a t
  val mem : 'a t -> 'a var -> bool
  val find : 'a t -> ID.t -> 'a var option
  val find_exn : 'a t -> ID.t -> 'a var
  val diff : 'a t -> 'a t -> 'a t
  val of_seq : 'a var Sequence.t -> 'a t
  val to_seq : 'a t -> 'a var Sequence.t
  val add_seq : 'a t -> 'a var Sequence.t -> 'a t
  val add_list : 'a t -> 'a var list -> 'a t
  val to_list : 'a t -> 'a var list
  val of_list : 'a var list -> 'a t
  val cardinal : _ t -> int
  val pp : _ t CCFormat.printer
end

module Subst : sig
  type (+'a, +'b) t
  val empty : (_,_) t
  val is_empty : (_,_) t -> bool
  val singleton : 'a var -> 'b -> ('a,'b) t
  val size: (_,_) t -> int
  val add : ('a,'b) t -> 'a var -> 'b -> ('a,'b) t
  val mem : ('a,_) t -> 'a var -> bool
  val find : ('a,'b) t -> 'a var -> 'b option
  val find_exn : ('a,'b) t -> 'a var -> 'b
  val merge : ('a,'b) t -> ('a,'b) t -> ('a,'b) t
  val of_list : ('a var * 'b) list -> ('a,'b) t
  val of_seq : ('a var * 'b) Sequence.t -> ('a,'b) t
  val to_list : ('a,'b) t -> ('a var * 'b) list
  val to_seq: ('a,'b) t -> ('a var * 'b) Sequence.t
  val pp : 'b CCFormat.printer -> (_, 'b) t CCFormat.printer
end


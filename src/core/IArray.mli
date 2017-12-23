
(* This file is free software, part of Zipperposition. See file "license" for more details. *)

(** {1 Immutable Arrays} *)

(** An API over arrays that doesn't expose any mutating function. This
    type ['a IArray.t] benefits from good cache locality and fast
    access to items, but still is functional. However, most operations
    that create new arrays are linear in time (they copy/traverse the whole
    input). *)

type 'a equal = 'a -> 'a -> bool
type 'a ord = 'a -> 'a -> int

type 'a t
(** Array of values of type 'a *)

val of_list : 'a list -> 'a t

val to_list : 'a t -> 'a list

val to_array_copy : 'a t -> 'a array
(** make a copy into a mutable array *)

val to_array_unsafe : 'a t -> 'a array
(** Show the underlying array. DO NOT MODIFY *)

val of_array_unsafe : 'a array -> 'a t
(** Take ownership of the given array. Careful, the array must {b NOT}
    be modified afterwards! *)

val empty : 'a t

val length : _ t -> int

val singleton : 'a -> 'a t

val doubleton : 'a -> 'a -> 'a t

val make : int -> 'a -> 'a t

val init : int -> (int -> 'a) -> 'a t

val get : 'a t -> int -> 'a

val set : 'a t -> int -> 'a -> 'a t
(** Copy the array and modify its copy *)

val map : ('a -> 'b) -> 'a t -> 'b t

val map_arr : ('a -> 'b) -> 'a t -> 'b array

val mapi : (int -> 'a -> 'b) -> 'a t -> 'b t

val mapi_arr : (int -> 'a -> 'b) -> 'a t -> 'b array

val append : 'a t -> 'a t -> 'a t

val iter : ('a -> unit) -> 'a t -> unit

val iteri : (int -> 'a -> unit) -> 'a t -> unit

val foldi : ('a -> int -> 'b -> 'a) -> 'a -> 'b t -> 'a

val fold : ('a -> 'b -> 'a) -> 'a -> 'b t -> 'a

val for_all : ('a -> bool) -> 'a t -> bool

val exists : ('a -> bool) -> 'a t -> bool

val equal : 'a equal -> 'a t equal

val compare : 'a ord -> 'a t ord

val hash : 'a Hash.t -> 'a t Hash.t

val hash_comm : 'a Hash.t -> 'a t Hash.t
(** Commutative hash *)

val to_seq : 'a t -> 'a Sequence.t
val to_seqi : 'a t -> (int * 'a) Sequence.t
val of_seq : 'a Sequence.t -> 'a t

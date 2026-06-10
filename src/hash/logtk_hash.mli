(* This file is free software. See file "license" for more details. *)

(** Hashing, from containers. *)

type hash = int
(** A hash value is a non-negative integer. *)

type 'a t = 'a -> hash
(** A hash function for values of type ['a]. *)

val const : hash -> _ t
(** [const h] hashes any value into [h]. Use with caution!. *)

val const0 : _ t
(** Always return 0. Useful for ignoring elements. *)

val bool : bool t
val int : int t
val char : char t
val int32 : int32 t
val int64 : int64 t
val string : string t

val bytes : bytes t
(** Hash a byte array. *)

val poly : 'a t
val list : 'a t -> 'a list t
val array : 'a t -> 'a array t
val opt : 'a t -> 'a option t
val pair : 'a t -> 'b t -> ('a * 'b) t
val triple : 'a t -> 'b t -> 'c t -> ('a * 'b * 'c) t
val quad : 'a t -> 'b t -> 'c t -> 'd t -> ('a * 'b * 'c * 'd) t

(** {2 Hash combinators} *)

val combine : 'a t -> int -> 'a -> int
val combine2 : int -> int -> int
val combine3 : int -> int -> int -> int
val combine4 : int -> int -> int -> int -> int
val combine5 : int -> int -> int -> int -> int -> int
val combine6 : int -> int -> int -> int -> int -> int -> int

(** {2 Iterators} *)

type 'a iter = ('a -> unit) -> unit
type 'a gen = unit -> 'a option

val seq : 'a t -> 'a iter t
val iter : 'a t -> 'a iter t
val gen : 'a t -> 'a gen t

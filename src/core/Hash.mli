
(* This file is free software. See file "license" for more details. *)

(** {1 Hashing Combinators (for hashconsing)} *)

(** A compositional interface for hashing values of type ['a]
    by providing a ['a -> int] function.
    Hashing composite structures is done by combining the
    hashes of individual components using {!combine} or {!combine2};
    Helpers for hashing lists, options, arrays, tuples, etc. are provided. *)

type 'a t = 'a -> int

val bool : bool t
val int : int t
val string : string t
val combine : 'a t -> int -> 'a -> int

val pair : 'a t -> 'b t -> ('a * 'b) t

val opt : 'a t -> 'a option t
val list : 'a t -> 'a list t
val array : 'a t -> 'a array t
val seq : 'a t -> 'a Sequence.t t

(** Orderless versions *)

val list_comm : 'a t -> 'a list t
val array_comm : 'a t -> 'a array t

val combine2 : int -> int -> int
val combine3 : int -> int -> int -> int
val combine4 : int -> int -> int -> int -> int
val combine5 : int -> int -> int -> int -> int -> int

val poly : 'a t
(** the regular polymorphic hash function *)

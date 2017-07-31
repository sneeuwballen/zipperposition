
(* This file is free software, part of Zipperposition. See file "license" for more details. *)

(** {2 Extensible Map for State}  *)

(** This data structure maps keys to values of any type (although the
    type is fixed for a given key). Useful for configuration.
*)

type t

val empty : t

type 'a key
val create_key : unit -> 'a key

val add : 'a key -> 'a -> t -> t

val get : 'a key -> t -> 'a option

val get_exn : 'a key -> t -> 'a
(** @raise Not_found if the key is not present *)

val get_or : or_:'a -> 'a key -> t -> 'a

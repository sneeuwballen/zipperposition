(* This source code is released into the Public Domain *)

type 'a t
  (** A hash table containing values of different types.
      The type parameter ['a] represents the type of the keys. *)

type ('a, 'b) injection
  (** An accessor for values of type 'b in the table. Values put
      in the table using an injection can only be retrieved using this
      very same injection. *)

val create : int -> 'a t
  (** [create n] creates a hash table of initial size [n]. *)

val access : unit -> ('a, 'b) injection
  (**
     Return a value that works for a given type of values.
     This function is normally called once for each type of value.
     Several injections may be created for the same type,
     but a value set with a given setter can only be retrieved with
     the matching getter.
     The same injection can be reused across multiple tables (although
     not in a thread-safe way).
  *)

val get : inj:('a, 'b) injection -> 'a t  -> 'a -> 'b option
  (** Get the value corresponding to this key, if it exists and
      belongs to the same injection *)

val set : inj:('a, 'b) injection -> 'a t -> 'a -> 'b -> unit
  (** Bind the key to the value, using [inj] *)

val length : 'a t -> int
  (** Number of bindings *)

val clear : 'a t -> unit
  (** Clear content of the hashtable *)

val remove : 'a t -> 'a -> unit
  (** Remove the binding for this key *)

val copy : 'a t -> 'a t
  (** Copy of the table *)

val mem : inj:('a, _) injection -> 'a t -> 'a -> bool
  (** Is the given key in the table, with the right type? *)

val find : inj:('a, 'b) injection -> 'a t -> 'a -> 'b
  (** Find the value for the given key, which must be of the right type.
      raises Not_found if either the key is not found, or if its value
      doesn't belong to the right type *)

val iter_keys : 'a t -> ('a -> unit) -> unit
  (** Iterate on the keys of this table *)

val fold_keys : 'a t -> 'b -> ('b -> 'a -> 'b) -> 'b
  (** Fold over the keys *)

val keys : 'a t -> 'a list
  (** List of the keys *)

val bindings : inj:('a, 'b) injection -> 'a t -> ('a * 'b) list
  (** All the bindings that come from the corresponding injection *)

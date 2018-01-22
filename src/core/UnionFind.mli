
(* This file is free software, part of Zipperposition. See file "license" for more details. *)

(** {1 Imperative Union-Find structure} *)

(** This structure operates on arbitrary objects as long as they are
    hashable. It maps keys to values (values belong to a monoid,
    if they are not needed, unit makes for a simple implementation)
    and each equivalence class' representative maps to
    the monoid merge of all the class' elements values.
    One also can iterate on the representative elements. *)

(** We need to be able to hash and compare keys, and values need to form
    a monoid *)
module type PAIR = sig
  type key
  type value

  val hash : key -> int
  val equal : key -> key -> bool

  val merge : value -> value -> value   (** Should be associative commutative *)
  val zero : value  (** Neutral element of {!merge} *)
end

(** Build a union-find module from a key/value specification *)
module Make(P : PAIR) : sig
  type key = P.key
  (** Elements that can be compared *)

  type value = P.value
  (** Values associated with elements *)

  type t
  (** The union-find imperative structure itself *)

  val create : key list -> t
  (** Create a union-find for the given elements. Elements are mapped
      to zero by default. *)

  val mem : t -> key -> bool
  (** Does the key belong to the UF? *)

  val find : t -> key -> key
  (** Finds the representative of this key's equivalence class.
      @raise Not_found if the key does not belong to the UF *)

  val find_value : t -> key -> value
  (** Find value for the given element. The value is the monoid
      merge of all values associated to [key]'s equivalence class.
      @raise Not_found if [mem uf key] is false. *)

  val union : t -> key -> key -> unit
  (** Merge two elements (and their equivalence classes) *)

  val add : t -> key -> value -> unit
  (** Add the given value to the key's class (monoid). It modifies the value
      by merging it with [value]. If the key does not belong
      to the union-find, it is added. *)

  val iter : t -> (key -> value -> unit) -> unit
  (** Iterate on representative and their value *)

  val to_seq : t -> (key * value) Sequence.t
end

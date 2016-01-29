
(* This file is free software, part of Zipperposition. See file "license" for more details. *)

(** {1 Hashconsing} *)

module type HashedType = sig
  type t
  val equal : t -> t -> bool
  val hash : t -> int
  val tag : int -> t -> unit
end

module type S = sig
  type elt
  (** Hashconsed objects *)

  val hashcons : elt -> elt
  (** Hashcons the elements *)

  val mem : elt -> bool
  (** Is the element present in this table? *)

  val fresh_unique_id : unit -> int
  (** Unique ID that will never occur again in this table (modulo 2^63...) *)

  val stats : unit -> int*int*int*int*int*int
end

module Make(X : HashedType) : S with type elt = X.t

(** Version that uses a regular Hashtbl, rather than a weak table. Never
    frees memory, but might be faster. *)
module MakeNonWeak(X : HashedType) : S with type elt = X.t

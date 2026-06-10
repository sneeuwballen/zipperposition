module type HashedType = sig
  type t

  val equal : t -> t -> bool
  val hash : t -> int
  val tag : int -> t -> unit
end

module type S = sig
  type elt

  val hashcons : elt -> elt
  val mem : elt -> bool
  val fresh_unique_id : unit -> int
  val stats : unit -> int * int * int * int * int * int
end

module Make : functor (X : HashedType) -> S with type elt = X.t

module type HashedType = sig
  type t

  val equal : t -> t -> bool
  val hash : t -> int
  val tag : int -> t -> unit
  val n_shards_log2 : int
  val init_size : int
end

module type S = sig
  type elt

  val hashcons : elt -> elt
  val fresh_unique_id : unit -> int
end

module type Make = functor (X : HashedType) -> S with type elt = X.t

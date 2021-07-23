
module Z : sig
  type t
  val zero : t
  val one : t
  val minus_one : t
  val of_int : int -> t
  val of_string : string -> t
  val succ : t -> t
  val pred : t -> t
  val abs : t -> t
  val neg : t -> t
  val add : t -> t -> t
  val sub : t -> t -> t
  val mul : t -> t -> t
  val div : t -> t -> t
  val rem : t -> t -> t
  val to_string : t -> string
  val pp_print : Format.formatter -> t -> unit
  val compare : t -> t -> int
  val equal : t -> t -> bool
  val leq : t -> t -> bool
  val geq : t -> t -> bool
  val lt : t -> t -> bool
  val gt : t -> t -> bool
  val sign : t -> int
  val min : t -> t -> t
  val max : t -> t -> t
  val hash : t -> int
end

module Q : sig

  type t
  val zero : t
  val one : t
  val minus_one : t
  val of_int : int -> t
  val of_ints : Z.t -> Z.t -> t
  val of_string : string -> t
  val sign : t -> int
  val lt : t -> t -> bool
  val gt : t -> t -> bool
  val to_string : t -> string
  val pp_print : Format.formatter -> t -> unit
  val compare : t -> t -> int
  val equal : t -> t -> bool

end

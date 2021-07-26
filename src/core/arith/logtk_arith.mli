
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
  val div_rem : t -> t -> t * t
  val gcd : t -> t -> t
  val lcm : t -> t -> t
  val to_string : t -> string
  val to_int_exn : t -> int
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

  val (+) : t -> t -> t
  val ( * ) : t -> t -> t
  val ( - ) : t -> t -> t
end

module Q : sig

  type t
  val zero : t
  val one : t
  val minus_one : t
  val of_int : int -> t
  val of_bigint : Z.t -> t
  val of_ints : int -> int -> t
  val of_string : string -> t
  val sign : t -> int
  val abs : t -> t
  val neg : t -> t
  val leq : t -> t -> bool
  val geq : t -> t -> bool
  val lt : t -> t -> bool
  val gt : t -> t -> bool
  val to_string : t -> string
  val to_bigint : t -> Z.t
  val pp_print : Format.formatter -> t -> unit
  val compare : t -> t -> int
  val equal : t -> t -> bool
  val div : t -> t -> t

  val (+) : t -> t -> t
  val ( * ) : t -> t -> t
  val ( - ) : t -> t -> t
  val ( / ) : t -> t -> t

end

type operator = int -> int -> int 

type t

val sum_algebra : t
val max_algebra : t

val create : string -> operator -> operator -> int -> t

val empty: t -> int 
val accumulator : t -> operator
val coeff_app : t -> operator
val name : t -> string

val alg_of_string : string -> t
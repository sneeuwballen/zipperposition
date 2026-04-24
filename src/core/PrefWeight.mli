module type S = sig
  val insert_term : Term.t -> unit
  val calc_pref_weight : Term.t -> int
end

module Make (PARAMETERS : sig
  val match_weight : float
  val miss_weight : float
end) : S

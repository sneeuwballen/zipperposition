
(* This file is free software, part of Logtk. See file "license" for more details. *)

(** {1 Polynomial} *)


module type IntegerModule = sig
  type t

  val zero : t
  val one : t

  val add : t -> t -> t
  val mult : int -> t -> t
  val compare : t -> t -> int

  val pp : Format.formatter -> t -> unit
end

module type OrderedType = sig
  type t
  val compare : t -> t -> int
  val pp : Format.formatter -> t -> unit
end


module type S = sig
  type t
  type coeff
  type indet

  val const : coeff -> t
  val indet : indet -> t

  val add : t -> t -> t

  val mult_const : int -> t -> t
  val mult_indet : indet -> t -> t

  val compare : t -> t -> int
  (** Compares two polynomials by comparing the coefficients for each monomial:
      If all coefficients of p1 >= the corresponding coefficient in p2,
      and one is even >, then return 1.
      If all coefficients of p1 <= the corresponding coefficient in p2,
      and one is even <, then return -1.
      If the polynomials are equal, return 0.
      If some coefficients are < and some are >, return 0. *)

  val pp : Format.formatter -> t -> unit
end

module Make (Coeff : IntegerModule) (Indet : OrderedType) : S with type coeff = Coeff.t and type indet = Indet.t

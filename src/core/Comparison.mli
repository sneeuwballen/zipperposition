
(* This file is free software, part of Logtk. See file "license" for more details. *)

(** {1 Partial Ordering values} *)

(** {2 Combined nonstrict-strict partial orders} *)

module Nonstrict : sig
  type t = Lt | Leq | Eq | Geq | Gt | Incomparable

  type nonstrict_comparison = t

  val equal : t -> t -> bool

  include Interfaces.PRINT with type t := t

  val opp : t -> t
  (** Opposite of the relation: a R b becomes b R a *)

  val to_total : t -> int
  (** Conversion to a total ordering. Geq, Leq, and Incomparable are translated
      to 0 (equal). *)

  val of_total : int -> t
  (** Conversion from a total order *)

  val merge_with_Geq : t -> t
  val merge_with_Leq : t -> t
  (** Combine a comparison value with Geq or Leq. *)

  val smooth : t -> t
  (** Replace Gt by Geq and Lt by Leq. *)

  type 'a comparator = 'a -> 'a -> t

  val (@>>) : 'a comparator -> 'a comparator -> 'a comparator
  (** Combination of comparators that work on the same values. *)
end

val is_Gt_or_Geq : Nonstrict.t -> bool
val is_Gt_or_Geq_or_Eq : Nonstrict.t -> bool
val is_Lt_or_Leq : Nonstrict.t -> bool
val is_Lt_or_Leq_or_Eq : Nonstrict.t -> bool
(** Test for several constructors at once. *)

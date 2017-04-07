
(* This file is free software, part of Zipperposition. See file "license" for more details. *)

(** {1 Simple and Lightweight Congruence and order} *)

module type S = Congruence_intf.S

(** {2 Functor} *)

module type TERM = sig
  type t

  val equal : t -> t -> bool
  (** Syntactic equality on terms *)

  val hash : t -> int
  (** Hash function on terms *)

  val subterms : t -> t list
  (** Subterms of the term (possibly empty list) *)

  val update_subterms : t -> t list -> t
  (** Replace immediate subterms by the given list.
      This is used to test for equality *)
end

module Make(T : TERM) : S with type term = T.t

(** {2 Common implementations} *)

module FO : S with type term = FOTerm.t

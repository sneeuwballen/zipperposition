
(* This file is free software, part of Zipperposition. See file "license" for more details. *)

(** The congruence stores a finite representation of a set of (ground)
    equations an inequalities. It is {b backtrackable}, ie one can go
    back to a previous point as long as it is compatible with
    a LIFO usage. *)

module type S = sig
  type term

  type t
  (** Represents a congruence *)

  val create : ?size:int -> unit -> t
  (** New congruence.
      @param size a hint for the initial size of the hashtable. *)

  val iter : t -> (mem:term -> repr:term -> unit) -> unit
  (** Iterate on terms that are explicitely present in the congruence.
      The callback is given [mem], the term itself, and [repr],
      the current representative of the term [mem].

      Invariant: when calling [iter cc f], if [f ~mem ~repr] is called,
      then [find cc mem == repr] holds.
  *)

  val iter_roots : t -> (term -> unit) -> unit
  (** Iterate on the congruence classes' representative elements.
      Exactly one term per congruence class will be passed to the
      function. *)

  val add : t -> term -> t
  (** Add the term to the congruence closure *)

  val mk_eq : t -> term -> term -> t
  (** [mk_eq congruence t1 t2] asserts that [t1 = t2] belongs to
      the congruence *)

  val is_eq : t -> term -> term -> bool
  (** Returns true if the two terms are equal in the congruence. This
      updates the congruence, because the two terms need to be added. *)

  val pp_debug : t CCFormat.printer
end

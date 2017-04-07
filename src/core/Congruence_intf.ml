
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

  val clear : t -> unit
  (** Clear the content of the congruence. It is now equivalent to
      the empty congruence. *)

  type level

  val save : t -> level
  (** Push a checkpoint on the stack of the congruence. An equivalent call
      to {!pop} will restore the congruence to its current state. *)

  val restore : t -> level -> unit
  (** Restore to the given checkpoint. *)

  val find : t -> term -> term
  (** Current representative of this term *)

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

  val mk_eq : t -> term -> term -> unit
  (** [mk_eq congruence t1 t2] asserts that [t1 = t2] belongs to
      the congruence *)

  val mk_less : t -> term -> term -> unit
  (** [mk_less congruence t1 t2] asserts that [t1 < t2] belongs to
      the congruence *)

  val is_eq : t -> term -> term -> bool
  (** Returns true if the two terms are equal in the congruence. This
      updates the congruence, because the two terms need to be added. *)

  val is_less : t -> term -> term -> bool
  (** Returns true if the first term is strictly lower than the second
      one in the congruence *)

  val cycles : t -> bool
  (** Checks whether there are cycles in inequalities.
      @return true if calls to [mk_eq] and [mk_less] entail a cycle in
      the ordering (hence contradicting irreflexivity/transitivity of less) *)
end

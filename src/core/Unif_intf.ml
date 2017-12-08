
(* This file is free software, part of Logtk. See file "license" for more details. *)

(** {1 Interface for unification} *)

type unif_subst = Unif_subst.t
type subst = Subst.t

module type S = sig
  type ty
  type term

  val bind : ?check:bool -> subst -> ty HVar.t Scoped.t -> term Scoped.t -> subst
  (** [bind subst v t] binds [v] to [t], but fails if [v] occurs in [t]
      (performs an occur-check first)
      @param check if true, perform occur check
      @raise Fail if occurs-check fires or if the variable is bound already *)

  val update : ?check:bool -> subst -> ty HVar.t Scoped.t -> term Scoped.t -> subst
  (** [bind subst v t] replaces the binding of [v] to [t], but fails if [v] occurs in [t]
      (performs an occur-check first)
      @param check if true, perform occur check
      @raise Fail if occurs-check fires or if the variable is not yet bound *)

  val unify_syn : ?subst:subst ->
    term Scoped.t -> term Scoped.t -> subst
  (** Unify terms syntictally, returns a subst
      @raise Fail if the terms are not unifiable *)

  val unify_full : ?subst:unif_subst ->
    term Scoped.t -> term Scoped.t -> unif_subst
  (** Unify terms, returns a subst + constraints or
      @raise Fail if the terms are not unifiable *)

  val matching : ?subst:subst ->
    pattern:term Scoped.t -> term Scoped.t -> subst
  (** [matching ~pattern scope_p b scope_b] returns
      [sigma] such that [sigma pattern = b], or fails.
      Only variables from the scope of [pattern] can  be bound in the subst.
      @param subst initial substitution (default empty)
      @raise Fail if the terms do not match.
      @raise Invalid_argument if the two scopes are equal *)

  val matching_same_scope :
    ?protect:(ty HVar.t Sequence.t) -> ?subst:subst ->
    scope:int -> pattern:term -> term -> subst
  (** matches [pattern] (more general) with the other term.
      The two terms live in the same scope, which is passed as the
      [scope] argument. It needs to gather the variables of the
      other term to make sure they are not bound.
      @param scope the common scope of both terms
      @param protect a sequence of variables to protect (they cannot
        be bound during matching!). Variables of the second term
        are automatically protected. *)

  val matching_adapt_scope :
    ?protect:(ty HVar.t Sequence.t) -> ?subst:subst ->
    pattern:term Scoped.t -> term Scoped.t -> subst
  (** Call either {!matching} or {!matching_same_scope} depending on
      whether the given scopes are the same or not.
      @param protect used if scopes are the same, see {!matching_same_scope} *)

  val variant : ?subst:subst ->
    term Scoped.t -> term Scoped.t -> subst
  (** Succeeds iff the first term is a variant of the second, ie
      if they are alpha-equivalent *)

  val equal : subst:subst -> term Scoped.t -> term Scoped.t -> bool
  (** [equal subst t1 s1 t2 s2] returns [true] iff the two terms
      are equal under the given substitution, i.e. if applying the
      substitution will return the same term. *)

  val are_unifiable_full : term -> term -> bool
  (** Unifiable with some additional constraints? *)

  val are_unifiable_syn : term -> term -> bool
  (** Unifiable syntactically? *)

  val matches : pattern:term -> term -> bool

  val are_variant : term -> term -> bool
end

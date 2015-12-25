
(* This file is free software, part of Libzipperposition. See file "license" for more details. *)

type subst = Substs.t

module type UNARY = sig
  type ty
  type term

  val bind : subst -> ty HVar.t Scoped.t -> term Scoped.t -> subst
  (** [bind subst v t] binds [v] to [t], but fails if [v] occurs in [t]
      (performs an occur-check first)
      @raise Fail if occurs-check fires *)

  val unification : ?subst:subst ->
    term Scoped.t -> term Scoped.t -> subst
  (** Unify terms, returns a subst or
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

  val are_unifiable : term -> term -> bool

  val matches : pattern:term -> term -> bool

  val are_variant : term -> term -> bool
end

module type NARY = sig
  type term

  val unification : ?subst:subst ->
    term Scoped.t -> term Scoped.t -> subst Sequence.t
  (** unification of two terms *)

  val matching :
    ?subst:subst ->
    pattern:term Scoped.t -> term Scoped.t -> subst Sequence.t
  (** matching of two terms.
      @raise Invalid_argument if the two scopes are equal. *)

  val variant : ?subst:subst -> term Scoped.t -> term Scoped.t -> subst Sequence.t
  (** alpha-equivalence checking of two terms *)

  val are_unifiable : term -> term -> bool

  val matches : pattern:term -> term -> bool

  val are_variant : term -> term -> bool
end

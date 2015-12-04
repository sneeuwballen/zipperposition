
(* This file is free software, part of Logtk. See file "license" for more details. *)

(** {1 Unification and Matching} *)

type scope = Substs.scope
type subst = Substs.t
type env = ScopedTerm.t DBEnv.t
type 'a sequence = ('a -> unit) -> unit

exception Fail
(** Raised when a unification/matching attempt fails *)

(** {2 Signatures} *)

module type UNARY = sig
  type term

  val unification : ?env1:env -> ?env2:env -> ?subst:subst ->
    term Scoped.t -> term Scoped.t -> subst
  (** Unify terms, returns a subst or
      @raise Fail if the terms are not unifiable
      @param env1 environment for the first term
      @param env2 environment for the second term *)

  val matching : ?allow_open:bool -> ?env1:env -> ?env2:env -> ?subst:subst ->
    pattern:term Scoped.t -> term Scoped.t -> subst
  (** [matching ~pattern scope_p b scope_b] returns
      [sigma] such that [sigma pattern = b], or fails.
      Only variables from the scope of [pattern] can  be bound in the subst.
      @param subst initial substitution (default empty)
      @param allow_open if true, variables can bind to non-closed DB terms (default [false])
      @raise Fail if the terms do not match.
      @raise Invalid_argument if the two scopes are equal *)

  val matching_same_scope : ?env1:env -> ?env2:env -> ?protect:(term Sequence.t) -> ?subst:subst ->
    scope:scope -> pattern:term -> term -> subst
  (** matches [pattern] (more general) with the other term.
      The two terms live in the same scope, which is passed as the
      [scope] argument. It needs to gather the variables of the
      other term to make sure they are not bound.
      @param scope the common scope of both terms
      @param protect a sequence of variables to protect (they cannot
        be bound during matching!). Variables of the second term
        are automatically protected. *)

  val matching_adapt_scope : ?env1:env -> ?env2:env -> ?protect:(term Sequence.t) -> ?subst:subst ->
    pattern:term Scoped.t -> term Scoped.t -> subst
  (** Call either {!matching} or {!matching_same_scope} depending on
      whether the given scopes are the same or not.
      @param protect used if scopes are the same, see {!matching_same_scope} *)

  val variant : ?env1:env -> ?env2:env -> ?subst:subst ->
    term Scoped.t -> term Scoped.t -> subst
  (** Succeeds iff the first term is a variant of the second, ie
      if they are alpha-equivalent *)

  val equal : ?env1:env -> ?env2:env -> subst:subst -> term Scoped.t -> term Scoped.t -> bool
  (** [equal subst t1 s1 t2 s2] returns [true] iff the two terms
      are equal under the given substitution, i.e. if applying the
      substitution will return the same term. *)

  val are_unifiable : term -> term -> bool

  val matches : pattern:term -> term -> bool

  val are_variant : term -> term -> bool
end

module type NARY = sig
  type term

  val unification : ?env1:env -> ?env2:env -> ?subst:subst ->
    term Scoped.t -> term Scoped.t -> subst Sequence.t
  (** unification of two terms *)

  val matching : ?allow_open:bool -> ?env1:env -> ?env2:env -> ?subst:subst ->
    pattern:term Scoped.t -> term Scoped.t -> subst Sequence.t
  (** matching of two terms.
      @param allow_open if true, variables can bind to non-closed DB terms (default [false])
      @raise Invalid_argument if the two scopes are equal. *)

  val variant : ?env1:env -> ?env2:env -> ?subst:subst ->
    term Scoped.t -> term Scoped.t -> subst Sequence.t
  (** alpha-equivalence checking of two terms *)

  val are_unifiable : term -> term -> bool

  val matches : pattern:term -> term -> bool

  val are_variant : term -> term -> bool
end

(** {2 Base (scoped terms)} *)

module Nary : NARY with type term = ScopedTerm.t

module Unary : UNARY with type term = ScopedTerm.t
(** To be used only on terms without {!ScopedTerm.Multiset} constructor *)

(** {2 Specializations} *)

module Ty : UNARY with type term = Type.t
module FO : UNARY with type term = FOTerm.t
module HO : NARY with type term = HOTerm.t

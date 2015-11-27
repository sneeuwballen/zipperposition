
(*
Copyright (c) 2013, Simon Cruanes
All rights reserved.

Redistribution and use in source and binary forms, with or without
modification, are permitted provided that the following conditions are met:

Redistributions of source code must retain the above copyright notice, this
list of conditions and the following disclaimer.  Redistributions in binary
form must reproduce the above copyright notice, this list of conditions and the
following disclaimer in the documentation and/or other materials provided with
the distribution.

THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS "AS IS" AND
ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED
WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE ARE
DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT HOLDER OR CONTRIBUTORS BE LIABLE
FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL
DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR
SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER
CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY,
OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE
OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
*)

(** {1 LogtkUnification and Matching} *)

type scope = LogtkSubsts.scope
type subst = LogtkSubsts.t
type env = LogtkScopedTerm.t LogtkDBEnv.t
type 'a sequence = ('a -> unit) -> unit

(** {2 Result of (multiple) LogtkUnification} *)

type res = subst sequence

val res_head : res -> subst option
(** Obtain the first result, if any
    @since 0.5.2 *)

exception Fail
  (** Raised when a unification/matching attempt fails *)

(** {2 LogtkSignatures} *)

module type UNARY = sig
  type term

  val unification : ?env1:env -> ?env2:env -> ?subst:subst ->
                    term -> scope -> term -> scope -> subst
    (** LogtkUnify terms, returns a subst or
        @raise Fail if the terms are not unifiable
        @param env1 environment for the first term
        @param env2 environment for the second term *)

  val matching : ?allow_open:bool -> ?env1:env -> ?env2:env -> ?subst:subst ->
                 pattern:term -> scope -> term -> scope -> subst
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
                             pattern:term -> scope -> term -> scope -> subst
    (** Call either {!matching} or {!matching_same_scope} depending on
        whether the given scopes are the same or not.
        @param protect used if scopes are the same, see {!matching_same_scope} *)

  val variant : ?env1:env -> ?env2:env -> ?subst:subst ->
                term -> scope -> term -> scope -> subst
    (** Succeeds iff the first term is a variant of the second, ie
        if they are alpha-equivalent *)

  val equal : ?env1:env -> ?env2:env -> subst:subst -> term -> scope -> term -> scope -> bool
    (** [equal subst t1 s1 t2 s2] returns [true] iff the two terms
        are equal under the given substitution, i.e. if applying the
        substitution will return the same term. *)

  val are_unifiable : term -> term -> bool

  val matches : pattern:term -> term -> bool

  val are_variant : term -> term -> bool
end

module type NARY = sig
  type term
  type result = res

  val unification : ?env1:env -> ?env2:env -> ?subst:subst ->
                    term -> scope -> term -> scope -> result
    (** unification of two terms *)

  val matching : ?allow_open:bool -> ?env1:env -> ?env2:env -> ?subst:subst ->
                 pattern:term -> scope -> term -> scope -> result
    (** matching of two terms.
        @param allow_open if true, variables can bind to non-closed DB terms (default [false])
        @raise Invalid_argument if the two scopes are equal. *)

  val variant : ?env1:env -> ?env2:env -> ?subst:subst ->
                term -> scope -> term -> scope -> result
    (** alpha-equivalence checking of two terms *)

  val are_unifiable : term -> term -> bool

  val matches : pattern:term -> term -> bool

  val are_variant : term -> term -> bool
end

(** {2 Base (scoped terms)} *)

module Nary : NARY with type term = LogtkScopedTerm.t

module Unary : UNARY with type term = LogtkScopedTerm.t
(** To be used only on terms without {!LogtkScopedTerm.Multiset} constructor *)

(** {2 Specializations} *)

module Ty : UNARY with type term = LogtkType.t
module FO : UNARY with type term = LogtkFOTerm.t
module HO : NARY with type term = LogtkHOTerm.t

(** {2 LogtkFormulas} *)

module Form : sig
  val variant : ?env1:env -> ?env2:env -> ?subst:subst ->
                LogtkFormula.FO.t -> scope -> LogtkFormula.FO.t -> scope ->
                res

  val are_variant : LogtkFormula.FO.t -> LogtkFormula.FO.t -> bool
end


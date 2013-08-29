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

(** {1 Unification and matching algorithms} *)

exception Fail
  (** Raised when a unification/matching attempt fails *)

val types : Signature.t -> Term.t -> Term.t -> bool
  (** Check that, if one of the terms is a variable, the (inferred) types
      are compatible with the given signature. *)

val unification : ?subst:Substs.t -> Term.t -> Substs.scope ->
                  Term.t -> Substs.scope -> Substs.t
  (** Unify terms, returns a Substs.t or
      @raise Fail if the terms are not unifiable *)

val matching : ?subst:Substs.t -> Term.t -> Substs.scope ->
                Term.t -> Substs.scope -> Substs.t
  (** [matching a scope_a b scope_b] returns sigma such that sigma(a) = b, or
      @raise Fail if the terms do not match.
      Only variables from the scope of [a] can  be bound in the Substs.t. *)

val variant : ?subst:Substs.t -> Term.t -> Substs.scope ->
              Term.t -> Substs.scope -> Substs.t
  (** Succeeds iff the first term is a variant of the second *)

val matching_ac : ?is_ac:(Symbol.t -> bool) -> ?is_com:(Symbol.t -> bool) ->
                  ?offset:int ref -> ?subst:Substs.t ->
                  Term.t -> Substs.scope -> Term.t -> Substs.scope ->
                  Substs.t Sequence.t
  (** [matching_ac a b] returns Substs.ts such that [subst(a) =_AC b]. It
      is much more costly than [matching]. By default [is_ac] returns true only
      for symbols that have [attr_ac], and [is_com] only for [attr_commut].
      [offset] is used to create new variables. *)

(** {2 Unification on formulas} *)

val form_unify : ?subst:Substs.t ->
                  Formula.t -> Substs.scope ->
                  Formula.t -> Substs.scope ->
                  Substs.t Sequence.t
  (** Set of unifiers of both formulas modulo AC. No variable is
      introduced at the proposition level, and the properties
      that f1 or f2 may become f if f1\sigma = f2\sigma
      are ignored (no deduplication of subformulas). *)

val form_variant : ?subst:Substs.t ->
                    Formula.t -> Substs.scope ->
                    Formula.t -> Substs.scope ->
                    Substs.t Sequence.t
  (** Set of renamings of both formulas, if they are alpha-equivalent
      modulo AC *)

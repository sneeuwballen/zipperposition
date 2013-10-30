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

type term = HOTerm.t
type subst = Substs.HO.t
type scope = Substs.scope

val unification : ?subst:subst -> term -> scope ->
                  term -> scope -> subst
  (** Unify terms, returns a subst or
      @raise Fail if the terms are not unifiable *)

val matching : ?subst:subst -> term -> scope ->
                term -> scope -> subst
  (** [matching a scope_a b scope_b] returns sigma such that sigma(a) = b, or
      @raise Fail if the terms do not match.
      Only variables from the scope of [a] can  be bound in the subst. *)

val variant : ?subst:subst -> term -> scope ->
              term -> scope -> subst
  (** Succeeds iff the first term is a variant of the second *)

val matching_ac : ?is_ac:(Symbol.t -> bool) -> ?is_com:(Symbol.t -> bool) ->
                  ?offset:int ref -> ?subst:subst ->
                  term -> scope -> term -> scope ->
                  subst Sequence.t
  (** [matching_ac a b] returns substs such that [subst(a) =_AC b]. It
      is much more costly than [matching]. By default [is_ac] returns true only
      for symbols that have [attr_ac], and [is_com] only for [attr_commut].
      [offset] is used to create new variables. *)

val are_variant : term -> term -> bool


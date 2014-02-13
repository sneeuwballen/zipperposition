
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

(** {1 Unification and Matching} *)

exception Fail
  (** Raised when a unification/matching attempt fails *)

type scope = Substs.scope
type subst = Substs.t

(** {2 Signature} *)

module type S = sig
  type term

  val unification : ?subst:subst -> term -> scope -> term -> scope -> subst
    (** Unify terms, returns a subst or
        @raise Fail if the terms are not unifiable *)

  val matching : ?subst:subst -> pattern:term -> scope -> term -> scope -> subst
    (** [matching ~pattern scope_p b scope_b] returns
        [sigma] such that [sigma pattern = b], or
        @raise Fail if the terms do not match.
        Only variables from the scope of [pattern] can  be bound in the subst. *)

  val variant : ?subst:subst -> term -> scope -> term -> scope -> subst
    (** Succeeds iff the first term is a variant of the second, ie
        if they are alpha-equivalent *)

  val are_unifiable : term -> term -> bool

  val matches : pattern:term -> term -> bool

  val are_variant : term -> term -> bool
end

(** {2 Base (scoped terms)} *)

include S with type term = ScopedTerm.t

(** {2 Specializations} *)

module Ty : S with type term = Type.t
module FO : S with type term = FOTerm.t
module HO : S with type term = HOTerm.t

(** {2 AC} *)

module type AC_SPEC = sig
  val is_ac : Symbol.t -> bool
  val is_comm : Symbol.t -> bool
end

module AC(S : AC_SPEC) : sig
  val matching_ac : ?offset:int ref -> ?subst:subst ->
                    pattern:term -> scope -> term -> scope ->
                    subst Sequence.t
  (** [matching_ac ~pattern s_p b s_b] returns substs such that
      [subst pattern =_AC b]. It
      is much more costly than [matching].
      @param offset is used to create new variables. *)
end

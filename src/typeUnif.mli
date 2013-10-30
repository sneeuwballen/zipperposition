
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

(** {1 Unification of Types} *)

type scope = Substs.scope

type error = {
  left : Type.t;
  s_left : scope;
  right : Type.t;
  s_right : scope;
  subst : Substs.Ty.t;
}

val pp_error : Buffer.t -> error -> unit
val error_to_string : error -> string

exception Error of error
  (** Raised when a unification error occurred *)

(** {2 Unification} *)

val unify : ?subst:Substs.Ty.t ->
            Type.t -> scope -> Type.t -> scope ->
            Substs.Ty.t
  (** Unify two types.
      @raise Error in case they are not unifiable. *)

val unify_fo : ?subst:Substs.FO.t -> 
              Type.t -> scope -> Type.t -> scope ->
              Substs.FO.t
  (** Unify types, using the FO Term substitution (which contains a
      type substitution) *)

val unify_ho : ?subst:Substs.HO.t -> 
              Type.t -> scope -> Type.t -> scope ->
              Substs.HO.t
  (** Unify types, using the HO Term substitution (which contains a
      type substitution) *)

val are_unifiable : Type.t -> Type.t -> bool
  (** Can the two types be unified? *)

val unifier : Type.t -> Type.t -> Type.t
  (** Given two types, belonging to unique scopes, can we unify them into
      a new type? Variables not bound during unification will be
      arbitrarily renamed. *)

(** {2 Matching}
Only variables of the pattern (first type) can be bound. *)

val match_ : ?subst:Substs.Ty.t ->
              Type.t -> scope -> Type.t -> scope ->
              Substs.Ty.t
  (** Match first type against the second
      @raise Error in case the first is not a generalization of the second *)

val match_fo : ?subst:Substs.FO.t -> 
               Type.t -> scope -> Type.t -> scope ->
               Substs.FO.t

val match_ho : ?subst:Substs.HO.t -> 
               Type.t -> scope -> Type.t -> scope ->
               Substs.HO.t

(** {2 Alpha-equivalence}
Same structure, only the type variables' names change *)

val variant : ?subst:Substs.Ty.t ->
              Type.t -> scope -> Type.t -> scope ->
              Substs.Ty.t
  (** Check for alpha renaming *)

val variant_fo : ?subst:Substs.FO.t -> 
                 Type.t -> scope -> Type.t -> scope ->
                 Substs.FO.t

val variant_ho : ?subst:Substs.HO.t -> 
                 Type.t -> scope -> Type.t -> scope ->
                 Substs.HO.t

val are_variants : Type.t -> Type.t -> bool
  (** Shortcut for {!variant} in distinct scopes *)

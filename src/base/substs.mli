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

(** {1 Substitutions}

Substitutions map variables to terms/types. They work on free variables (within
a scope, so that the same variable can live within several scopes).

The concept of scope is to allow the same free variable to be used in
several contexts without being renamed. A scope is kind of a namespace,
where variables from distinct namespaces are always distinct.
*)

type scope = int
  (** A scope is an integer. Variables can only be bound in one scope,
      and variables from distinct scopes are distinct too. *)

type 'a scoped = 'a * scope

type term = ScopedTerm.t

(** {2 Renamings}
A renaming is used to disambiguate variables that come from distinct
scopes but have the same index. It is used to merge together several scopes, in
a sound way, by ensuring variables from those scopes are mapped to distinct
variables of the new scope. For instance, a given renaming applied to (X,0) and
(X,1) will return two different variables, as if one of the X had been renamed
prior to unification/binding. *)

module Renaming : sig
  type t
  val create : unit -> t
  val clear : t -> unit
end

(** {3 Basics} *)

type t
  (** A substitution that binds term variables to other terms *)

type subst = t

val empty : t
  (** The identity substitution *)

val is_empty : t -> bool
  (** Is the substitution empty? *)

(** {3 Operations on Substitutions} *)

val lookup : t -> term -> scope -> term * scope
  (** Lookup variable in substitution.
      @raise Not_found if variable not bound. *)

val get_var : t -> term -> scope -> term * scope
  (** Lookup recursively the var in the substitution, until it is not a
      variable anymore, or it is not bound *)

val mem : t -> term -> scope -> bool
  (** Check whether the variable is bound by the substitution *)

exception KindError

val bind : t -> term -> scope -> term -> scope -> t
  (** Add [v] -> [t] to the substitution. Both terms have a context.
      @raise Invalid_argument if [v] is already bound in
        the same context, to another term.
      @raise KindError if a term of a given kind is bound to a term
        of another kind (see {!ScopedTerm.Kind.t}) *)

val append : t -> t -> t
  (** [append s1 s2] is the substitution that maps [t] to [s2 (s1 t)]. *)

val remove : t -> term -> int -> t
  (** Remove the given binding. No other variable should depend on it... *)

(** {2 Set operations} *)

val domain : t -> (term * scope) Sequence.t
  (** Domain of substitution *)

val codomain : t -> (term * scope) Sequence.t
  (** Codomain (image terms) of substitution *)

val introduced : t -> (term * scope) Sequence.t
  (** Variables introduced by the substitution (ie vars of codomain) *)

(*
val compose : t -> t -> t
  (** [compose s1 s2] is the substitution that to [x] associates
      [s1 (s2 x)]. *)
*)

val is_renaming : t -> bool
  (** Check whether the substitution is a variable renaming *)

include Interfaces.PRINT with type t := t

val fold : t -> 'a -> ('a -> term -> scope -> term -> scope -> 'a) -> 'a
val iter : t -> (term -> scope -> term -> scope -> unit) -> unit

val to_seq : t -> (term * scope * term * scope) Sequence.t
val to_list : t -> (term * scope * term * scope) list
val of_seq : ?init:t -> (term * scope * term * scope) Sequence.t -> t
val of_list : ?init:t -> (term * scope * term * scope) list -> t

(** {2 Applying a substitution} *)

val apply : ?depth:int -> t -> renaming:Renaming.t -> term -> scope -> term
  (** Apply the substitution to the given term.
      @param renaming used to desambiguate free variables from distinct scopes *)

val apply_no_renaming : ?depth:int -> t -> term -> scope -> term
  (** Same as {!apply}, but performs no renaming of free variables.
      {b Caution}, can entail collisions between scopes! *)

(* TODO
include Interfaces.SERIALIZABLE with type t := t
*)

(** {2 Specializations} *)

module type SPECIALIZED = sig
  type term
  type t = subst

  val apply : ?depth:int -> t -> renaming:Renaming.t -> term -> scope -> term
    (** Apply the substitution to the given term/type.
        @param renaming used to desambiguate free variables from distinct scopes *)

  val apply_no_renaming : ?depth:int -> t -> term -> scope -> term
    (** Same as {!apply}, but performs no renaming of free variables.
      {b Caution}, can entail collisions between scopes! *)

  val bind : t -> term -> scope -> term -> scope -> t
    (** Add [v] -> [t] to the substitution. Both terms have a context.
        @raise Invalid_argument if [v] is already bound in
          the same context, to another term. *)
end

module Ty : SPECIALIZED with type term = Type.t

module FO : SPECIALIZED with type term = FOTerm.t

module HO : SPECIALIZED with type term = HOTerm.t

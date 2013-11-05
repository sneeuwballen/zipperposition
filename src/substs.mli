(*
Zipperposition: a functional superposition prover for prototyping
Copyright (C) 2012 Simon Cruanes

This is free software; you can redistribute it and/or
modify it under the terms of the GNU General Public License
as published by the Free Software Foundation; either version 2
of the License, or (at your option) any later version.

This is distributed in the hope that it will be useful,
but WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
GNU General Public License for more details.

You should have received a copy of the GNU General Public License
along with this program; if not, write to the Free Software
Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA
02110-1301 USA.
*)

(** {1 Substitutions} *)

type scope = int
  (** A scope is an integer. Variables can only be bound in one scope,
      and variables from distinct scopes are distinct too. *)

type 'a scoped = 'a * scope

(** {2 Signature of substitutions} *)

module type S = sig
  type term
    (** Some term structure *)

  (** {3 Basics} *)

  type t
    (** A substitution that binds term variables to other terms *)

  val empty : unit -> t
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

  val bind : t -> term -> scope -> term -> scope -> t
    (** Add [v] -> [t] to the substitution. Both terms have a context.
        @raise Invalid_argument if [v] is already bound in
          the same context, to another term. *)

  val append : t -> t -> t
    (** [append s1 s2] is the substitution that maps [t] to [s2 (s1 t)]. *)

  val remove : t -> term -> int -> t
    (** Remove the given binding. No other variable should depend on it... *)

  module H : Hashtbl.S with type key = term * scope
    (** Set of bound terms *)

  val domain : t -> unit H.t
    (** Domain of substitution *)

  val codomain : t -> unit H.t
    (** Codomain (image terms) of substitution *)

  val introduced : t -> unit H.t
    (** Variables introduced by the substitution (ie vars of codomain) *)

  val compose : t -> t -> t
    (** [compose s1 s2] is the substitution that to [x] associates
        [s1 (s2 x)].
        XXX not implemented *)

  val is_renaming : t -> bool
    (** Check whether the substitution is a variable renaming *)

  val pp : Buffer.t -> t -> unit
  val to_string : t -> string
  val fmt : Format.formatter -> t -> unit

  val fold : t -> 'a -> ('a -> term -> scope -> term -> scope -> 'a) -> 'a
  val iter : t -> (term -> scope -> term -> scope -> unit) -> unit

  val to_seq : t -> (term * scope * term * scope) Sequence.t
  val to_list : t -> (term * scope * term * scope) list
  val of_seq : ?init:t -> (term * scope * term * scope) Sequence.t -> t
  val of_list : ?init:t -> (term * scope * term * scope) list -> t

  val bij : t Bij.t
end

(** {2 Renaming}
A renaming is used to merge together several scopes, in a sound way,
by ensuring variables from those scopes are mapped to distinct
variables of the new scope. For instance, a given renaming
applied to (X,0) and (X,1) will return two different variables, as
if one of the X had been renamed prior to unification/binding. *)

module type RENAMING = sig
  type t

  val create : int -> t
    (** Fresh renaming *)

  val clear : t -> unit
    (** Cleanup the content of the renaming. It is as new afterwards! *)
end

(** {2 Substitutions on types}

This kind of substitution is used to keep track of universal type
variables during type unification. Other substitutions use this kind
of substitutions to deal with free variables's types (that may need to
be specialized).
*)

module Ty : sig
  include S with type term = Type.t

  module Renaming : RENAMING

  val apply : t -> renaming:Renaming.t -> Type.t -> scope -> Type.t
    (** Apply the substitution to the type.
        @param renaming used to desambiguate free variables from distinct scopes *)

  val apply_no_renaming : t -> Type.t -> scope -> Type.t
    (** Same as {!apply}, but performs no renaming of free variables.
        {b Caution}, can entail collisions between scopes! *)
end

(** {2 Substitutions on various Terms}

Substitutions on terms also contain a substitution on types, because unifying
term variables requires to unify their types. *)

module FO : sig
  include S with type term = FOTerm.t

  val ty_subst : t -> Ty.t
    (** The substitution on types *)

  val bind_ty : t -> Type.t -> scope -> Type.t -> scope -> t
    (** Bind types *)

  val update_ty : t -> (Ty.t -> Ty.t) -> t
    (** Update the type substitution inside the substitution *)

  val of_ty : Ty.t -> t
    (** Lift substitution on type to substution on terms *)

  module Renaming : RENAMING

  val apply : renaming:Renaming.t -> t -> term -> scope -> term
    (** Apply substitution to term, replacing variables by the terms they are bound to.

        [renaming] is used to rename free variables (not bound by [subst])
        while avoiding collisions. *)

  val apply_no_renaming : t -> term -> scope -> term
    (** Apply the substitution, and does not rename variables. {b Caution}, this
        can entail collisions between scopes! *)

  val apply_f : renaming:Renaming.t -> t -> FOFormula.t -> scope -> FOFormula.t
    (** Apply the substitution to the formula *)
end

module HO : sig
  include S with type term = HOTerm.t

  val ty_subst : t -> Ty.t
    (** The substitution on types *)

  val bind_ty : t -> Type.t -> scope -> Type.t -> scope -> t
    (** Bind types *)

  val update_ty : t -> (Ty.t -> Ty.t) -> t
    (** Update the type substitution inside the substitution *)

  val of_ty : Ty.t -> t
    (** Lift substitution on type to substution on terms *)

  module Renaming : RENAMING

  val apply : ?depth:int -> renaming:Renaming.t -> t -> term -> scope -> term
    (** Apply substitution to term, replacing variables by the terms they are
        bound to.
        @param depth number of binders surrounding the term (used for De
        Bruijn indexes) *)

  val apply_no_renaming : ?depth:int -> t -> term -> scope -> term
    (** Apply the substitution, and does not rename variables. {b Caution}, this
        can entail collisions between scopes! *)
end

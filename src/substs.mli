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

(** substitution, a list of (variable -> term) *)
type t = private
  | SubstBind of (Term.t * int * Term.t * int * t)
  | SubstEmpty
and scope = int
and 'a scoped = 'a * int
  (** A scope is an integer. Variables can only be bound in one scope,
      and variables from distinct scopes are distinct too. *)

val empty : t
  (** The identity substitution *)

val is_empty : t -> bool
  (** Is the substitution empty? *)

val eq : t -> t -> bool
  (** Check (naively, ie structurally) whether two substitutions are equal *)

val compare : t -> t -> int
  (** Compare substitutions (arbitrary but total order) *)

val lookup : t -> Term.t -> int -> Term.t * int
  (** Lookup variable in substitution. Raise Not_found if not present. *)

val get_var : t -> Term.t -> int -> Term.t * int
  (** Lookup recursively the var in the substitution, until it is not a
      variable anymore, or it is not bound *)

val is_in_subst : t -> Term.t -> int -> bool
  (** Check whether the variable is bound by the substitution *)

val bind : ?recursive:bool -> t -> Term.t -> int -> Term.t -> int -> t
  (** Add v -> t to the substitution. Both terms have a context. Raise
      Invalid_argument if v is already bound in the same context, to another term. *)

val remove : t -> Term.t -> int -> t
  (** Remove the given binding. No other variable should depend on it... *)

val append : t -> t -> t
  (** [append s1 s2] is the substitution that maps [t] to [s2 (s1 t)]. *)

(** Disambiguation of variables between different contexts *)
module Renaming : sig
  type t
    (** A renaming, from (variable,offset) to variable *)
  
  val create : int -> t
    (** Create a new general-purpose renaming, which manages to rename
        variables of any number of contexts without ambiguities *)

  val clear : t -> unit
    (** Clear the content of the renaming *)

  val rename : t -> Term.t -> int -> Term.t
    (** Rename the given variable, scoped by the given context *)
end

val apply_subst : ?recursive:bool -> ?renaming:Renaming.t ->
                   t -> Term.t -> int -> Term.t
  (** Apply substitution to term, replacing variables by the terms they are bound to.
      The [renaming] is used to rename free variables (not bound
      by [subst]) while avoiding collisions. Otherwise variables are shifted
      by [offset].
      [recursive] decides whether, when [v] is replaced by [t], [subst] is
      applied to [t] recursively or not (default true). *)

module Domain : Set.S with type elt = Term.t * int
  (** Set of bound terms *)

val domain : t -> Domain.t
  (** Domain of substitution *)

val codomain : t -> Domain.t
  (** Codomain (image terms) of substitution *)

val is_renaming : t -> bool
  (** Check whether the substitution is a variable renaming *)

val pp_full : (Buffer.t -> Term.t -> unit) -> Buffer.t -> t -> unit
val pp : Buffer.t -> t -> unit
val to_string : t -> string
val fmt : Format.formatter -> t -> unit

val iter : t -> (Term.t * int * Term.t * int -> unit) -> unit

val to_seq : t -> (Term.t * int * Term.t * int) Sequence.t
val of_seq : ?recursive:bool -> ?subst:t ->
            (Term.t * int * Term.t * int) Sequence.t -> t
val of_list : ?recursive:bool -> ?subst:t ->
            (Term.t * int * Term.t * int) list -> t

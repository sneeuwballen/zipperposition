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

(** Operations on substitutions *)

open Basic

val id_subst : substitution
  (** The identity substitution *)

val is_empty : substitution -> bool
  (** Is the substitution empty? *)

val eq_subst: substitution -> substitution -> bool
  (** Check (naively, ie structurally) whether two substitutions are equal *)

val compare_substs: substitution -> substitution -> int
  (** Compare substitutions (arbitrary but total order) *)

val lookup : substitution -> term bind -> term bind
  (** Lookup variable in substitution. Raise Not_found if not present. *)

val get_var : substitution -> term bind -> term bind
  (** Lookup recursively the var in the substitution, until it is not a
      variable anymore, or it is not bound *)

val is_in_subst : substitution -> term bind -> bool
  (** Check whether the variable is bound by the substitution *)

val bind : ?recursive:bool -> substitution -> term bind -> term bind -> substitution
  (** Add v -> t to the substitution. Both terms have a context. Raise
      Invalid_argument if v is already bound in the same context, to another term. *)

val apply_subst : ?recursive:bool -> substitution -> term bind -> term
  (** Apply substitution to term, replacing variables by the terms they are bound to.
      The offset (term bind) is applied to variables that are not bound by subst.
      [recursive] decides whether, when [v] is replaced by [t], [subst] is
      applied to [t] recursively or not (default true). *)

module Domain : Set.S with type elt = term bind
  (** Set of bound terms *)

val domain : substitution -> Domain.t
  (** Domain of substitution *)

val codomain : substitution -> Domain.t
  (** Codomain (image terms) of substitution *)

val is_renaming : substitution -> bool
  (** Check whether the substitution is a variable renaming *)

val pp_substitution : Format.formatter -> substitution -> unit

val to_seq : substitution -> (term bind * term bind) Sequence.t
val of_seq : ?recursive:bool -> (term bind * term bind) Sequence.t -> substitution

val to_json : substitution -> json
val of_json : ?recursive:bool -> json -> substitution

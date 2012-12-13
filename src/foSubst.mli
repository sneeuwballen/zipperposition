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

open Types

val id_subst : substitution
  (** the identity substitution *)

val is_empty : substitution -> bool
  (** is the substitution empty? *)

val build_subst : ?recursive:bool -> substitution -> term -> term -> substitution
  (** add v -> t to the substitution. If recursive is true,
      then v -> subst(t) is considered instead.
      If v occurs in t, OccurCheck (v,t) is raised. *)

val update_binding : ?recursive:bool -> substitution -> term -> substitution
  (** update the substitution with the current binding of the variable *)

val update_bindings : ?recursive:bool -> substitution -> term list -> substitution
  (** update the substitution with current binding of the varibles *)

val expand_bindings : substitution -> substitution
  (** expand bindings in the codomain of subst *)

val eq_subst: substitution -> substitution -> bool
  (** check (naively, ie structurally) whether two substitutions are equal *)

val hash_subst: substitution -> int
  (** hash a substitution *)

val compare_substs: substitution -> substitution -> int
  (** compare substitutions (arbitrary but total order) *)

module SSet : Set.S with type elt = substitution

val lookup : term -> substitution -> term
  (** lookup variable in substitution *)

val is_in_subst : term -> substitution -> bool
  (** check whether the variable is bound by the substitution *)

val domain : substitution -> Terms.TSet.t
  (** domain of substitution *)

val codomain : substitution -> Terms.TSet.t
  (** codomain (image terms) of substitution *)

val is_renaming : substitution -> bool
  (** check whether the substitution is a renaming *)

val reset_bindings : substitution -> unit
  (** reset bindings of variables and terms of the substitution *)

val apply_subst_bind : substitution -> unit
  (** for each (v, t) in subst, v.binding <- t *)

val apply_subst : ?recursive:bool -> substitution -> term -> term
  (** apply substitution to term, replacing variables by the term they are bound to *)

val relocate : int -> varlist -> substitution
  (** relocate variables in the varset so that they are > int *)

val normalize_term : term -> term * substitution
  (** unique representation of term t, substitution to get back to t *)

val pp_substitution : Format.formatter -> substitution -> unit
val pp_set : Format.formatter -> SSet.t -> unit

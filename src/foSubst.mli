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

exception OccurCheck of (term * term)

val id_subst : substitution

(** add v -> t to the substitution. If recursive is true,
    then v -> subst(t) is considered instead.
    If v occurs in t, OccurCheck (v,t) is raised. *)
val build_subst : ?recursive:bool -> term -> term -> substitution -> substitution

(** update the substitution with the current binding of the variable *)
val update_binding : substitution -> term -> substitution
(** update the substitution with current binding of the varibles *)
val update_bindings : substitution -> term list -> substitution
(** expand bindings in the codomain of subst *)
val expand_bindings : substitution -> substitution

(** check (naively, ie structurally) whether two substitutions are equal *)
val eq_subst: substitution -> substitution -> bool
(** hash a substitution *)
val hash_subst: substitution -> int
(** compare substitutions (arbitrary but total order) *)
val compare_substs: substitution -> substitution -> int

module SSet : Set.S with type elt = substitution

(** lookup variable in substitution *)
val lookup : term -> substitution -> term
val is_in_subst : term -> substitution -> bool

(** filter out from the varlist the variables bound by subst *)
val filter : substitution -> varlist -> varlist
(** restrict the domain to variables not present in term *)
val restrict_exclude: substitution -> term -> substitution

(** domain of substitution *)
val domain : substitution -> Terms.THashSet.t
(** codomain (image terms) of substitution *)
val codomain : substitution -> Terms.THashSet.t

(** reset bindings of variables and terms of the substitution *)
val reset_bindings : substitution -> unit

(** for each (v, t) in subst, v.binding <- t *)
val apply_subst_bind : substitution -> unit
(** apply substitution to term, replacing variables by the term they are bound to *)
val apply_subst : ?recursive:bool -> substitution -> term -> term

(** normalize the substitution, such that subst(subst(v)) = subst(v)
    for all v. The result is idempotent. *)
val flat: substitution -> substitution
val concat: substitution -> substitution -> substitution

(** perform renaming to get disjoint variables sets,
    ie the resulting substitution's domain has no common
    variable with [varlist], and its new domain is newvarlist
    relocate [maxvar] [varlist] [subst] ->
    [newmaxvar] * [newvarlist] * [relocsubst] *)
val relocate : ?recursive:bool -> int -> varlist -> substitution
            -> (int * varlist * substitution)

val fresh_term : int -> term -> term      (** fresh term, with all variables > maxvar *)
val relocate_term : varlist -> term -> term (** rename the term so that
                                                    it has no variable in varlist *)
val normalize_term : term -> term * substitution  (** unique representation of term t,
                                                          substitution to get back to t *)

val pp_substitution : Format.formatter -> substitution -> unit
val pp_set : Format.formatter -> SSet.t -> unit

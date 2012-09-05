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

exception OccurCheck of (foterm * foterm)

val id_subst : substitution

(** add v -> t to the substitution. If recursive is true,
    then v -> subst(t) is considered instead.
    If v occurs in t, OccurCheck (v,t) is raised. *)
val build_subst : ?recursive:bool -> foterm -> foterm -> substitution -> substitution

(** lookup variable in substitution *)
val lookup : foterm -> substitution -> foterm
val is_in_subst : foterm -> substitution -> bool

(** filter out from the varlist the variables bound by subst *)
val filter : substitution -> varlist -> varlist

val apply_subst : ?recursive:bool -> substitution -> foterm -> foterm

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

val fresh_foterm : int -> foterm -> foterm      (** fresh term, with all variables > maxvar *)
val relocate_term : varlist -> foterm -> foterm (** rename the term so that
                                                    it has no variable in varlist *)
val normalize_term : foterm -> foterm * substitution  (** unique representation of term t,
                                                          substitution to get back to t *)


val pp_substitution : Format.formatter -> substitution -> unit

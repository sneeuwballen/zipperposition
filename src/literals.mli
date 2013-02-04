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

(** Equational literals *)

open Types
open Symbols

val eq : literal -> literal -> bool         (** equality of literals *)
val eq_com : literal -> literal -> bool     (** commutative equality of lits *)
val compare : literal -> literal -> int     (** lexicographic comparison of literals *)

val compare_partial : ord:ordering -> literal -> literal -> comparison
  (** partial comparison of literals *)

val to_multiset : literal -> term list      (** literal to multiset of terms *)

val hash : literal -> int                   (** hashing of literal *)
val weight : literal -> int                 (** weight of the lit *)

val is_pos : literal -> bool                (** is the literal positive? *)
val is_neg : literal -> bool                (** is the literal negative? *)
val equational : literal -> bool            (** is the literal a proper equation? *)
val orientation_of : literal -> comparison  (** get the orientation of the literal *)

(** build literals. If sides so not have the same sort,
    a SortError will be raised. An ordering must be provided *)
val mk_eq : ord:ordering -> term -> term -> literal
val mk_neq : ord:ordering -> term -> term -> literal
val mk_lit : ord:ordering -> term -> term -> bool -> literal
val reord : ord:ordering -> literal -> literal      (** recompute order *)
val lit_of_fof : ord:ordering -> literal -> literal (** translate eq/not to literal *)
val term_of_lit : literal -> term                   (** translate lit to term *)

val apply_subst : ?recursive:bool -> ord:ordering -> substitution -> literal bind -> literal

val negate : literal -> literal                     (** negate literal *)
val fmap : ord:ordering -> (term -> term) -> literal -> literal (** fmap in literal *)
val add_vars : Terms.THashSet.t -> literal -> unit  (** Add variables to the set *)
val vars : literal -> varlist                       (** gather variables *)

val eq_lits : literal array -> literal array -> bool
val compare_lits : literal array -> literal array -> int
val hash_lits : literal array -> int
val vars_lits : literal array -> varlist
val ground_lits : literal array -> bool             (** all the literals are ground? *)
val term_of_lits : literal array -> term
val apply_subst_lits : ?recursive:bool -> ord:ordering -> substitution ->
                       literal array bind -> literal array
val apply_subst_list : ?recursive:bool -> ord:ordering -> substitution ->
                        literal list bind -> literal list

(** pretty printer for literals *)

val pp_literal : Format.formatter -> literal -> unit
val pp_lits : Format.formatter -> literal array -> unit

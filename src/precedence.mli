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

open Types
open Symbols

(** Precedence (total ordering) on symbols *)

(** compute the current signature of symbols. It returns a
    map of symbols to sorts, a map of symbols to their arity,
    and the signature (the list of current symbols) *)
val current_signature : unit ->
                        sort SHashtbl.t * int SHashtbl.t * symbol list

(* ----------------------------------------------------------------------
 * hard constraints on the ordering
 * ---------------------------------------------------------------------- *)

val cluster_constraint : symbol list list -> ordering_constraint
  (** ordering constraint by clustering symbols by decreasing order.
      all symbols in the first clusters are bigger than those in the second, etc. *)

val list_constraint : symbol list -> ordering_constraint
  (** symbols in the given list are in decreasing order *)

val ordering_to_constraint : symbol_ordering -> ordering_constraint
  (** convert a symbol ordering into an ordering constraint. Useful to
      extend an ordering without breaking it. *)

val arity_constraint : ordering_constraint
  (** decreasing arity constraint *)

val invfreq_constraint : hclause list -> ordering_constraint
  (** symbols with high frequency are smaller *)

val max_constraint : symbol list -> ordering_constraint
  (** maximal symbols, in decreasing order *)

val min_constraint : symbol list -> ordering_constraint
  (** minimal symbols, in decreasing order *)

val alpha_constraint : ordering_constraint
  (** regular (alphabetic) ordering on symbols *)

(* ----------------------------------------------------------------------
 * Creation of a precedence (symbol_ordering) from constraints
 * ---------------------------------------------------------------------- *)

val make_ordering : ordering_constraint list -> symbol_ordering
  (** make an ordering from the given constraints. First constraints are
      more important than later constraints. Only the very first constraint
      is assured to be totally satisfied. *)

val default_symbol_ordering : unit -> symbol_ordering
  (** default ordering on symbols *)

(* ----------------------------------------------------------------------
 * Heuristic creation of precedences
 * ---------------------------------------------------------------------- *)

val heuristic_precedence : (symbol_ordering -> ordering)
                           -> ordering_constraint list
                           -> ordering_constraint list
                           -> hclause list -> symbol_ordering
  (** define a constraint on symbols that is believed to improve
      the search by enabling as many simplifications as possible. It takes
      an ordering as a parameter, to be able to decide the orientation of
      terms in a given precedence, and ordering constraints that are
      respectively weaker/stronger than the optimization (the first one, weaker,
      is applied to break ties, the second one, stronger, is always enforced first) *)


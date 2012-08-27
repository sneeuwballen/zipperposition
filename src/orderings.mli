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

(** Partial ordering on terms, total ordering on symbols *)

(* ----------------------------------------------------------------------
 symbol total ordering
 ---------------------------------------------------------------------- *)

(** compute the current signature of symbols. It returns a
    map of symbols to sorts, a map of symbols to their arity,
    and the signature (the list of current symbols) *)
val current_signature : unit ->
                        (symbol, sort) Hashtbl.t * (symbol, int) Hashtbl.t * symbol list

(** ordering constraint by clustering symbols by decreasing order.
    all symbols in the first clusters are bigger than those in the second, etc. *)
val cluster_constraint : symbol list list -> ordering_constraint
(** symbols in the given list are in decreasing order *)
val list_constraint : symbol list -> ordering_constraint
(** convert a symbol ordering into an ordering constraint. Useful to
    extend an ordering without breaking it. *)
val ordering_to_constraint : symbol_ordering -> ordering_constraint
(** decreasing arity constraint *)
val arity_constraint : (symbol, int) Hashtbl.t -> ordering_constraint
(** maximal symbols, in decreasing order *)
val max_constraint : symbol list -> ordering_constraint
(** minimal symbols, in decreasing order *)
val min_constraint : symbol list -> ordering_constraint

(** compose constraints (the second one is prioritary) *)
val compose_constraints : ordering_constraint -> ordering_constraint -> ordering_constraint

(** enforce that minimal symbols are $false > $true *)
val consts_constraint : ordering_constraint

(** apply the constraint to the ordering, to get a new ordering that respect
    the constraint. less important constraints should be applied first,
    most important constraints should be applied last. *)
val apply_constraint : symbol_ordering -> ordering_constraint -> symbol_ordering
(** check that the constraint is respected by the ordering *)
val check_constraint : symbol_ordering -> ordering_constraint -> bool
(** make an ordering from the given constraint *)
val make_ordering : ordering_constraint -> symbol_ordering

(** default ordering on symbols *)
val default_symbol_ordering : unit -> symbol_ordering

(** symbol ordering that just compares symbols by string ordering *)
val dummy_symbol_ordering : symbol_ordering


(* ----------------------------------------------------------------------
 terms partial ordering
 ---------------------------------------------------------------------- *)

class kbo : symbol_ordering -> ordering

class rpo : symbol_ordering -> ordering

val default_ordering : unit -> ordering   (** default ordering on terms *)

val dummy_ordering : ordering             (** always returns incomparable *)

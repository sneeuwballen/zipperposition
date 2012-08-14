(*
    ||M||  This file is part of HELM, an Hypertextual, Electronic
    ||A||  Library of Mathematics, developed at the Computer Science
    ||T||  Department, University of Bologna, Italy.
    ||I||
    ||T||  HELM is free software; you can redistribute it and/or
    ||A||  modify it under the terms of the GNU General Public License
    \   /  version 2 or (at your option) any later version.
     \ /   This software is distributed as is, NO WARRANTY.
      V_______________________________________________________________ *)

open Types

(* ----------------------------------------------------------------------
 symbol total ordering
 ---------------------------------------------------------------------- *)

(** compute the current signature of symbols *)
val current_signature : unit ->
                        (symbol, sort) Hashtbl.t * (symbol, int) Hashtbl.t * symbol list

(** compute an arity ordering, based on the current terms table *)
val arity_ordering : unit -> symbol_ordering

(** default ordering on symbols *)
val default_symbol_ordering : unit -> symbol_ordering

val dummy_symbol_ordering : symbol_ordering

(* ----------------------------------------------------------------------
 terms partial ordering
 ---------------------------------------------------------------------- *)

class nrkbo : symbol_ordering -> ordering

class kbo : symbol_ordering -> ordering

class lpo : symbol_ordering -> ordering

val default_ordering : unit -> ordering   (** default ordering on terms *)

val dummy_ordering : ordering             (** always returns incomparable *)

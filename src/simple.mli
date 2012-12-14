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

(** Simple representation of terms and formula *)

open Symbols

type term =
  | Var of int * sort
  | Node of symbol * sort * term list

val compare_terms : term -> term -> int

val eq_terms : term -> term -> bool

val term_sort : term -> sort

val cast : term -> sort -> term

val is_var : term -> bool
val is_node : term -> bool

val mk_var : int -> sort -> term
val mk_node : symbol -> sort -> term list -> term
val mk_const : symbol -> sort -> term

type formula =
  | True | False
  | Atom of term
  | Eq of term * term
  | Or of formula list
  | And of formula list
  | Not of formula
  | Equiv of formula * formula
  | Forall of term * formula
  | Exists of term * formula
and sourced_formula = formula * source
and source = Axiom of string * string | Derived of string * formula list

val compare_formulas : formula -> formula -> int

val eq_formulas : formula -> formula -> bool

val mk_true : formula
val mk_false : formula
val mk_atom : term -> formula
val mk_eq : term -> term -> formula
val mk_neq : term -> term -> formula
val mk_lit : term -> term -> bool -> formula
val mk_or : formula list -> formula
val mk_and : formula list -> formula
val mk_not : formula -> formula
val mk_imply : formula -> formula -> formula
val mk_equiv : formula -> formula -> formula
val mk_xor : formula -> formula -> formula
val mk_forall : term -> formula -> formula
val mk_exists : term -> formula -> formula

module TMap : Map.S with type key = term

val signature : formula list -> (int * sort) SMap.t
  (** Signature (map symbol -> arity * sort) of this set of formulas *)

val symbols : formula list -> symbol list
  (** get a list of symbols from a list of (sourced) formulas *)

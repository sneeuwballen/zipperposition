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

(** Some common things for superposition calculi *)

open Types
open Symbols

(** binary inferences. An inference returns a list of conclusions *)
type binary_inf_rule = ProofState.active_set -> clause -> clause list

(** unary infererences *)
type unary_inf_rule = ord:ordering -> clause -> clause list

(** The type of a calculus for first order reasoning with equality *) 
class type calculus =
  object
    (** the binary inference rules *)
    method binary_rules : (string * binary_inf_rule) list
    (** the unary inference rules *)
    method unary_rules : (string * unary_inf_rule) list
    (** how to simplify a clause *)
    method basic_simplify : ord:ordering -> clause -> clause
    (** how to simplify a clause w.r.t a set of clauses *)
    method simplify : ProofState.active_set -> clause -> clause
    (** check whether the clause is redundant w.r.t the set *)
    method redundant : ProofState.active_set -> clause -> bool
    (** find redundant clauses in set w.r.t the clause *)
    method redundant_set : ProofState.active_set -> clause -> hclause list
    (** how to simplify a clause into a (possibly empty) list
        of clauses. This subsumes the notion of trivial clauses (that
        are simplified into the empty list of clauses) *)
    method list_simplify : ord:ordering -> select:selection_fun -> clause -> clause list option
    (** a list of axioms to add to the Set of Support *)
    method axioms : clause list
    (** some constraints on the precedence *)
    method constr : clause list -> ordering_constraint
    (** how to preprocess the initial list of clauses *)
    method preprocess : ord:ordering -> clause list -> clause list
  end

(** do binary inferences that involve the given clause *)
val do_binary_inferences : ProofState.active_set ->
                          (string * binary_inf_rule) list -> (** named rules *)
                          clause -> clause list

(** do unary inferences for the given clause *)
val do_unary_inferences : ord:ordering ->
                          (string * unary_inf_rule) list ->
                          clause -> clause list

(* some helpers *)
val fold_lits :
  ?pos:bool -> ?neg:bool -> ?both:bool ->
  ('a -> term -> term -> bool -> position -> 'a) -> 'a ->
  (literal * int) list -> 'a
val fold_positive :
  ?both:bool -> ('a -> term -> term -> bool -> position -> 'a) -> 'a ->
  (literal * int) list -> 'a
val fold_negative :
  ?both:bool -> ('a -> term -> term -> bool -> position -> 'a) -> 'a ->
  (literal * int) list -> 'a

val get_equations_sides : clause -> position -> term * term * bool

(** Skolemize the given term at root (assumes it occurs just under an
    existential quantifier, whose De Bruijn variable is replaced
    by a fresh symbol applied to free variables). This also
    caches symbols, so that the same term is always skolemized
    the same way. The sort is the sort of the free De Bruijn symbol in t.

    It also refreshes the ordering (the signature has changed) *)
val skolem : ord:ordering -> term -> sort -> term

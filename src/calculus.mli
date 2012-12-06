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
type binary_inf_rule = ProofState.active_set -> clause -> hclause list

(** unary infererences *)
type unary_inf_rule = ord:ordering -> hclause -> hclause list

(** The type of a calculus for first order reasoning with equality *) 
class type calculus =
  object

    method binary_rules : (string * binary_inf_rule) list
      (** the binary inference rules *)

    method unary_rules : (string * unary_inf_rule) list
      (** the unary inference rules *)

    method basic_simplify : ord:ordering -> hclause -> hclause
      (** how to simplify a clause *)

    method simplify : select:selection_fun -> ProofState.active_set -> Index.unit_index -> hclause -> hclause
      (** how to simplify a clause w.r.t a set of clauses *)

    method redundant : ProofState.active_set -> hclause -> bool
      (** check whether the clause is redundant w.r.t the set *)

    method redundant_set : ProofState.active_set -> hclause -> hclause list
      (** find redundant clauses in set w.r.t the clause *)

    method list_simplify : ord:ordering -> select:selection_fun -> hclause -> hclause list option
      (** how to simplify a clause into a (possibly empty) list
          of clauses. This subsumes the notion of trivial clauses (that
          are simplified into the empty list of clauses) *)

    method axioms : hclause list
      (** a list of axioms to add to the problem *)

    method constr : hclause list -> ordering_constraint
      (** some constraints on the precedence *)

    method preprocess : ord:ordering -> select:selection_fun -> hclause list -> hclause list
      (** how to preprocess the initial list of clauses *)
  end

(** do binary inferences that involve the given clause *)
val do_binary_inferences : ProofState.active_set ->
                          (string * binary_inf_rule) list -> (** named rules *)
                          hclause -> hclause list

(** do unary inferences for the given clause *)
val do_unary_inferences : ord:ordering ->
                          (string * unary_inf_rule) list ->
                          hclause -> hclause list

(** fold on equation sides of literals that satisfy predicate *)
val fold_lits : ?both:bool -> (int -> literal -> bool) ->
                ('a -> term -> term -> bool -> position -> 'a) -> 'a ->
                literal array -> 'a

(** get the term l at given position in clause, and r such that l ?= r
    is the literal at the given position *)
val get_equations_sides : clause -> position -> term * term * bool

(** Skolemize the given term at root (assumes it occurs just under an
    existential quantifier, whose De Bruijn variable is replaced
    by a fresh symbol applied to free variables). This also
    caches symbols, so that the same term is always skolemized
    the same way. The sort is the sort of the free De Bruijn symbol in t.

    It also refreshes the ordering (the signature has changed) *)
val classic_skolem : ord:ordering -> term -> sort -> term

(** Skolemization with a special non-first order symbol. The purpose is
    not to introduce too many terms. A proposition p is skolemized
    into $$skolem(p), which makes naturally for inner skolemization.

    The advantage is that it does not modify the signature, and also that
    rewriting can be performed inside the skolem terms. *)
val unamed_skolem : ord:ordering -> term -> sort -> term

(** default skolemization function *)
val skolem : (ord:ordering -> term -> sort -> term) ref

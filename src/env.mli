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

(** {1 Global environment for an instance of the prover} *)

open Basic
open Symbols

type binary_inf_rule = ProofState.active_set -> clause -> hclause list
  (** binary inferences. An inference returns a list of conclusions *)

type unary_inf_rule = hclause -> hclause list
  (** unary infererences *)

type t = {
  mutable params : Basic.parameters;
  mutable ctx : Basic.context;

  mutable binary_rules : (string * binary_inf_rule) list;
    (** the binary inference rules *)
  
  mutable unary_rules : (string * unary_inf_rule) list;
    (** the unary inference rules *)
  
  mutable basic_simplify : hclause -> hclause;
    (** how to simplify a clause *)
  
  mutable rw_simplify : ProofState.simpl_set -> hclause -> hclause;
    (** how to simplify a clause w.r.t a set of unit clauses *)
  
  mutable active_simplify : ProofState.active_set -> hclause -> hclause;
    (** how to simplify a clause w.r.t an active set of clauses *)

  mutable backward_simplify : ProofState.active_set -> hclause -> Clauses.CSet.t;
    (** backward simplification by a unit clause. It returns a set of
        active clauses that can potentially be simplified by the given clause *)

  mutable redundant : ProofState.active_set -> hclause -> bool;
    (** check whether the clause is redundant w.r.t the set *)

  mutable backward_redundant : ProofState.active_set -> hclause -> hclause list;
    (** find redundant clauses in set w.r.t the clause *)

  mutable list_simplify : hclause -> hclause list;
    (** how to simplify a clause into a (possibly empty) list
        of clauses. This subsumes the notion of trivial clauses (that
        are simplified into the empty list of clauses) *)

  mutable is_trivial : hclause -> bool;
    (** single test to detect trivial clauses *)

  mutable axioms : hclause list;
    (** a list of axioms to add to the problem *)

  mutable mk_constr : (hclause list -> precedence_constraint list) list;
    (** How to build constraints from a list of clauses *)

  mutable constr : precedence_constraint list;
    (** some constraints on the precedence *)

  mutable preprocess : ctx:context -> hclause list -> hclause list;
    (** how to preprocess the initial list of clauses *)

  mutable state : ProofState.state;
    (** Proof state *)

  mutable empty_clauses : Clauses.CSet.t;
    (** Set of empty clauses *)

  mutable on_empty : (hclause -> unit) list;
    (** Callbacks for empty clause detection *)
}

(** {2 Basic operations} *)

val mk_env : ?meta:Meta.Prover.t -> ctx:context -> parameters -> signature -> t
  (** Create an environment (initially empty) *)

val add_passive : env:t -> hclause Sequence.t -> unit

val add_active : env:t -> hclause Sequence.t -> unit

val add_simpl : env:t -> hclause Sequence.t -> unit

val remove_passive : env:t -> hclause Sequence.t -> unit
val remove_passive_id : env:t -> int Sequence.t -> unit

val remove_active : env:t -> hclause Sequence.t -> unit

val remove_simpl  : env:t -> hclause Sequence.t -> unit

val clean_passive : env:t -> unit
  (** Clean passive set *)

val add_constrs : env:t -> precedence_constraint Sequence.t -> unit

val add_mk_constr : env:t -> (hclause list -> precedence_constraint list) -> unit

val get_passive : env:t -> hclause Sequence.t

val get_active : env:t -> hclause Sequence.t

val get_simpl : env:t -> hclause Sequence.t

val add_binary_inf : env:t -> string -> binary_inf_rule -> unit

val add_unary_inf : env:t -> string -> unary_inf_rule -> unit

val add_expert : env:t -> Experts.t -> unit

val list_simplify : env:t -> hclause -> hclause list

val get_experts : env:t -> Experts.Set.t

val get_meta : env:t -> Meta.Prover.t option

val get_params : env:t -> parameters

val get_empty_clauses : env:t -> Clauses.CSet.t
val get_some_empty_clause : env:t -> hclause option

val add_on_empty : env:t -> (hclause -> unit) -> unit

val compute_constrs : env:t -> hclause list -> precedence_constraint list
  (** Compute all ordering constraints for the given list of clauses *)

val pp : Format.formatter -> t -> unit

(** {2 High level operations} *)

type stats = int * int * int
  (** statistics on clauses : num active, num passive, num simplification *)

val stats : env:t -> stats
  (** Compute stats *)

val next_passive : env:t -> hclause option
  (** Extract next passive clause *)

val do_binary_inferences : env:t -> hclause -> hclause Sequence.t
  (** do binary inferences that involve the given clause *)

val do_unary_inferences : env:t -> hclause -> hclause Sequence.t
  (** do unary inferences for the given clause *)

val is_trivial : env:t -> hclause -> bool
  (** Check whether the clause is trivial (also with Experts) *)

val simplify : env:t -> hclause -> hclause * hclause
  (** Simplify the hclause. Returns both the hclause and its simplification. *)

val backward_simplify : env:t -> hclause -> Clauses.CSet.t * hclause Sequence.t
  (** Perform backward simplification with the given clause. It returns the
      CSet of clauses that become redundant, and the sequence of those
      very same clauses after simplification. *)

val forward_simplify : env:t -> hclause -> hclause Sequence.t
  (** Simplify the clause w.r.t to the active set and experts *)

val remove_orphans : env:t -> hclause Sequence.t -> unit
  (** remove orphans of the (now redundant) clauses *)

val generate : env:t -> hclause -> hclause Sequence.t
  (** Perform all generating inferences *)

val is_redundant : env:t -> hclause -> bool
  (** Is the given clause redundant w.r.t the active set? *)

val subsumed_by : env:t -> hclause -> hclause list
  (** List of active clauses subsumed by the given clause *)

val all_simplify : env:t -> hclause -> hclause list
  (** Use all simplification rules to convert a clause into a list of maximally
      simplified clauses (possibly empty, if trivial). *)

val meta_step : env:t -> hclause -> hclause Sequence.t
  (** Do one step of the meta-prover with the current given clause. New clauses
      (lemmas) are returned. *)
  
val preprocess : env:t -> hclause list -> hclause list
  (** Preprocess clauses *)


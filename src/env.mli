
(*
Zipperposition: a functional superposition prover for prototyping
Copyright (c) 2013, Simon Cruanes
All rights reserved.

Redistribution and use in source and binary forms, with or without
modification, are permitted provided that the following conditions are met:

Redistributions of source code must retain the above copyright notice, this
list of conditions and the following disclaimer.  Redistributions in binary
form must reproduce the above copyright notice, this list of conditions and the
following disclaimer in the documentation and/or other materials provided with
the distribution.

THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS "AS IS" AND
ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED
WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE ARE
DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT HOLDER OR CONTRIBUTORS BE LIABLE
FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL
DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR
SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER
CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY,
OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE
OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
*)

(** {1 Global environment for an instance of the prover} *)

open Logtk

type binary_inf_rule = ProofState.ActiveSet.t -> Clause.t -> Clause.t list
  (** binary inferences. An inference returns a list of conclusions *)

type unary_inf_rule = Clause.t -> Clause.t list
  (** unary infererences *)

type lit_rewrite_rule = ctx:Ctx.t -> Literal.t -> Literal.t
  (** Rewrite rule on literals *)

type t = {
  mutable params : Params.t;
  mutable ctx : Ctx.t;

  mutable binary_rules : (string * binary_inf_rule) list;
    (** the binary inference rules *)
  
  mutable unary_rules : (string * unary_inf_rule) list;
    (** the unary inference rules *)

  mutable rewrite_rules : (string * (Term.t -> Term.t)) list;
    (** Rules to apply to term *)

  mutable lit_rules : (string * lit_rewrite_rule) list;
    (** Rules to be applied to literals *)
  
  mutable basic_simplify : Clause.t -> Clause.t;
    (** how to simplify a clause *)
  
  mutable rw_simplify : ProofState.SimplSet.t -> Clause.t -> Clause.t;
    (** how to simplify a clause w.r.t a set of unit clauses *)
  
  mutable active_simplify : ProofState.ActiveSet.t -> Clause.t -> Clause.t;
    (** how to simplify a clause w.r.t an active set of clauses *)

  mutable backward_simplify : ProofState.ActiveSet.t -> Clause.t -> Clause.CSet.t;
    (** backward simplification by a unit clause. It returns a set of
        active clauses that can potentially be simplified by the given clause *)

  mutable redundant : ProofState.ActiveSet.t -> Clause.t -> bool;
    (** check whether the clause is redundant w.r.t the set *)

  mutable backward_redundant : ProofState.ActiveSet.t -> Clause.t -> Clause.t list;
    (** find redundant clauses in set w.r.t the clause *)

  mutable list_simplify : Clause.t -> Clause.t list;
    (** how to simplify a clause into a (possibly empty) list
        of clauses. This subsumes the notion of trivial clauses (that
        are simplified into the empty list of clauses) *)

  mutable is_trivial : Clause.t -> bool;
    (** single test to detect trivial clauses *)

  mutable axioms : PFormula.t list;
    (** a list of axioms to add to the problem *)

  mutable mk_constr : (Clause.t Sequence.t -> Precedence.constr list) list;
    (** How to build constraints from a list of clauses *)

  mutable constr : Precedence.constr list;
    (** some constraints on the precedence *)

  mutable preprocess : ctx:Ctx.t -> PFormula.t list -> PFormula.t list;
    (** how to preprocess the initial list of formulas *)

  mutable state : ProofState.t;
    (** Proof state *)

  mutable empty_clauses : Clause.CSet.t;
    (** Set of empty clauses *)

  mutable on_empty : (Clause.t -> unit) list;
    (** Callbacks for empty clause detection *)
}

(** {2 Basic operations} *)

val create : ?meta:MetaProverState.t -> ctx:Ctx.t -> Params.t ->
             Signature.t -> t
  (** Create an environment (initially empty) *)

val add_passive : env:t -> Clause.t Sequence.t -> unit

val add_active : env:t -> Clause.t Sequence.t -> unit

val add_simpl : env:t -> Clause.t Sequence.t -> unit

val remove_passive : env:t -> Clause.t Sequence.t -> unit

val remove_passive_id : env:t -> int Sequence.t -> unit

val remove_active : env:t -> Clause.t Sequence.t -> unit

val remove_simpl  : env:t -> Clause.t Sequence.t -> unit

val clean_passive : env:t -> unit
  (** Clean passive set *)

val add_constrs : env:t -> Precedence.constr Sequence.t -> unit

val add_mk_constr : env:t -> (Clause.t Sequence.t -> Precedence.constr list) -> unit

val get_passive : env:t -> Clause.t Sequence.t

val get_active : env:t -> Clause.t Sequence.t

val get_simpl : env:t -> Clause.t Sequence.t

val add_binary_inf : env:t -> string -> binary_inf_rule -> unit

val add_unary_inf : env:t -> string -> unary_inf_rule -> unit

val add_expert : env:t -> Experts.t -> unit

val add_rewrite_rule : env:t -> string -> (Term.t -> Term.t) -> unit

val add_lit_rule : env:t -> string -> lit_rewrite_rule -> unit

val list_simplify : env:t -> Clause.t -> Clause.t list

val get_experts : env:t -> Experts.Set.t

val get_meta : env:t -> MetaProverState.t option

val get_params : env:t -> Params.t

val get_empty_clauses : env:t -> Clause.CSet.t
val get_some_empty_clause : env:t -> Clause.t option

val add_on_empty : env:t -> (Clause.t -> unit) -> unit

val compute_constrs : env:t -> Clause.t Sequence.t -> Precedence.constr list
  (** Compute all ordering constraints for the given list of clauses *)

val ord : t -> Ordering.t
val precedence : t -> Precedence.t
val signature : t -> Signature.t

val pp : Buffer.t -> t -> unit
val fmt : Format.formatter -> t -> unit

(** {2 High level operations} *)

type stats = int * int * int
  (** statistics on clauses : num active, num passive, num simplification *)

val cnf : env:t -> PFormula.t list -> Clause.t list
  (** Reduce formulas to CNF *)

val stats : env:t -> stats
  (** Compute stats *)

val next_passive : env:t -> Clause.t option
  (** Extract next passive clause *)

val do_binary_inferences : env:t -> Clause.t -> Clause.t Sequence.t
  (** do binary inferences that involve the given clause *)

val do_unary_inferences : env:t -> Clause.t -> Clause.t Sequence.t
  (** do unary inferences for the given clause *)

val is_trivial : env:t -> Clause.t -> bool
  (** Check whether the clause is trivial (also with Experts) *)

val simplify : env:t -> Clause.t -> Clause.t * Clause.t
  (** Simplify the hclause. Returns both the hclause and its simplification. *)

val backward_simplify : env:t -> Clause.t -> Clause.CSet.t * Clause.t Sequence.t
  (** Perform backward simplification with the given clause. It returns the
      CSet of clauses that become redundant, and the sequence of those
      very same clauses after simplification. *)

val forward_simplify : env:t -> Clause.t -> Clause.t Sequence.t
  (** Simplify the clause w.r.t to the active set and experts *)

val remove_orphans : env:t -> Clause.t Sequence.t -> unit
  (** remove orphans of the (now redundant) clauses *)

val generate : env:t -> Clause.t -> Clause.t Sequence.t
  (** Perform all generating inferences *)

val is_redundant : env:t -> Clause.t -> bool
  (** Is the given clause redundant w.r.t the active set? *)

val subsumed_by : env:t -> Clause.t -> Clause.t list
  (** List of active clauses subsumed by the given clause *)

val all_simplify : env:t -> Clause.t -> Clause.t list
  (** Use all simplification rules to convert a clause into a list of maximally
      simplified clauses (possibly empty, if trivial). *)

val meta_step : env:t -> Clause.t -> Clause.t Sequence.t
  (** Do one step of the meta-prover with the current given clause. New clauses
      (lemmas) are returned. *)
  
val preprocess : env:t -> PFormula.t list -> PFormula.t list
  (** Preprocess clauses *)


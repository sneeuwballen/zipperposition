(* This file is free software, part of Zipperposition. See file "license" for more details. *)

open Logtk

(** The Env module type — defines the interface for the prover environment. *)
module type S = sig
  type t

  module Ctx : module type of Ctx
  module C : Clause_intf.S with type t = Clause.t
  module ProofState : module type of ProofState
  module Stm : module type of Stream
  module StmQ : module type of StreamQueue
  module FormRename : FormulaRename.S with module Ctx = Ctx and module C = C

  type inf_rule = t -> Clause.t -> Clause.t list
  type generate_rule = t -> full:bool -> unit -> Clause.t list
  type clause_elim_rule = t -> unit
  type binary_inf_rule = inf_rule
  type unary_inf_rule = inf_rule
  type simplify_rule = t -> Clause.t -> Clause.t SimplM.t
  type active_simplify_rule = simplify_rule
  type rw_simplify_rule = simplify_rule
  type backward_simplify_rule = t -> Clause.t -> Clause.ClauseSet.t
  type redundant_rule = t -> Clause.t -> bool

  type backward_redundant_rule =
    t -> Clause.ClauseSet.t -> Clause.t -> Clause.ClauseSet.t

  type immediate_simplification_rule =
    t -> Clause.t -> Clause.t Iter.t -> Clause.t Iter.t option

  type is_trivial_trail_rule = t -> Trail.t -> bool
  type is_trivial_rule = t -> Clause.t -> bool
  type term_rewrite_rule = t -> Term.t -> (Term.t * Proof.parent list) option
  type term_norm_rule = t -> Term.t -> Term.t option

  type lit_rewrite_rule =
    t -> Literal.t -> (Literal.t * Proof.parent list * Proof.tag list) option

  type multi_simpl_rule = t -> Clause.t -> Clause.t list option

  type 'a conversion_result =
    | CR_skip
    | CR_drop
    | CR_add of 'a
    | CR_return of 'a

  type clause_conversion_rule =
    t -> Statement.clause_t -> Clause.t list conversion_result

  type stats = int * int * int
end

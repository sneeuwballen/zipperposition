(* This file is free software, part of Zipperposition. See file "license" for more details. *)

open Logtk

(** The old-style bridge module type — used by unconverted calculus modules.
    Functions don't take [t] and instead use a global [Env.t] ref. *)
module type S = sig
  module Ctx : module type of Ctx
  module C : Clause_intf.S with type t = Clause.t
  module ProofState : module type of ProofState
  module Stm : module type of Stream
  module StmQ : module type of StreamQueue
  module FormRename : FormulaRename.S with module Ctx = Ctx and module C = C

  type inf_rule = Clause.t -> Clause.t list
  type generate_rule = full:bool -> unit -> Clause.t list
  type clause_elim_rule = unit -> unit
  type binary_inf_rule = inf_rule
  type unary_inf_rule = inf_rule
  type simplify_rule = Clause.t -> Clause.t SimplM.t
  type active_simplify_rule = simplify_rule
  type rw_simplify_rule = simplify_rule
  type backward_simplify_rule = Clause.t -> Clause.ClauseSet.t
  type redundant_rule = Clause.t -> bool

  type backward_redundant_rule =
    Clause.ClauseSet.t -> Clause.t -> Clause.ClauseSet.t

  type immediate_simplification_rule =
    Clause.t -> Clause.t Iter.t -> Clause.t Iter.t option

  type is_trivial_trail_rule = Trail.t -> bool
  type is_trivial_rule = Clause.t -> bool
  type term_rewrite_rule = Term.t -> (Term.t * Proof.parent list) option
  type term_norm_rule = Term.t -> Term.t option

  type lit_rewrite_rule =
    Literal.t -> (Literal.t * Proof.parent list * Proof.tag list) option

  type multi_simpl_rule = Clause.t -> Clause.t list option

  type 'a conversion_result =
    | CR_skip
    | CR_drop
    | CR_add of 'a
    | CR_return of 'a

  type clause_conversion_rule =
    Statement.clause_t -> Clause.t list conversion_result

  type stats = int * int * int
end

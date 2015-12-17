
(* This file is free software, part of Zipperposition. See file "license" for more details. *)

(** {1 Preprocessing Env} *)

open Logtk

(** This module is used for preprocessing problems. It computes the fixpoint
    of operations on a set of {!PFormula.t}. *)

(** {2 Transformations} *)

type operation_result =
  | SimplifyInto of TypedSTerm.t (** replace by formula *)
  | Remove (** remove formula *)
  | Esa of TypedSTerm.t list (** replace by list of formulas *)
  | Add of TypedSTerm.t list (** add given formulas, and restart! *)
  | AddOps of operation list (** New operations to perform, and restart *)

and operation = PFormula.Set.t -> PFormula.t -> operation_result list
(** An operation can have several results *)

val fix : operation list -> PFormula.Set.t -> PFormula.Set.t
(** Fixpoint of the given set of operations on the initial set. For a
    clause or formula f, if any operation in the list returns:

    - DoNothing: does nothing
    - Esa l: fixpoint for each element of l
    - Add l: keep f, but also take the fixpoint of l
    - SimplifyInto f': continue with f' instead of f
*)

val remove_trivial : operation
(** Remove trivial formulas *)

val cnf : operation
(** Transform clauses into their clausal normal form *)

val fmap_term : rule:string -> (FOTerm.t -> FOTerm.t) -> operation
(** Transformation on terms *)

val expand_def : operation
(** Expand definitions of terms and predicates *)

(** {2 Preprocessing} *)

type t
(** Environment used for preprocessing of the problem *)

val create : ?base:Signature.t -> Params.t -> t
(** Create a new preprocessing env.
    @param base initial signature *)

val copy : t -> t
(** Copy of the preprocessing env. Shares the same meta prover, if any *)

val get_params : penv:t -> Params.t
(** Parameters *)

val signature : penv:t -> Signature.t
(** Base signature *)

val add_base_sig : penv:t -> Signature.t -> unit
(** Declare a set of base symbols *)

val add_axiom : penv:t -> PFormula.t -> unit
(** Add a single axiom. Preprocessed sets will be enriched with the
    axiom. *)

val add_axioms : penv:t -> PFormula.t Sequence.t -> unit
(** Add a set of axioms *)

val add_operation : penv:t -> prio:int -> operation -> unit
(** Add a preprocessing operation. [prio] is the priority of the operation;
    the higher the priority, the latter the operation is run (ie
    operations with low priority are tried first) *)

val add_operation_rule : penv:t -> prio:int -> (PFormula.Set.t -> operation) -> unit
(** Add an operation that depends on the initial set of formulas to process *)

val add_constr : penv:t -> int -> [`partial] Precedence.Constr.t -> unit
(** Add a precedence constraint with its priority. The lower the
    priority, the stronger influence the constraint will have. *)

val add_constrs : penv:t -> (int * [`partial] Precedence.Constr.t) list -> unit

val add_constr_rule : penv:t -> int -> (PFormula.Set.t -> [`partial] Precedence.Constr.t) -> unit
(** Add a precedence constraint rule *)

val set_weight_rule : penv:t -> (PFormula.Set.t -> ID.t -> int) -> unit
(** Choose the way weights are computed *)

val add_status : penv:t -> (ID.t * Precedence.symbol_status) list -> unit
(** Specify explicitely the status of some symbols *)

val mk_precedence : penv:t -> PFormula.Set.t ->
  Precedence.t * (int * [`partial] Precedence.Constr.t) list
(** Make a precedence out of the formulas and constraints. Returns the
    precedence and the list of constraints used to build it *)

val process : penv:t -> PFormula.Set.t -> PFormula.Set.t
(** Process the input formulas recursively *)

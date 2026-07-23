(* This file is free software, part of Zipperposition. See file "license" for more details. *)

open Logtk

(** {1 Global environment for an instance of the prover} *)

module type S = Env_intf.S

module Ctx : module type of Ctx
module C : Clause_intf.S with type t = Clause.t
module ProofState : module type of ProofState
module Stm : module type of Stream
module StmQ : module type of StreamQueue
module FormRename : FormulaRename.S with module Ctx = Ctx and module C = C

type t

type 'a packed = unit
(** Temporary backward compat *)

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

type term_rewrite_rule =
  t -> Logtk.Term.t -> (Logtk.Term.t * Proof.parent list) option

type term_norm_rule = t -> Logtk.Term.t -> Logtk.Term.t option

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

val create :
  params:Params.t -> flex_state:Logtk.Flex_state.t -> ctx:Ctx.t -> unit -> t
(** Create a new env record *)

(** Create a new env record *)

val get_ctx : t -> Ctx.t
val flex_state_of : t -> Logtk.Flex_state.t

val flex_get_of : t -> 'a Logtk.Flex_state.key -> 'a
(** Get a value from the environment's flex state *)

val flex_get_or_create : init:(unit -> 'a) -> t -> 'a Logtk.Flex_state.key -> 'a
(** Get or create a flexstate key *)

val convert_input_statements_env :
  t -> Logtk.Statement.clause_t CCVector.ro_vector -> Clause.t Clause.sets
(** Convert input statements to clauses *)

(** {2 Environment operations (take env explicitly)} *)

val add_passive : t -> Clause.t Iter.t -> unit
val add_active : t -> Clause.t Iter.t -> unit
val add_simpl : t -> Clause.t Iter.t -> unit
val remove_passive : t -> Clause.t Iter.t -> unit
val remove_active : t -> Clause.t Iter.t -> unit
val remove_simpl : t -> Clause.t Iter.t -> unit
val get_passive : t -> unit -> Clause.t Iter.t
val get_active : t -> unit -> Clause.t Iter.t
val add_binary_inf : t -> string -> binary_inf_rule -> unit
val add_unary_inf : t -> string -> unary_inf_rule -> unit
val add_rw_simplify : t -> rw_simplify_rule -> unit
val add_active_simplify : t -> active_simplify_rule -> unit
val add_backward_simplify : t -> backward_simplify_rule -> unit
val add_redundant : t -> redundant_rule -> unit
val add_backward_redundant : t -> backward_redundant_rule -> unit
val add_basic_simplify : t -> simplify_rule -> unit
val add_unary_simplify : t -> simplify_rule -> unit
val add_multi_simpl_rule : t -> priority:int -> multi_simpl_rule -> unit
val add_cheap_multi_simpl_rule : t -> multi_simpl_rule -> unit
val add_is_trivial_trail : t -> is_trivial_trail_rule -> unit
val add_is_trivial : t -> is_trivial_rule -> unit
val add_rewrite_rule : t -> string -> term_rewrite_rule -> unit
val set_ho_normalization_rule : t -> string -> term_norm_rule -> unit
val get_ho_normalization_rule : t -> term_norm_rule
val add_immediate_simpl_rule : t -> immediate_simplification_rule -> unit
val add_lit_rule : t -> string -> lit_rewrite_rule -> unit
val add_generate : t -> priority:int -> string -> generate_rule -> unit

val add_clause_elimination_rule :
  t -> priority:int -> string -> clause_elim_rule -> unit

val cr_skip : _ conversion_result
val cr_return : 'a -> 'a conversion_result
val cr_add : 'a -> 'a conversion_result
val add_clause_conversion : t -> clause_conversion_rule -> unit
val add_step_init : t -> (t -> unit) -> unit
val add_fragment_check : t -> (t -> Clause.t -> bool) -> unit
val check_fragment : t -> Clause.t -> bool
val multi_simplify : t -> depth:int -> Clause.t -> (Clause.t * int) list option
val params_of : t -> Params.t
val get_empty_clauses : t -> Clause.ClauseSet.t
val get_some_empty_clause : t -> Clause.t option
val has_empty_clause : t -> bool
val on_start : t -> unit Signal.t
val on_input_statement : t -> Statement.clause_t Signal.t
val on_forward_simplified : t -> (Clause.t * Clause.t option) Signal.t
val on_empty_clause : t -> Clause.t Signal.t
val on_pred_var_elimination : t -> (Clause.t * Term.t) Signal.t
val get_stm_queue : t -> StreamQueue.t
val should_force_stream_eval : t -> unit -> bool
val get_finite_infs : t -> 'a option OSeq.t CCList.t -> 'a CCList.t
val stats : t -> stats
val next_passive : t -> unit -> Clause.t option
val do_binary_inferences : t -> Clause.t -> Clause.t Iter.t
val do_unary_inferences : t -> Clause.t -> Clause.t Iter.t
val do_generate : t -> full:bool -> unit -> Clause.t Iter.t
val do_clause_eliminate : t -> unit
val is_trivial_trail : t -> Trail.t -> bool
val is_trivial : t -> Clause.t -> bool
val is_active : t -> Clause.t -> bool
val is_passive : t -> Clause.t -> bool
val basic_simplify : t -> Clause.t -> Clause.t SimplM.t
val unary_simplify : t -> Clause.t -> Clause.t SimplM.t
val backward_simplify : t -> Clause.t -> Clause.ClauseSet.t * Clause.t Iter.t
val simplify_active_with : t -> (Clause.t -> Clause.t list option) -> unit
val forward_simplify : t -> Clause.t -> Clause.t SimplM.t
val cheap_multi_simplify : t -> Clause.t -> Clause.t list option
val immediate_simplify : t -> Clause.t -> Clause.t Iter.t -> Clause.t Iter.t
val generate : t -> Clause.t -> Clause.t Iter.t
val is_redundant : t -> Clause.t -> bool
val subsumed_by : t -> Clause.t -> Clause.ClauseSet.t
val all_simplify : t -> Clause.t -> Clause.t list SimplM.t

val step_init : t -> unit
(** Run all step init hooks *)

val flex_add_of : t -> 'a Logtk.Flex_state.key -> 'a -> unit
val update_flex_state : t -> (Logtk.Flex_state.t -> Logtk.Flex_state.t) -> unit

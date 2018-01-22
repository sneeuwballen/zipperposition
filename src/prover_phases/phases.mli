
(* This file is free software, part of Zipperposition. See file "license" for more details. *)

(** {1 Phases of the Prover}

    To process a file, the prover goes through a sequence of phases that
    are used to build values. This module reifies the phases. *)

open Logtk
open Libzipperposition

type filename = string
type 'a or_error = ('a, string) CCResult.t

(** {2 Phases} *)

type env_with_clauses =
    Env_clauses : 'c Env.packed * 'c Clause.sets -> env_with_clauses

type env_with_result =
    Env_result : 'c Env.packed * Saturate.szs_status -> env_with_result

type errcode = int

type prelude = UntypedAST.statement Sequence.t

type ('ret, 'before, 'after) phase =
  | Init : (unit, _, [`Init]) phase (* global setup *)
  | Setup_gc : (unit, [`Init], [`Init]) phase
  | Setup_signal : (unit, [`Init], [`Init]) phase
  | Parse_CLI :
      (filename list * Params.t, [`Init], [`Parse_cli]) phase
  (* parse CLI options: get a list of files to process, and parameters *)
  | LoadExtensions : (Extensions.t list, [`Parse_cli], [`LoadExtensions]) phase
  | Parse_prelude : (prelude, [`LoadExtensions], [`Parse_prelude]) phase
  | Start_file :
      (filename, [`Parse_prelude], [`Start_file]) phase (* file to process *)
  | Parse_file :
      (Input_format.t * UntypedAST.statement Sequence.t,
       [`Start_file], [`Parse_file]) phase (* parse some file *)
  | Typing :
      (TypeInference.typed_statement CCVector.ro_vector, [`Parse_file], [`Typing]) phase
  | CNF :
      (Statement.clause_t CCVector.ro_vector, [`Typing], [`CNF]) phase
  | Compute_prec :
      (Precedence.t, [`CNF], [`Precedence]) phase
  | Compute_ord_select :
      (Ordering.t * Selection.t, [`Precedence], [`Compute_ord_select]) phase
  (* compute orderign and selection function *)

  | MakeCtx : ((module Ctx.S), [`Compute_ord_select], [`MakeCtx]) phase

  | MakeEnv : (env_with_clauses, [`MakeCtx], [`MakeEnv]) phase

  | Pre_saturate :
      ('c Env.packed * Saturate.szs_status * 'c Clause.sets,
       [`MakeEnv], [`Pre_saturate]) phase

  | Saturate :
      (env_with_result, [`Pre_saturate], [`Saturate]) phase

  | Print_result : (unit, [`Saturate], [`Print_result]) phase
  | Print_dot : (unit, [`Print_result], [`Print_dot]) phase
  | Check_proof : (errcode, [`Print_dot], [`Check_proof]) phase
  | Print_stats : (unit, [`Check_proof], [`Print_stats]) phase
  | Exit : (unit, _, [`Exit]) phase

type any_phase = Any_phase : (_, _, _) phase -> any_phase
(** A phase hidden in an existential type *)

(** {2 Main Type} *)

(** Monad type, representing an action starting at phase ['p1]
    and stopping at phase ['p2] *)
type (+'a, 'p1, 'p2) t

val string_of_phase : _ phase -> string

val string_of_any_phase : any_phase -> string

val return : 'a -> ('a, 'p, 'p) t
(** Return a value into the monad *)

val fail : string -> (_, _, _) t
(** Fail with the given error message *)

val return_err : 'a or_error -> ('a, 'p, 'p) t

val exit : (unit, _, [`Exit]) t
(** Exit *)

val start_phase : (_, 'p1, 'p2) phase -> (unit, 'p1, [`InPhase of 'p2]) t
(** Start the given phase *)

val return_phase : 'a -> ('a, [`InPhase of 'p2], 'p2) t
(** Finish the given phase *)

val return_phase_err : 'a or_error -> ('a, [`InPhase of 'p2], 'p2) t

val current_phase : (any_phase, _, 'p2) t
(** Get the current phase *)

val with_phase : ('a, 'p1, 'p2) phase -> f:(unit -> 'a) -> ('a, 'p1, 'p2) t
(** Start phase, call [f ()] to get the result, return its result
    using {!return_phase} *)

val with_phase1 : ('b, 'p1, 'p2) phase -> f:('a -> 'b) -> 'a -> ('b, 'p1, 'p2) t

val with_phase2 : ('c, 'p1, 'p2) phase -> f:('a -> 'b -> 'c) -> 'a -> 'b -> ('c, 'p1, 'p2) t

val bind :
  ('a, 'p_before, 'p_middle) t ->
  f:('a -> ('b, 'p_middle, 'p_after) t) ->
  ('b, 'p_before, 'p_after) t
(** [bind state f] calls [f] to go one step further from [state] *)

val map : ('a, 'p1, 'p2) t -> f:('a -> 'b) -> ('b, 'p1, 'p2) t
(** Map the current value *)

val fold_l : f:('a -> 'b -> ('a, 'p, 'p) t) -> x:'a -> 'b list -> ('a, 'p, 'p) t

val run_parallel : (errcode, 'p1, 'p2) t list -> (errcode, 'p1, 'p2) t
(** [run_sequentiel l] runs each action of the list in succession,
    restarting every time with the initial state (once an action
    has finished, its state is discarded). Only the very last state
    is kept.
    If any errcode is non-zero, then the evaluation stops with this errcode *)

module Infix : sig
  val (>>=) : ('a, 'p1, 'p2) t -> ('a -> ('b, 'p2, 'p3) t) -> ('b, 'p1, 'p3) t
  val (>>?=) : 'a or_error -> ('a -> ('b, 'p1, 'p2) t) -> ('b, 'p1, 'p2) t
  val (>|=) : ('a, 'p1, 'p2) t -> ('a -> 'b) -> ('b, 'p1, 'p2) t
end

include module type of Infix

val empty_state : Flex_state.t

val get : (Flex_state.t, 'a, 'a) t

val set : Flex_state.t -> (unit, 'a, 'a) t

val get_key : 'a Flex_state.key -> ('a, 'b, 'b) t
(** [get_key k] returns the value associated with [k] in the state *)

val set_key : 'a Flex_state.key -> 'a -> (unit, 'b, 'b) t

val update : f:(Flex_state.t -> Flex_state.t) -> (unit, 'a, 'a) t
(** [update ~f] changes the state using [f] *)

val run_with : Flex_state.t -> ('a, [`Init], [`Exit]) t -> (Flex_state.t * 'a) or_error
(** [run_with state m] executes the actions in [m] starting with [state],
    returning some value (or error) and the final state. *)

val run : ('a, [`Init], [`Exit]) t -> (Flex_state.t * 'a) or_error
(** [run m] is [run_with empty_state m] *)

module Key : sig
  val cur_phase : any_phase Flex_state.key
  (** The current phase is stored in the state using this key *)
end

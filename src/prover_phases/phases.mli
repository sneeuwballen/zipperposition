(* This file is free software, part of Zipperposition. See file "license" for more details. *)

(** {1 Phases of the Prover}

    To process a file, the prover goes through a sequence of phases that are
    used to build values. This module reifies the phases. *)

open Logtk
open Libzipperposition

type filename = string
type 'a or_error = ('a, exn * Printexc.raw_backtrace) CCResult.t

(** {2 Phases} *)

type env_with_clauses = Env.t * Clause.t Clause.sets
type env_with_result = Env.t * Saturate.szs_status
type errcode = int
type prelude = UntypedAST.statement Iter.t

type phase =
  | Init
  | Setup_gc
  | Setup_signal
  | Parse_CLI
  | LoadExtensions
  | Parse_prelude
  | Start_file
  | Parse_file
  | Typing
  | CNF
  | Compute_prec
  | Compute_ord_select
  | MakeCtx
  | MakeEnv
  | Pre_saturate
  | Saturate
  | Print_result
  | Print_dot
  | Check_proof
  | Print_stats
  | Exit

(** {2 Main Type} *)

val show_phase : phase -> string

module State : sig
  type t = Flex_state.t
end

type 'a t = State.t ref -> 'a
(** Main state monad *)

val exit : unit t
(** Exit *)

val with_phase : State.t ref -> phase -> (unit -> 'a) -> 'a
(** [with_phase st_ref phase f] runs [f ()] within the given phase, logging
    start/end and adding a trace span. *)

val with_span :
  __FILE__:string -> __LINE__:int -> string -> (Trace.span -> 'a) -> 'a
(** [with_span name f] wraps [f _sp] in a trace span. *)

val failwith : string -> 'a t
(** [failwith msg] raises [Failure msg] (caught by {!run_with}) *)

val get_key : 'a Flex_state.key -> 'a t
(** [get_key k st_ref] returns the value associated with [k] in the state *)

val set_key : 'a Flex_state.key -> 'a -> unit t
(** [set_key k v st_ref] sets [k] to [v] in the state *)

val update : f:(Flex_state.t -> Flex_state.t) -> unit t
(** [update ~f st_ref] changes the state using [f] *)

val run_and_discard_l : int t list -> int t
(** [run_and_discard_l l] runs each action of the list in succession, restarting
    every time with the initial state (once an action has finished, its state is
    discarded). Only the very last state is kept. If any errcode is non-zero,
    then the evaluation stops with this errcode *)

val empty_state : Flex_state.t

val run_with : Flex_state.t -> 'a t -> (Flex_state.t * 'a) or_error
(** [run_with state m] executes the actions in [m] starting with [state],
    returning some value (or error with backtrace) and the final state. *)

val run : 'a t -> (Flex_state.t * 'a) or_error
(** [run m] is [run_with empty_state m] *)

module Key : sig
  val cur_phase : phase Flex_state.key
  (** The current phase is stored in the state using this key *)
end

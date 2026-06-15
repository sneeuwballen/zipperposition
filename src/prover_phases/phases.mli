(* This file is free software, part of Zipperposition. See file "license" for more details. *)

(** {1 Phases of the Prover}

    To process a file, the prover goes through a sequence of phases that are
    used to build values. This module reifies the phases. *)

open Logtk
open Libzipperposition

type filename = string
type 'a or_error = ('a, string) CCResult.t

(** {2 Phases} *)

type env_with_clauses =
  | Env_clauses : 'c Env.packed * 'c Clause.sets -> env_with_clauses

type env_with_result =
  | Env_result : 'c Env.packed * Saturate.szs_status -> env_with_result

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

type 'a t
(** Monad type, representing an action *)

val show_phase : phase -> string

val return : 'a -> 'a t
(** Return a value into the monad *)

val fail : string -> 'a t
(** Fail with the given error message *)

val return_result : 'a or_error -> 'a t

val exit : unit t
(** Exit *)

val with_phase : phase -> (unit -> 'a t) -> 'a t
(** Start phase, call [f ()] to get the result, return its result using
    {!return_phase} *)

val bind : 'a t -> f:('a -> 'b t) -> 'b t
(** [bind state f] calls [f] to go one step further from [state] *)

val with_span :
  __FILE__:string -> __LINE__:int -> string -> (Trace.span -> 'a t) -> 'a t

val map : 'a t -> f:('a -> 'b) -> 'b t
(** Map the current value *)

val fold_l : f:('a -> 'b -> 'a t) -> x:'a -> 'b list -> 'a t

val run_and_discard_l : int t list -> int t
(** [run_and_discard_l l] runs each action of the list in succession, restarting
    every time with the initial state (once an action has finished, its state is
    discarded). Only the very last state is kept. If any errcode is non-zero,
    then the evaluation stops with this errcode *)

module Syntax : sig
  val ( let* ) : 'a t -> ('a -> 'b t) -> 'b t
  val ( let+ ) : 'a t -> ('a -> 'b) -> 'b t
end

val empty_state : Flex_state.t
val get : Flex_state.t t
val set : Flex_state.t -> unit t

val get_key : 'a Flex_state.key -> 'a t
(** [get_key k] returns the value associated with [k] in the state *)

val set_key : 'a Flex_state.key -> 'a -> unit t

val update : f:(Flex_state.t -> Flex_state.t) -> unit t
(** [update ~f] changes the state using [f] *)

val run_with : Flex_state.t -> 'a t -> (Flex_state.t * 'a) or_error
(** [run_with state m] executes the actions in [m] starting with [state],
    returning some value (or error) and the final state. *)

val run : 'a t -> (Flex_state.t * 'a) or_error
(** [run m] is [run_with empty_state m] *)

module Key : sig
  val cur_phase : phase Flex_state.key
  (** The current phase is stored in the state using this key *)
end

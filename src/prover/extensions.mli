
(* This file is free software, part of Zipperposition. See file "license" for more details. *)

open Logtk

(** {1 Dynamic extensions} *)

type 'a or_error = ('a, string) CCResult.t

(** {2 Type Definitions} *)

type state = Flex_state.t

(** An extension is allowed to modify an environment *)
type env_action = (module Env.S) -> unit

type prec_action = state -> Compute_prec.t -> Compute_prec.t
(** Actions that modify the set of rules {!Compute_prec} *)

type 'a state_actions = ('a -> state -> state) list
(* a list of actions parametrized by ['a] *)

type t = {
  name : string;
  prio : int;  (** the lower, the more urgent, the earlier it is loaded *)
  start_file_actions : string state_actions;
  post_parse_actions : UntypedAST.statement Sequence.t state_actions;
  post_typing_actions : TypeInference.typed_statement CCVector.ro_vector state_actions;
  post_cnf_actions: Statement.clause_t CCVector.ro_vector state_actions;
  ord_select_actions : (Ordering.t * Selection.t) state_actions;
  ctx_actions : (module Ctx_intf.S) state_actions;
  prec_actions : prec_action list;
  env_actions : env_action list;
}
(** An extension contains a number of actions that can modify the {!Flex_state.t}
    during preprocessing, or modify the {!Env_intf.S} once it is built. *)

val default : t
(** Default extension. Does nothing. *)

(** {2 Registration} *)

val register : t -> unit
(** Register an extension to the (current) prover. Plugins should call this
    when they are loaded. *)

val extensions : unit -> t list
(** All currently available extensions *)

val by_name : string -> t option
(** Get an extension by its name, if any *)

val names : unit -> string Sequence.t
(** Names of loaded extensions *)


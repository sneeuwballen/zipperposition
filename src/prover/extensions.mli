
(* This file is free software, part of Zipperposition. See file "license" for more details. *)

(** {1 Dynamic extensions} *)

type 'a or_error = [ `Ok of 'a | `Error of string ]

(** {2 Type Definitions} *)

(** An extension is allowed to modify an environment *)
type action =
  | Do of ((module Env.S) -> unit)

type prec_action =
  | Prec_do of (Compute_prec.t -> unit)

type init_action =
  | Init_do of (unit -> unit)

type t = {
  name : string;
  prio : int;  (** the lower, the more urgent, the earlier it is loaded *)
  prec_actions : prec_action list;
  init_actions : init_action list;
  actions : action list;
}
(** An extension contains a number of actions that can modify a {!PEnv},
    an environment {!Env.S} or run some initialization action
    (typically, add some CLI argument) *)

val default : t
(** Default extension. Does nothing. *)

(** {2 Registration} *)

val register : t -> unit
(** Register an extension to the (current) prover. Plugins should call this
    when they are loaded. *)

val apply_env : env:(module Env.S) -> t -> unit
(** Apply the extension to the Env, adding rules, modifying the env
    in place. *)

val apply_prec : Compute_prec.t -> t -> unit
(** Apply the extension to the precedence settings *)

val init : t -> unit
(** Apply all initialization functions of the given extension *)

val extensions : unit -> t list
(** All currently available extensions *)

val by_name : string -> t option
(** Get an extension by its name, if any *)

val names : unit -> string list
(** Names of loaded extensions *)


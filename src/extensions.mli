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

(** {1 Dynamic extensions} *)

open Logtk

type 'a or_error = [ `Ok of 'a | `Error of string ]

(** {2 Type Definitions} *)

(** An extension is allowed to modify an environment *)
type action =
  | Do of ((module Env.S) -> unit)

type penv_action =
  | Penv_do of (PEnv.t -> unit)

type init_action =
  | Init_do of (unit -> unit)

type t = {
  name : string;
  penv_actions : penv_action list;
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

val dyn_load : string -> t or_error
  (** Try to load the extension located in the given file *)

val apply_env : env:(module Env.S) -> t -> unit
  (** Apply the extension to the Env, adding rules, modifying the env
      in place. *)

val apply_penv : penv:PEnv.t -> t -> unit
  (** Apply the extension to the PEnv *)

val init : t -> unit
  (** Apply all initialization functions of the given extension *)

val extensions : unit -> t list
  (** All currently available extensions *)

val by_name : string -> t option
  (** Get an extension by its name, if any *)

val names : unit -> string list
  (** Names of loaded extensions *)

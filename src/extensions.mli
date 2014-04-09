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

(* TODO: also allow to contribute to {!PEnv} *)

(** {2 Type Definitions} *)

(** Actions that can be performed by an extension on a given Env *)
module type ACTION = sig
  module Env : Env.S

  type action =
    | Ext_general of (unit -> unit)
    | Ext_binary_inf_rule of string * Env.binary_inf_rule
    | Ext_unary_inf_rule of string * Env.unary_inf_rule
    | Ext_signal_incompleteness  (** with extension, prover is incomplete *)
    | Ext_term_rewrite of string * (FOTerm.t -> FOTerm.t)
    | Ext_lit_rewrite of string * (Literal.t -> Literal.t)
    | Ext_simplification_rule of (Env.C.t -> Env.C.t)
    (** Action that can be performed by an extension *)
end

module MakeAction(Env : Env.S) : ACTION with module Env = Env

(** An extension, applied to an Env, can apply a list of actions to it *)
module type S = sig
  include ACTION

  val actions : action list
end

(** An extension defines a functor over any Env *)
module type ENV_TO_S =
  functor (Env : Env.S) -> S with module Env = Env

type penv_action =
  | Ext_penv_do of (PEnv.t -> unit)

type t = {
  name : string;
  penv_actions : penv_action list;
  make : (module ENV_TO_S);
}
(** An extension is a named first-class functor that works over any {!Env.S}
    and can also contribute to the preprocessing env. *)

(** {2 Registration} *)

val register : t -> unit
  (** Register an extension to the (current) prover. Plugins should call this
      when they are loaded. *)

type load_result =
  | Ext_success of t
  | Ext_failure of string
  (** Result of an attempt to load a plugin *)

val dyn_load : string -> load_result
  (** Try to load the extension located in the given file *)

val apply : env:(module Env.S) -> t -> unit
  (** Apply the extension to the Env, adding rules, modifying the env
      in place. *)

val apply_list : env:(module Env.S) -> t list -> unit

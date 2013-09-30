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

type t = {
  name : string;
  actions : action list;
} (** An extension *)
and action =
  | Ext_general of (Env.t -> unit)
  | Ext_expert of (ctx:Ctx.t -> Experts.t)
  | Ext_binary_inf_rule of string * Env.binary_inf_rule
  | Ext_unary_inf_rule of string * Env.unary_inf_rule
  | Ext_signal_incompleteness  (** with extension, prover is incomplete *)
  | Ext_term_rewrite of string * (Term.t -> Term.t)
  | Ext_lit_rewrite of string * (ctx:Ctx.t -> Literal.t -> Literal.t)
  | Ext_simplification_rule of (Clause.t -> Clause.t)
  (** Action that can be performed by an extension *)

val register : t -> unit
  (** Register an extension to the (current) prover. Plugins should call this
      when they are loaded. *)

type load_result =
  | Ext_success of t
  | Ext_failure of string
  (** Result of an attempt to load a plugin *)

val dyn_load : string -> load_result
  (** Try to load the extension located in the given file *)

val apply : env:Env.t -> t -> unit
  (** Apply the extension to the Env.t, adding rules, modifying the env
      in place. *)

val apply_list : env:Env.t -> t list -> unit

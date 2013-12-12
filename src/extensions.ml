
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

module T = FOTerm

type t = {
  name : string;
  actions : action list;
} (** An extension *)
and action =
  | Ext_general of (Env.t -> unit)
  | Ext_binary_inf_rule of string * Env.binary_inf_rule
  | Ext_unary_inf_rule of string * Env.unary_inf_rule
  | Ext_signal_incompleteness  (** with extension, prover is incomplete *)
  | Ext_term_rewrite of string * (T.t -> T.t)
  | Ext_lit_rewrite of string * (ctx:Ctx.t -> Literal.t -> Literal.t)
  | Ext_simplification_rule of (Clause.t -> Clause.t)
  (** Action that can be performed by an extension *)

type load_result =
  | Ext_success of t
  | Ext_failure of string
  (** Result of an attempt to load a plugin *)

let (__current : load_result ref) = ref (Ext_failure "could not load plugin")

(* TODO: use a mutex? *)

let register self =
  __current := Ext_success self

let dyn_load filename =
  let filename = Dynlink.adapt_filename filename in
  (* be sure no previous plugin remains *)
  __current := Ext_failure ("could not load file " ^ filename);
  (* load the plugin, that should have called [register] *)
  let current =
    try
      Dynlink.loadfile filename;
      !__current
    with Dynlink.Error e ->
      let s = Dynlink.error_message e in
      Util.debug 0 "%% error loading plugin %s: %s" filename s;
      let msg = "could not load " ^ filename ^ ": " ^ s in
      Ext_failure msg
  in
  current

(** Apply the extension to the Env.t *)
let apply ~env ext =
  let apply_action action = match action with
  | Ext_general f -> f env
  | Ext_binary_inf_rule (name, r) -> Env.add_binary_inf ~env name r
  | Ext_unary_inf_rule (name, r) -> Env.add_unary_inf ~env name r
  | Ext_signal_incompleteness -> Ctx.lost_completeness (Env.ctx env)
  | Ext_term_rewrite (name, rule) ->  (* add rewrite rule *)
    Env.add_rewrite_rule ~env name rule
  | Ext_lit_rewrite (name, rule) ->
    Env.add_lit_rule ~env name rule
  | Ext_simplification_rule r ->  (* add simplifcation rule *)
    Env.add_simplify ~env r
  in
  List.iter apply_action ext.actions

let apply_list ~env l =
  List.iter (apply ~env) l


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

(** {2 Type Definitions} *)

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

module MakeAction(Env : Env.S) = struct
  module Env = Env

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

module type S = sig
  include ACTION

  val actions : action list
end

module type ENV_TO_S =
  functor (Env : Env.S) -> S with module Env = Env

type penv_action =
  | Ext_penv_do of (PEnv.t -> unit)

type init_action =
  | Init_do of (unit -> unit)

type t = {
  name : string;
  penv_actions : penv_action list;
  init_actions : init_action list;
  make : (module ENV_TO_S);
} (** An extension is a named first-class functor that works
      over any {!Env.S} *)

let default = {
  name="<no name>";
  penv_actions = [];
  init_actions = [];
  make =
    let module Make(E:Env.S) = struct
      include MakeAction(E)
      let actions = []
    end in
    (module Make : ENV_TO_S);
}

(** {2 Registration} *)

type load_result =
  | Ext_success of t
  | Ext_failure of string
  (** Result of an attempt to load a plugin *)

let (__current : load_result ref) = ref (Ext_failure "could not load plugin")

let _extensions = Hashtbl.create 11

(* TODO: use a mutex? *)

let register self =
  (* register by name, if loading succeeded *)
  if not (Hashtbl.mem _extensions self.name)
  then begin
    Util.debug 1 "register extension %s..." self.name;
    Hashtbl.replace _extensions self.name self
  end;
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
let apply_env ~env ext =
  let module Env = (val env : Env.S) in
  let module MakePlugin = (val ext.make : ENV_TO_S) in
  let module Plugin = MakePlugin(Env) in
  let apply_action action = match action with
    | Plugin.Ext_general f -> f ()
    | Plugin.Ext_binary_inf_rule (name, r) -> Env.add_binary_inf name r
    | Plugin.Ext_unary_inf_rule (name, r) -> Env.add_unary_inf name r
    | Plugin.Ext_signal_incompleteness -> Env.Ctx.lost_completeness ()
    | Plugin.Ext_term_rewrite (name, rule) ->  (* add rewrite rule *)
      Env.add_rewrite_rule name rule
    | Plugin.Ext_lit_rewrite (name, rule) ->
      Env.add_lit_rule name rule
    | Plugin.Ext_simplification_rule r ->  (* add simplifcation rule *)
      Env.add_simplify r
  in
  List.iter apply_action Plugin.actions

let apply_penv ~penv ext =
  List.iter
    (function Ext_penv_do f -> f penv)
    ext.penv_actions

let init ext =
  List.iter
    (function
      Init_do f -> f ()
    ) ext.init_actions

let extensions () =
  Sequence.of_hashtbl _extensions
    |> Sequence.map snd
    |> Sequence.to_rev_list

let by_name name =
  try Some (Hashtbl.find _extensions name)
  with Not_found -> None

let names () =
  Sequence.of_hashtbl _extensions
    |> Sequence.map fst
    |> Sequence.to_rev_list

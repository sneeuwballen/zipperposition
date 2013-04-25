(*
Zipperposition: a functional superposition prover for prototyping
Copyright (C) 2012 Simon Cruanes

This is free software; you can redistribute it and/or
modify it under the terms of the GNU General Public License
as published by the Free Software Foundation; either version 2
of the License, or (at your option) any later version.

This is distributed in the hope that it will be useful,
but WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
GNU General Public License for more details.

You should have received a copy of the GNU General Public License
along with this program; if not, write to the Free Software
Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA
02110-1301 USA.
*)

(** {1 Dynamic extensions} *)

open Basic

type t = {
  name : string;
  actions : action list;
} (** An extension *)
and action =
  | Ext_general of (Env.t -> unit)
  | Ext_expert of (ctx:context -> Experts.t)
  | Ext_binary_inf_rule of string * Env.binary_inf_rule
  | Ext_unary_inf_rule of string * Env.unary_inf_rule
  | Ext_signal_incompleteness  (** with extension, prover is incomplete *)
  | Ext_term_rewrite of string * (term -> term)
  | Ext_simplification_rule of (hclause -> hclause list)
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
      FoUtils.debug 0 "%% error loading plugin %s: %s" filename s;
      let msg = "could not load " ^ filename ^ ": " ^ s in
      Ext_failure msg
  in
  current

(** Apply the extension to the Env.t *)
let apply_ext ~env ext =
  let apply_action action = match action with
  | Ext_general f -> f env
  | Ext_expert e -> Env.add_expert ~env (e ~ctx:env.Env.ctx)
  | Ext_binary_inf_rule (name, r) -> Env.add_binary_inf ~env name r
  | Ext_unary_inf_rule (name, r) -> Env.add_unary_inf ~env name r
  | Ext_signal_incompleteness -> env.Env.ctx.ctx_complete <- false
  | Ext_term_rewrite (name, rule) ->  (* add rewrite rule *)
    Env.add_rewrite_rule ~env name rule
  | Ext_simplification_rule r ->  (* add simplifcation rule *)
    let list_simplify' = env.Env.list_simplify in
    env.Env.list_simplify <-
      (fun hc -> FoUtils.list_flatmap list_simplify' (r hc))
  in
  List.iter apply_action ext.actions

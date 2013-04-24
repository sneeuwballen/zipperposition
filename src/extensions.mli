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
  | Ext_expert of Experts.t
  | Ext_binary_inf_rule of string * Env.binary_inf_rule
  | Ext_unary_inf_rule of string * Env.unary_inf_rule
  | Ext_simplification_rule of (hclause -> hclause list)
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

val apply_ext : env:Env.t -> t -> unit
  (** Apply the extension to the Env.t, adding rules, modifying the env
      in place. *)

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

type t = {
  init : unit -> unit;
  exit : unit -> unit;
  name : string;
} (** Type of an extension *)
and factory = unit -> t
  (** Type of an extension generator (For stateful extension) *)

val register : factory -> unit
  (** Register an extension to the (current) prover. Plugins should call this
      when they are loaded. *)

val dyn_load : string -> factory option
  (** Try to load the extension located in the given file *)

(* TODO: factory should take a Env.t and return unit (register functions,
   inference rules, etc.) *)

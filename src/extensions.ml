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

let __current = ref None


(*
let __mutex = Mutex.create ()
*)

let register (self : factory) =
  __current := Some self

let dyn_load filename : factory option =
  let filename = Dynlink.adapt_filename filename in
  (* be sure no previous plugin remains *)
  __current := None;
  (* load the plugin, that should have called [register] *)
  let current =
    try
      Dynlink.loadfile filename;
      !__current
    with Dynlink.Error e ->
      let s = Dynlink.error_message e in
      FoUtils.debug 0 "%% error loading plugin %s: %s" filename s;
      None
  (*
  Mutex.acquire __mutex;
  let current =
    try
      Dynlink.loadfile filename;
      let c = !__current in
      Mutex.release __mutex;
      c
    with e ->
      Mutex.release __mutex;
      None
  *)
  in
  current

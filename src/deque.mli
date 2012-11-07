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

(** Imperative deque *)

type 'a t

exception Empty

val create : unit -> 'a t

val is_empty : 'a t -> bool

val length : 'a t -> int

val push_front : 'a t -> 'a -> unit

val push_back : 'a t -> 'a -> unit

val take_back : 'a t -> 'a

val take_front : 'a t -> 'a

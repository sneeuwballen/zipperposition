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

(** {1 Json encoding/decoding} *)

(** Basically a wrapper around Yojson, not to depend too tightly on it *)

type t =
  | Int of int
  | Float of float
  | Bool of bool
  | Null
  | String of string
  | List of t list
  | Assoc of (string * t) list
  (** Json values *)

exception Type_error of string * t
  (** Signal a type error during decoding *)

val type_error : string -> t -> 'a
  (** Raise a type error *)

(** {2 Utils} *)

val to_list : t -> t list
val to_int : t -> int
val to_string : t -> string
val to_bool : t -> bool
val to_float : t -> float
val to_assoc : t -> (string * t) list

val mk_null : t
val mk_int : int -> t
val mk_float : float -> t
val mk_string : string -> t

val mk_list : t list -> t
val mk_list_seq : t Sequence.t -> t
val mk_assoc : (string * t) list -> t

(** {2 Parsing/Printing} *)

val from_string : string -> t
val string_of : t -> string

val out_pretty : out_channel -> t -> unit
val pp_pretty : Format.formatter -> t -> unit

val stream_to_string : t Stream.t -> string
val stream_from_lexbuf : Lexing.lexbuf -> t Stream.t

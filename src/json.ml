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

let type_error s t = raise (Type_error (s, t))

(* conversion to Yojson type *)
let rec to_yojson j : Yojson.Basic.json = match j with
  | Int i -> `Int i
  | Float f -> `Float f
  | Bool b -> `Bool b
  | Null -> `Null
  | String s -> `String s
  | List l -> `List (List.map to_yojson l)
  | Assoc l -> `Assoc (List.map (fun (s,t) -> s, to_yojson t) l)

let rec of_yojson (j : Yojson.Basic.json) : t = match j with
  | `Int i -> Int i
  | `Float f -> Float f
  | `Null -> Null
  | `Bool b -> Bool b
  | `String s -> String s
  | `List l -> List (List.map of_yojson l)
  | `Assoc l -> Assoc (List.map (fun (s,t) -> s, of_yojson t) l)

(** {2 Utils} *)

let to_list j = match j with
  | List l -> l
  | _ -> type_error "expected list" j
let to_int j = match j with
  | Int i -> i
  | _ -> type_error "expected int" j
let to_bool j = match j with
  | Bool b -> b
  | _ -> type_error "expected bool" j
let to_string j = match j with
  | String s -> s
  | _ -> type_error "expected string" j
let to_float j = match j with
  | Float f -> f
  | _ -> type_error "expected float" j
let to_assoc j = match j with
  | Assoc l -> l
  | _ -> type_error "expected assoc" j

let mk_null = Null
let mk_int i = Int i
let mk_float f = Float f
let mk_string s = String s
let mk_list l = List l
let mk_list_seq seq = List (Sequence.to_list seq)
let mk_assoc l = Assoc l

(** Map for streams *)
let map_stream f stream =
  Stream.from
    (fun i -> 
      match Stream.peek stream with
      | None -> None
      | Some x ->
        Stream.junk stream;
        Some (f x))

(** {2 Parsing/Printing} *)

let from_string s =
  of_yojson (Yojson.Basic.from_string s)

let string_of t =
  Yojson.Basic.to_string (to_yojson t)

let out_pretty oc s =
  Yojson.Basic.pretty_to_channel oc (to_yojson s)

let pp_pretty fmt s =
  let str = Yojson.Basic.pretty_to_string (to_yojson s) in
  Format.pp_print_string fmt str

let stream_to_string s =
  let s = map_stream to_yojson s in
  Yojson.Basic.stream_to_string s

let stream_from_lexbuf lexbuf =
  let lexer = Yojson.Basic.init_lexer () in
  let stream = Yojson.Basic.stream_from_lexbuf lexer lexbuf in
  let stream = map_stream of_yojson stream in
  stream

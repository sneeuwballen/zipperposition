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

(* some constants, apparently (from the TPTP parser of Darwin) *)

type location = unit

let first_pos () = 0
let last_pos () = 0
let make_loc _ = ()

(** name of the file being parsed *)
let cur_filename = ref ""

(** lexer state *)
let prev_column_index =
  ref 1

let current_column_index =
  ref 1

let prev_line_index =
  ref 1

let current_line_index =
  ref 1

let current_token =
  ref ""

(** Reset all counters *)
let reset () =
  cur_filename := "";
  prev_column_index := 1;
  current_column_index := 1;
  prev_line_index := 1;
  current_line_index := 1;
  current_token := "";
  ()

(** Print a located message to signal a parse error, and then raise
    Parsing.Parse_error *)
let parse_error msg =
  Format.printf "%% Parse error (%s,%d,%d): %s@."
    !cur_filename !current_line_index !current_column_index msg;
  raise Parsing.Parse_error

(* place for storage *)
let home = "ZIPPERPOSITION_HOME"

let version = "ZIPPERPOSITION_VERSION"

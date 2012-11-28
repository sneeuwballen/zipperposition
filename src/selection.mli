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

(** Selection functions. See "E: a brainiac theorem prover". *)

open Types

val select_max_goal : ?strict:bool -> selection_fun
  (** Select a maximal negative literal, if any, or nothing *)

val select_diff_neg_lit : ?strict:bool -> ord:ordering -> selection_fun
  (** arbitrary negative literal with maximal weight difference between sides *)

val select_complex : ?strict:bool -> ord:ordering -> selection_fun
  (** x!=y, or ground negative lit, or like select_diff_neg_lit *)

val select_complex_except_RR_horn : ?strict:bool -> ord:ordering -> selection_fun
  (** if clause is a restricted range horn clause, then select nothing;
      otherwise, like select_complex *)

val default_selection : ord:ordering -> selection_fun
  (** Default selection function *)

val selection_from_string : ord:ordering -> string -> selection_fun
  (** selection function from string (may fail) *)

val available_selections : unit -> string list
  (** available names for selection functions *)

val check_selected : clause -> unit
  (** check that if some literals are selected, at least one is negative *)

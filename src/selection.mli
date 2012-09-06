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

(** Select a maximal negative literal, if any, or nothing *)
val select_max_goal : selection_fun
(** no selection *)
val select_nothing : selection_fun
(** arbitrary negative literal with maximal weight difference between sides *)
val select_diff_neg_lit : selection_fun
(** x!=y, or ground negative lit, or like select_diff_neg_lit *)
val select_complex : selection_fun
(** if clause is closed horn clause, then select nothing; otherwise, like select_complex *)
val select_complex_except_horn : selection_fun

(** Default selection function *)
val default_selection : selection_fun

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

(** Recognition of theories *)

open Types
open Symbols

module C = Clauses

(** Recognized whether the clause is a Range-Restricted Horn clause *)
let is_RR_horn_clause c = 
  let lit = ref None in
  (* find whether there is exactly one positive literal *)
  let rec find_uniq_pos n lits = match lits with
  | [] when n = 1 -> !lit
  | [] -> None  (* zero or several positive lits *)
  | (Equation (l,r,true,_) as lit')::lits' ->
    lit := Some lit';
    find_uniq_pos (n+1) lits'
  | _::lits' -> find_uniq_pos n lits'
  in
  match find_uniq_pos 0 c.clits with
  | None -> false
  | Some lit' ->
    (* check that all variables of the clause occur in the head *)
    List.length (C.vars_of_lit lit') = List.length c.cvars


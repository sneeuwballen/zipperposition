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

(** Selection functions *)

open Types

let select_max_goal clause =
  (* find negative lits *)
  let negative_lits = List.filter
    (fun ((Equation (_,_,sign,_), _)) -> not sign)
    (C.maxlits clause) in
  match negative_lits with
  | [] -> clause, []  (* select nothing *)
  | (_,idx)::_ -> clause, [idx]  (* select one negative max goal *)

let default_selection = select_bigger_goal

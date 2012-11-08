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

(** Term rewriting *)


(* ----------------------------------------------------------------------
 * Definition of TRS
 * ---------------------------------------------------------------------- *)

(** Term Rewriting System *)
type trs = (term * term) list  (* TODO *)

let id_trs = []

let add_rule trs (l,r) = (l,r) :: trs

let add_rules trs l = List.fold_left add_rule trs l

(* ----------------------------------------------------------------------
 * Computation of normal forms
 * ---------------------------------------------------------------------- *)

(** Rewrite term to its normal form *)
let rewrite trs t = failwith "not implemented"

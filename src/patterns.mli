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

(** Representative patterns for terms and clauses, signature-independent *)

open Types
open Symbols

type psymbol = int
type psort = int

(** A pattern term. Symbols, sorts and variables can all be bound. *)
type pterm =
  | PVar of int * psort
  | PNode of psymbol * psort * pterm list

val compare_pterm : pterm -> pterm -> int
val eq_pterm : pterm -> pterm -> bool

(** A pattern literal is a pair of pattern terms + the sign *)
type pliteral = {
  lterm: pterm;
  lweight: int;
  rterm : pterm;
  rweight: int;
  psign : bool;
}

(** A pattern clause is just a list of pliterals *)
type pclause = pliteral list

(** An abstract substitution maps abstract symbols to symbols, variables and sorts *)
type mapping = {
  mutable m_var : int Ptmap.t;
  mutable m_symbol : symbol Ptmap.t;
}

val empty_mapping : unit -> mapping

(** Reverse mapping, from concrete vars/symbols/sorts to abstract ones *)
type rev_mapping = {
  mutable rm_var : int Ptmap.t;
  rm_symbol : psymbol SHashtbl.t;
  mutable rm_varnum : int;
  mutable rm_symbolnum : int;
}

val empty_rev_mapping : unit -> rev_mapping

(*s canonical patterns for terms, literals and clauses *)

val pterm_of_term : ?rev_map:rev_mapping -> term -> pterm
val plit_of_lit : ?rev_map:rev_mapping -> literal -> pliteral
val pclause_of_clause : ?rev_map:rev_mapping -> hclause -> pclause

(*s instantiate an abstract pattern *)

val instantiate_pterm : map:mapping -> pterm -> term
val instantiate_plit : map:mapping -> ord:ordering -> pliteral -> literal
val instantiate_pclause : map:mapping -> ord:ordering -> pclause -> proof -> hclause list -> hclause

(*s match an abstract pattern against a term of a clause. Failure is
    indicated by an empty list, but several mappings can exist for
    literals and clauses. *)

val match_pterm : map:mapping -> term -> pterm -> mapping option
val match_plit : map:mapping -> literal -> pliteral -> mapping list
val match_pclause : ?map:mapping -> hclause -> pclause -> mapping list

(** An indexing structure that maps pclauses to values *)
module PMap : Map.S with type key = pclause

val pp_pterm : Format.formatter -> pterm -> unit
val pp_pclause : Format.formatter -> pclause -> unit

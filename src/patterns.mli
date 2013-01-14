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

(* ----------------------------------------------------------------------
 * pattern symbol, variables, clauses
 * ---------------------------------------------------------------------- *)

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

val eq_plit : pliteral -> pliteral -> bool
val hash_plit : pliteral -> int

(** A pattern clause is just a list of pliterals. We also keep the canonical
    pattern (starting from 0) of each literal.
    
    Exemple: the clause  p(X) | q(Y,x)  may have
    pc_lits = [f0(X0); f1(X1, X0)]
    pc_canonical = [f0(X0); f0(X0, X1)]
    *)
type pclause = {
  pc_lits : pliteral list;        (* literals that have consistent naming *)
  pc_canonical : pliteral list;   (* canonical pattern of each literal *)
}

val pclause_symbols : pclause -> psymbol list
  (** List of non-special symbols/sort (index) that occur in the clause *)

(* ----------------------------------------------------------------------
 * mapping between regular terms/clauses and pattern terms/clauses
 * ---------------------------------------------------------------------- *)

(** An abstract substitution maps abstract symbols to symbols, variables and sorts *)
type mapping = {
  m_var : int Ptmap.t;
  m_symbol : symbol Ptmap.t;
}

val empty_mapping : mapping
  (** empty mapping *)

val binds_all : mapping -> psymbol list -> bool
  (** Checks whether the mapping binds all symbols in the list *)

val bind_symbol : mapping -> psymbol -> symbol -> mapping
  (** bind a symbol in the mapping *)

(** Reverse mapping, from concrete vars/symbols/sorts to abstract ones *)
type rev_mapping = {
  mutable rm_var : int Ptmap.t;
  rm_symbol : psymbol SHashtbl.t;
  mutable rm_varnum : int;
  mutable rm_symbolnum : int;
}

val empty_rev_mapping : unit -> rev_mapping

(* ----------------------------------------------------------------------
 * conversion to patterns, instantiation of patterns, match pattern
 * ---------------------------------------------------------------------- *)

(*s canonical patterns for terms, literals and clauses.
    The clause pattern is not necessarily unique. *)

val pterm_of_term : ?rev_map:rev_mapping -> term -> pterm
val plit_of_lit : ?rev_map:rev_mapping -> literal -> pliteral
val pclause_of_clause : ?rev_map:rev_mapping -> hclause -> pclause

(*s instantiate an abstract pattern *)

val instantiate_pterm : map:mapping -> pterm -> term
val instantiate_plit : map:mapping -> ord:ordering -> pliteral -> literal
val instantiate_pclause : map:mapping -> ord:ordering -> pclause -> proof -> hclause list -> hclause

(*s match an abstract pattern against a term or a clause. Failure is
    indicated by an empty list, but several mappings can exist for
    literals and clauses. *)

val match_pterm : map:mapping -> pterm -> term -> mapping
val match_plit : map:mapping -> pliteral -> literal -> mapping list
val match_pclause : ?map:mapping -> pclause -> hclause -> mapping list

(* ----------------------------------------------------------------------
 * map from patterns to data, with matching of clauses
 * ---------------------------------------------------------------------- *)

(** Match hclauses with sets of pattern clauses, with some associated values *)
module Map :
  sig
    type 'a t
      (** the mapping *)

    val create : unit -> 'a t
      (** create a mapping *)

    val add : 'a t -> pclause -> 'a -> unit
      (** add a mapping pattern clause -> value *)

    val fold : 'a t -> 'b -> ('b -> pclause -> 'a -> 'b) -> 'b
      (** fold on all stored key->value *)

    val retrieve : 'a t -> hclause -> 'b -> ('b -> pclause -> mapping -> 'a -> 'b) -> 'b
      (** match the hclause with pattern clauses. The callback, fold-like, is called
          on every match with both the pattern and the mapping. *)

    val pp : (Format.formatter -> 'a -> unit) -> Format.formatter -> 'a t -> unit
      (** Pretty print the map, given a printer for values *)
  end

(* ----------------------------------------------------------------------
 * pretty printing
 * ---------------------------------------------------------------------- *)

val pp_pterm : Format.formatter -> pterm -> unit
val pp_pclause : Format.formatter -> pclause -> unit
val pp_mapping : Format.formatter -> mapping -> unit

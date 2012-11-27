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

(** Most of the useful types *)

(** a symbol is just a string *)
open Symbols

(** exception raised when sorts are mismatched *)
exception SortError of string

(** hashconsed term *)
type term = typed_term
(** term with a simple sort *)
and typed_term = {
  term : term_cell;             (** the term itself *)
  sort : sort;                  (** the sort of the term *)
  mutable binding : term;       (** binding of the term (if variable), or normal form *)
  mutable vars : term list;     (** the variables of the term *)
  mutable flags : int;
  mutable tsize : int;          (** number of symbol/vars occurrences in the term (weight) *)
  mutable tag : int;            (** hashconsing tag *)
  mutable hkey : int;           (** hash *)
}
(** content of the term *)
and term_cell =
  | Var of int                  (** variable *)
  | Node of symbol * term list  (** term application *)

(** list of variables *)
type varlist = term list            

(** substitution, a list of (variable -> term) *)
type substitution = (term * term) list

(** (Church-Rosser) term rewriting system *)
type rewriting_system = term -> term

(** partial order comparison *)
type comparison = Lt | Eq | Gt | Incomparable

(** position in a term TODO compact positions *)
type position = int list

(** a literal, that is, a signed equation *)
type literal = 
 | Equation of    term  (** lhs *)
                * term  (** rhs *)
                * bool  (** sign (equality, ie true, or difference) *)
                * comparison   (* TODO remove *)

(** a hashconsed first order clause *)
type hclause = clause
(** a first order clause *)
and clause = {
  ctag : int;                             (** hashconsing tag *)
  clits : literal list;                   (** the equations *)
  cweight : int;                          (** weight of clause *)
  cmaxlits : (literal * int) list Lazy.t; (** maximal literals and their index *)
  cselected : int list;                   (** index of selected literals *)
  cvars : term list;                      (** the free variables *)
  cproof : proof Lazy.t;                  (** the proof for this clause (lazy...) *)
  cparents : clause list;                 (** clauses used to create this one *)
}
(** a proof step for a clause *)
and proof = Axiom of string * string (** file, axiom name *)
          | Proof of string * (clause * position * substitution) list

(** a selection function *)
type selection_fun = clause -> int list

let no_select c = []                          (** selects no literals *)

(** an ordering constraint *)
type ordering_constraint = symbol -> symbol -> int

(** the interface of a total ordering on symbols *)
class type symbol_ordering =
  object
    method refresh : unit -> unit             (** refresh the signature *)
    method signature : symbol list            (** current symbols in decreasing order *)
    method compare : symbol -> symbol -> int  (** total order on symbols *)
    method multiset_status : symbol -> bool   (** does the symbol have a multiset status? *)
    method set_multiset : (symbol -> bool) -> unit  (** set the function that recognized multiset symbols *)
  end

(** the interface of an ordering type *)
class type ordering =
  object
    method refresh : unit -> unit             (** refresh the symbol ordering (the signature) *)
    method clear_cache : unit -> unit         (** clear cache, if any *)
    method symbol_ordering : symbol_ordering
    method compare : term -> term -> comparison
    method compute_term_weight : term -> int
    method compute_clause_weight : clause -> int
    method name : string
  end

exception UnificationFailure

(** A pretty printer of 'a values *)
class type ['a] pp_printer =
  object
    method pp : Format.formatter -> 'a -> unit
  end

(** a statistic object: name and count *)
type statistics = string * int64 ref
let mk_stat name = (name, ref Int64.zero)
let incr_stat (_, count) = count := Int64.add !count Int64.one  (** increment given statistics *)

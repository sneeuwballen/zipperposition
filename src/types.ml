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
type symbol = string

(* connectives *)
let true_symbol = "$true"
let false_symbol = "$false"
let eq_symbol = "="
let exists_symbol = "$$exists"
let forall_symbol = "$$forall"
let lambda_symbol = "$$lambda"
let not_symbol = "$$not"
let imply_symbol = "$$imply"
let and_symbol = "$$and"
let or_symbol = "$$or"

(* De Bruijn *)
let db_symbol = "$$db"
let succ_db_symbol = "$$s"
let subst_symbol = "$$subst"

(** a sort for terms (only the return sort is kept) *)
type sort = string

let type_sort = "$tType"
let bool_sort = "$o"
let univ_sort = "$i"

(** exception raised when sorts are mismatched *)
exception SortError of string

(** hashconsed term *)
type term = typed_term
(** term with a simple sort *)
and typed_term = {
  term : term_cell;             (** the term itself *)
  sort : sort;                  (** the sort of the term *)
  mutable binding : term;       (** binding of the term (if variable), or normal form *)
  mutable vars : term array;    (** the variables of the term *)
  mutable flags : int;
  mutable tsize : int;          (** number of symbol/vars occurrences in the term *)
  mutable tag : int;            (** hashconsing tag *)
  mutable hkey : int;           (** hash *)
}
(** content of the term *)
and term_cell =
  | Var of int                  (** variable *)
  | Node of symbol * term list  (** term application *)

(** array of variables *)
type varset = term Vector.t

(** substitution, a list of (variable -> term) *)
type substitution = (term * term) list

(** (Church-Rosser) term rewriting system *)
type rewriting_system = term -> term

(** partial order comparison *)
type comparison = Lt | Eq | Gt | Incomparable

(** position in a term TODO compact positions *)
type position = int list

(** Equation between terms *)
type equation = 
 | Equation of    term  (** lhs *)
                * term  (** rhs *)
                * bool  (** sign (equality, ie true, or difference) *)
(** a literal, that is, an equation + metadata. It belongs
    only to a clause and is therefore copiable. *)
and literal = {
  lit_eqn : equation;           (** the equation *)
  mutable lit_selected : bool;  (** is the literal selected? *)
  mutable lit_maximal : bool;   (** is the literal maximal in the clause? *)
  mutable lit_hash : int;       (** hash of the literal *)
  mutable lit_depth : int;      (** depth of the literal *)
}

(** a first order (hashconsed) clause *)
type clause = {
  mutable ctag : int;                     (** hashconsing tag *)
  mutable cselected : int;                (** number of selected literals *)
  mutable cvars : term array;             (** the free variables *)
  chkey : int;                            (** hash of the clause *)
  clits : literal array;                  (** the literals (sorted in increasing hash order, duplicates removed) *)
  cproof : proof Lazy.t;                  (** the proof for this clause (lazy...) *)
  cparents : clause list;                 (** clauses used to create this one *)
}
(** a proof step for a clause *)
and proof = Axiom of string * string      (** file, axiom name *)
          | Proof of string * (clause * position * substitution) list

(** a selection function *)
type selection_fun = literal array -> unit

let no_select c = ()                      (** selects no literals *)

(** an ordering constraint *)
type ordering_constraint = symbol -> symbol -> int

(** the interface of a total ordering on symbols *)
class type symbol_ordering =
  object
    method refresh : unit -> unit             (** refresh the signature *)
    method signature : symbol list            (** current symbols in decreasing order *)
    method compare : symbol -> symbol -> int  (** total order on symbols *)
    method weight : symbol -> int             (** weight of symbols *)
    method var_weight : int                   (** weight of variables *)
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

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
type foterm = typed_term
(** term with a simple sort *)
and typed_term = {
  term : foterm_cell;           (** the term itself *)
  sort : sort;                  (** the sort of the term *)
  db_closed : bool Lazy.t;      (** is the term closed w.r.t. De Bruijn indexes? *)
  vars : foterm list Lazy.t;    (** the variables of the term *)
  tag : int;                    (** hashconsing tag *)
  hkey : int;                   (** hash *)
}
(** content of the term *)
and foterm_cell =
  | Leaf of symbol        (** constant *)
  | Var of int            (** variable *)
  | Node of foterm list   (** term application *)

(** list of variables *)
type varlist = foterm list            

(** substitution, a list of variables -> term *)
type substitution = (foterm * foterm) list

(** partial order comparison *)
type comparison = Lt | Eq | Gt | Incomparable | Invertible

(** direction of an equation (for rewriting) *)
type direction = Left2Right | Right2Left | Nodir

(** position in a term *)
type position = int list

(** a literal, that is, a signed equation *)
type literal = 
 | Equation of    foterm  (* lhs *)
                * foterm  (* rhs *)
                * bool    (* sign *)
                * comparison (* orientation *)

(** a hashconsed first order clause *)
type hclause = clause
(** a first order clause *)
and clause = {
  ctag : int;                             (** hashconsing tag *)
  clits : literal list;                   (** the equations *)
  cmaxlits : (literal * int) list Lazy.t; (** maximal literals and their index *)
  cselected : int list Lazy.t;            (** index of selected literals *)
  cvars : foterm list;                    (** the free variables *)
  cproof : proof Lazy.t;                  (** the proof for this clause (lazy...) *)
  cparents : clause list Lazy.t;          (** clauses used to create this one *)
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
    method compare : foterm -> foterm -> comparison
    method compute_term_weight : foterm -> int
    method compute_clause_weight : clause -> int
    method name : string
  end

exception UnificationFailure of string Lazy.t

(** A pretty printer of 'a values *)
class type ['a] pp_printer =
  object
    method pp : Format.formatter -> 'a -> unit
  end

(** a statistic object: name and count *)
type statistics = string * int64 ref
let mk_stat name = (name, ref Int64.zero)
let incr_stat (_, count) = count := Int64.add !count Int64.one  (** increment given statistics *)

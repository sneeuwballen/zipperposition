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

(** position in a term *)
type position = int list

(** compact position, as an integer *)
type compact_position = int

(** a literal, that is, a signed equation *)
type literal = 
 | Equation of    term  (** lhs *)
                * term  (** rhs *)
                * bool  (** sign (equality, ie true, or difference) *)
                * comparison   (* TODO remove? or just orient equations? *)

(** a small bitvector *)
type bitvector = int

(** a first order clause *)
type clause = {
  cref : hclause;                         (** the normalized clause *)
  clits : literal array;                  (** the equations *)
  cvars : term list;                      (** set of variables *)
}
(** a hashconsed clause, with additional metadata. *)
and hclause = {
  hclits : literal array;                 (** the (normalized) equations *)
  mutable hctag : int;                    (** hashconsing tag *)
  mutable hcweight : int;                 (** weight of clause *)
  mutable hcmaxlits : int;                (** bitvector for maximal literals *)
  mutable hcselected_done : bool;
  mutable hcselected : int;               (** bitvector for selected literals *)
  mutable hcvars : term list;             (** the free variables *)
  hcproof : proof;                        (** the proof for this clause (lazy...) *)
  hcparents : hclause list;               (** parents of the clause *)
  mutable hcdescendants : Ptset.t;        (** the set of descendants of the clause *)
}
(** a proof step for a clause *)
and proof = Axiom of string * string (** file, axiom name *)
          | Proof of string * (clause * position * substitution) list

(** a selection function *)
type selection_fun = hclause -> int list

(** selects no literals *)
let no_select c = []

(** an ordering constraint (a possibly non-total ordering on symbols) *)
type ordering_constraint = symbol -> symbol -> int

(** the interface of a total ordering on symbols *)
class type precedence =
  object
    method version : int                        (** version of the precedence (length of history) *)
    method snapshot : symbol list               (** current symbols in decreasing order *)
    method add_symbols : symbol list -> int     (** add the given symbols (returns how many were new) *)
    method compare : symbol -> symbol -> int    (** total order on symbols *)
    method weight : symbol -> int               (** weight of symbol (for KBO) *)
    method set_weight : (symbol -> int) -> unit (** change the weight function *)
    method multiset_status : symbol -> bool     (** does the symbol have a multiset status? *)
    method set_multiset : (symbol -> bool) -> unit  (** set the function that recognized multiset symbols *)
  end

(** the interface of an ordering type *)
class type ordering =
  object
    method clear_cache : unit -> unit           (** clear cache, if any *)
    method precedence : precedence              (** underlying precedence on symbols *)
    method compare : term -> term -> comparison (** compare two terms *)
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

let mk_stat, print_global_stats =
  let stats = ref [] in
  (* create a stat *)
  (fun name ->
    let stat = (name, ref 0L) in
    stats := stat :: !stats;
    stat),
  (* print stats *)
  (fun () ->
    List.iter
      (fun (name, cnt) -> Format.printf "%% %-30s ... %s@." name (Int64.to_string !cnt))
      !stats)

let incr_stat (_, count) = count := Int64.add !count Int64.one  (** increment given statistics *)

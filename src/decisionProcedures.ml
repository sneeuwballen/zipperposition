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

(** Decision procedures for theories *)

open Types
open Symbols

module T = Terms
module Utils = FoUtils

(** {2 General interface} *)

type dp = {
  dp_name : string;                 (** Theory this procedures decides *)
  dp_descr : string;                (** Description of the procedure *)
  dp_equal : term -> term -> bool;  (** Check whether two terms are equal *)
  dp_sig : SSet.t;                  (** Symbols of the theory *)
  dp_clauses : hclause list;        (** Clauses to add to the problem *)
  dp_canonize : term -> term;       (** Get a canonical form of the term *)
  dp_solve : (term -> term -> substitution) option;
}

(* TODO ensure that clauses in the dp have flag_persistent *)

(** Simple syntaxic criterion to decide whether two decision procedures
    are compatibles: check whether they have no symbol in common. *)
let dp_compatible dp1 dp2 = false (* TODO *)

(** Combine two decision procedures into a new one, that decides
    the combination of their theories, assuming they are compatible. *)
let dp_combine dp1 dp2 = failwith "not implemented"

(** Decide whether this clause is redundant *)
let dp_is_redundant dp hc = false (* TODO *)

(** Simplify the clause *)
let dp_simplify dp hc = hc (* TODO *)

(** {2 Ground joinable sets of equations} *)

(** We use ground convergent sets of equations to decide some equational
    theories. See
    "On using ground joinable equations in equational theorem proving", by
    Avenhaus, Hillenbrand, Lochner *)

type gnd_convergent = {
  gc_ord : string;                    (** name of the ordering *)
  gc_prec : precedence;               (** Precedence *)
  gc_sig : SSet.t;                    (** Symbols of the theory *)
  gc_equations : literal array list;  (** Equations of the system *)
} (** A set of ground convergent equations, for some order+precedence *)

let mk_gc ~ord clauses = failwith "nope"

(** From a set of ground convergent equations, make a decision
    procedure that can be used by the prover *)
let gc_to_dp gc = failwith "nope"

(** Pretty-print the system of ground convergent equations *)
let pp_gc formatter gc = failwith "nope"

(** {3 JSON encoding} *)

let gc_to_json gc = failwith "nope"

let gc_of_json ~ctx json = failwith "nope"

(** {2 Some builtin theories} *)

(** Theory of Associative-Commutative symbols, for the given symbol *)
let ac f = 
  (* function that computes the AC(f)-normal form of the term *)
  let nf t = failwith "AC not implemented"
  in
  let dp = {
    dp_name = Utils.sprintf "AC_%s" (name_symbol f);
    dp_descr = Utils.sprintf "AC for symbol %s" (name_symbol f);
    dp_equal = fun t1 t2 -> nf t1 == nf t2
    dp_sig = SSet.singleton f;
    dp_canonize t = nf t;
    dp_solve = None;
  } in
  dp

let assoc f =
  (* function that computes the A(f)-normal form of the term *)
  let nf t = failwith "A not implemented"
  in
  let dp = {
    dp_name = Utils.sprintf "A_%s" (name_symbol f);
    dp_descr = Utils.sprintf "A for symbol %s" (name_symbol f);
    dp_equal = fun t1 t2 -> nf t1 == nf t2
    dp_sig = SSet.singleton f;
    dp_canonize t = nf t;
    dp_solve = None;
  } in
  dp

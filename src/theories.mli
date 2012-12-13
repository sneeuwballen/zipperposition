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

(* ----------------------------------------------------------------------
 * recognize some shapes of clauses
 * ---------------------------------------------------------------------- *)

val is_RR_horn_clause : hclause -> bool
  (** Recognized whether the clause is a Range-Restricted Horn clause *)

val is_definition : hclause -> (term * term) option
  (** Check whether the clause defines a symbol, e.g.
      subset(X,Y) = \forall Z(Z in X -> Z in Y). It means the LHS
      is a flat symbol with variables, and all variables in RHS
      are also in LHS *)

val is_rewrite_rule : hclause -> (term * term) list
  (** More general than definition. It means the clause is an
      equality where all variables in RHS are also in LHS. It
      can return two rewrite rules if the clause can be oriented
      in both ways, e.g. associativity axiom. *)

val is_pos_eq : hclause -> (term * term) option
  (** Recognize whether the clause is a positive unit equality. *)


val is_functional_symbol : hclause -> [ `Functional of symbol | `None ]
  (** detect whether the clause is "p(x,y,z) & p(x,y,z') => z=z'", and
      returns p in this case *)

val is_total_symbol : hclause -> [ `Total of (symbol * symbol) | `None ]
  (** detect whether the clause is "p(x,y,f(x,y))", and returns (p,f)
      in this case *)

(* ----------------------------------------------------------------------
 * add some axioms when detecting some axioms
 * ---------------------------------------------------------------------- *)

val detect_total_relations : ord:ordering -> hclause list -> hclause list
  (** adds axioms for all total functional relations:
      if  p(x,y,f(x,y))  and  p(x,y,z) & p(x,y,z') => z=z'
      we add  p(x,y,z) <=> (z = f(x,y))  as a definition of p *)

(* ----------------------------------------------------------------------
 * generic representation of theories and formulas (persistent)
 * ---------------------------------------------------------------------- *)

type tterm =
  | TVar of int
  | TNode of string * tterm list
  (** an abstract term *)

type tformula = tterm list
  (** an abstract clause *)

val tterm_of_term : term -> tterm
val tformula_of_hclause : hclause -> tformula

type lemma = tformula * tformula list
  (** a lemma is a clause, with some hypothesis *)

type theory = tformula list
  (** a theory is a list of formula *) 

type kb = {
  kb_lemma_idx : int;
  kb_potential_lemmas : lemma list;     (** potential lemma, to explore *)
  kb_lemmas : (int * lemma) list;       (** lemma with their unique ID *)
  kb_theories : (string * theory) list; (** theories, with their name *)
} (** a Knowledge Base for lemma and theories *)

val empty_kb : kb

val add_potential_lemmas : kb -> lemma list -> kb
val add_lemmas : kb -> lemma list -> kb

(* ----------------------------------------------------------------------
 * (heuristic) search of "interesting" lemma in a proof.
 * ---------------------------------------------------------------------- *)

val search_lemmas : hclause -> lemma list
  (** given an empty clause (and its proof), look in the proof for
      potential lemma. *)

(* ----------------------------------------------------------------------
 * serialization/deserialization for abstract logic structures
 * ---------------------------------------------------------------------- *)

val tterm_of_sexp : Sexplib.Sexp.t -> tterm
val sexp_of_tterm : tterm -> Sexplib.Sexp.t

val tformula_of_sexp : Sexplib.Sexp.t -> tformula
val sexp_of_tformula : tformula -> Sexplib.Sexp.t

val sexp_of_lemma : lemma -> Sexplib.Sexp.t
val lemma_of_sexp : Sexplib.Sexp.t -> lemma

val kb_of_sexp : Sexplib.Sexp.t -> kb
val sexp_of_kb : kb -> Sexplib.Sexp.t

val read_kb : lock:string -> file:string -> kb
  (** parse KB from file (or gives an empty one) *)

val save_kb : lock:string -> file:string -> kb -> unit
  (** save the KB to the file *)

val update_kb : lock:string -> file:string -> (kb -> kb) -> unit
  (** updates the KB located in given file (with given lock file),
      with the function *)

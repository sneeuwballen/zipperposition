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

val is_const_definition : hclause -> (term * term) option
  (** Checks whether the clause is "const = ground composite term", e.g.
      a clause "aIbUc = inter(a, union(b, c))". In this case it returns
      Some(constant, definition of constant) *)

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

type name = string
  (** The name of a formula. If a formula is part of a known axiomatisation
      it can have a specific name, otherwise just "lemmaX" with X a number
      (e.g. f(X,Y)=f(Y,X) is named "commutativity") *)

val string_of_name : name -> string

type named_formula = {
  nf_name : name;
  nf_vars : int list;                   (* symbols (indexes) to bind. Order matters. *)
  nf_pclause : Patterns.pclause;        (* the pattern of the formula itself *)
} (** A named formula is a pattern clause, plus a name (used for the datalog
      representation of instances of this formula *)

type theory = {
  th_name : name;                       (* name of the theory *)
  th_vars : int list;                   (* symbols to bind *)
  th_definition : named_formula list;   (* definition (set of axioms) *)
} (** A theory is a named set of formulas (axioms) *)

type lemma = {
  lemma_name : name;                    (* unique name of the lemma *)
  lemma_conclusion : named_formula;     (* conclusion of the lemma *)
  lemma_premises : named_formula list;  (* hypotheses of the lemma *)
  lemma_vars : int list;                (* symbols to instantiate *)
} (** A lemma is a named formula that can be deduced from a list
      of other named formulas. It will be translated as a datalog rule. *)

type kb = {
  mutable kb_name_idx : int;
  mutable kb_lemma_idx : int;
  mutable kb_potential_lemmas : lemma list;           (** potential lemma, to explore *)
  mutable kb_formulas : named_formula Patterns.Map.t; (** named formulas, indexed by pattern *)
  kb_theories : (name, theory) Hashtbl.t;             (** theories, with their name *)
  mutable kb_lemmas : (name, lemma) Hashtbl.t;        (** lemma *)
} (** a Knowledge Base for lemma and theories *)

val empty_kb : unit -> kb
  (** Create an empty Knowledge Base *)

val add_potential_lemmas : kb -> lemma list -> unit
  (** Add a potential lemma to the KB. The lemma must be checked before
      it is used. *)

(* ----------------------------------------------------------------------
 * reasoning over a problem using Datalog
 * ---------------------------------------------------------------------- *)

type meta_prover = {
  meta_db : Datalog.Logic.db;
  meta_kb : kb;
  meta_ord : ordering;
  mutable meta_lemmas : hclause list;
} (** The main type used to reason over the current proof, detecting axioms
      and theories, inferring lemma... *)

val create_meta : ord:ordering -> kb -> meta_prover
  (** Create a meta_prover, using a knowledge base *)

val scan_clause : meta_prover -> hclause -> hclause list
  (** Scan the given clause to recognize if it matches axioms from the KB;
      if it does, return the lemma that are newly discovered by the Datalog engine.

      It returns lemma that have been discovered by adding the clause. Those
      lemma can be safely added to the problem.
      *)

(* ----------------------------------------------------------------------
 * (heuristic) search of "interesting" lemma in a proof.
 * ---------------------------------------------------------------------- *)

val rate_clause : Patterns.pclause -> float
  (** Heuristic "simplicity and elegance" measure for clauses. The smaller,
      the better. *)

val search_lemmas : hclause -> lemma list
  (** given an empty clause (and its proof), look in the proof for
      potential lemma. *)

(* ----------------------------------------------------------------------
 * serialization/deserialization for abstract logic structures
 * ---------------------------------------------------------------------- *)

val read_kb : lock:string -> file:string -> kb
  (** parse KB from file (or gives an empty one) *)

val save_kb : lock:string -> file:string -> kb -> unit
  (** save the KB to the file *)

val update_kb : lock:string -> file:string -> (kb -> kb) -> unit
  (** updates the KB located in given file (with given lock file),
      with the function *)

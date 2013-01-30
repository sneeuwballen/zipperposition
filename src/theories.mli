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
 * generic representation of theories and lemmas (persistent)
 * ---------------------------------------------------------------------- *)

module Logic : Datalog.Logic.S with type symbol = symbol

type atom_name = symbol
  (** The name of a formula. If a formula is part of a known axiomatisation
      it can have a specific name, otherwise just "lemmaX" with X a number
      (e.g. f(X,Y)=f(Y,X) is named "commutativity") *)

type atom = atom_name * [`Var of int | `Symbol of atom_name] list
  (** An atom in the meta level of reasoning. This represents a fact about
      the current proof search (presence of a theory, of a clause, of a lemma... *)

val compare_atom : atom -> atom -> int

type named_formula = Patterns.named_pattern
  (** A named formula is a pattern clause, plus a name (used for the datalog
      representation of instances of this formula *)

type theory = {
  th_atom : atom;                           (* meta-atom for the theory *)
  th_definition : atom list;                (* definition (set of axioms) *)
} (** A theory is a named set of formulas (axioms) *)

type lemma = {
  lemma_conclusion : atom;                  (* conclusion of the lemma *)
  lemma_premises : atom list;               (* hypotheses of the lemma *)
} (** A lemma is a named formula that can be deduced from a list
      of other named formulas. It will be translated as a datalog rule. *)

val compare_lemma : lemma -> lemma -> int
  (** Arbitrary lexicographic comparison of lemmas *)

val rule_of_lemma : lemma -> Logic.rule
  (** Convert the lemma into a datalog rule *)

module LemmaSet : Set.S with type elt = lemma
  (** Set of lemmas *)

type kb = {
  mutable kb_name_idx : int;
  mutable kb_patterns : named_formula Patterns.Map.t; (** named formulas, indexed by pattern *)
  kb_formulas : named_formula SHashtbl.t;             (** formulas, by name *)
  kb_theories : theory SHashtbl.t;                    (** theories, by name *)
  mutable kb_lemmas : LemmaSet.t;                     (** set of lemmas *)
} (** a Knowledge Base for lemma and theories *)

type disjunction =
  | Lemma of lemma
  | Theory of theory
  | Named of named_formula
  (** Type of an entry in a Knowledge Base file *)

val empty_kb : unit -> kb
  (** Create an empty Knowledge Base *)

val next_name : kb -> atom_name
  (** Find a new name for a formula *)

val load_kb : kb -> disjunction Sequence.t -> unit
  (** Add parsed content to the KB *)

val dump_kb : kb -> disjunction Sequence.t
  (** Dump content of the KB as a sequence of disjunctions *)

val add_named : kb -> named_formula list -> unit
val add_lemmas : kb -> lemma list -> unit
val add_theories : kb -> theory list -> unit

val pp_named_formula : Format.formatter -> named_formula -> unit
val pp_lemma : Format.formatter -> lemma -> unit
val pp_theory : Format.formatter -> theory -> unit

val pp_disjunction : Format.formatter -> disjunction -> unit
  (** Print the disjunction in a human readable form *)
val pp_disjunctions : Format.formatter -> disjunction Sequence.t -> unit

val pp_kb : Format.formatter -> kb -> unit
  (** Print the content of the KB in a human readable form *)

val pp_kb_stats : Format.formatter -> kb -> unit
  (** Print statistics about KB *)

(* ----------------------------------------------------------------------
 * reasoning over a problem using Datalog
 * ---------------------------------------------------------------------- *)

module TermMap : Map.S with type key = Logic.term

type meta_prover = {
  meta_db : Logic.db;
  meta_kb : kb;
  mutable meta_clauses : hclause TermMap.t; (* map terms to hclauses *)
  mutable meta_theories : Logic.term list;  (* detected theories *)
  mutable meta_theory_symbols : SSet.t;
  mutable meta_theory_clauses : Logic.term list Ptmap.t; (* clause -> list of theory terms *)
  mutable meta_ctx : context;
  mutable meta_lemmas : hclause list; (* temp buffer of deduced lemmas *)
} (** The main type used to reason over the current proof, detecting axioms
      and theories, inferring lemma... *)

val create_meta : ctx:context -> kb -> meta_prover
  (** Create a meta_prover, using a knowledge base *)

val meta_update_ctx : ctx:context -> meta_prover -> unit
  (** Update the ordering used by the meta-prover *)

val scan_clause : meta_prover -> hclause -> hclause list
  (** Scan the given clause to recognize if it matches axioms from the KB;
      if it does, return the lemma that are newly discovered by the Datalog engine.

      It returns lemma that have been discovered by adding the clause. Those
      lemma can be safely added to the problem. *)

(* ----------------------------------------------------------------------
 * serialization/deserialization for abstract logic structures
 * ---------------------------------------------------------------------- *)

type kb_parser = in_channel -> disjunction Sequence.t
  (** A parser reads a sequence of disjunctions from a channel *)

type kb_printer = Format.formatter -> disjunction Sequence.t -> unit
  (** A printer prints a disjunction on a channel *)

val read_kb : file:string -> kb_parser:kb_parser -> kb -> unit
  (** parse content of the file (as a list of disjunctions), and add it to the KB *)

val save_kb : file:string -> kb_printer:kb_printer -> kb -> unit
  (** save the KB to the file *)

val clear_kb : lock:string -> file:string -> unit
  (** Erase the content of the KB (remove the file) *)

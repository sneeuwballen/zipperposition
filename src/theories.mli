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
 * recognition of proof
 * ---------------------------------------------------------------------- *)

type proof_hash = Int64.t

val hash_proof : hclause -> proof_hash
  (** Get a (probably) unique hash for this proof *)

module ProofHashSet : Set.S with type elt = proof_hash
  (** Set of proof hashes *)

(* ----------------------------------------------------------------------
 * generic representation of theories and lemmas (persistent)
 * ---------------------------------------------------------------------- *)

type atom_name = string
  (** The name of a formula. If a formula is part of a known axiomatisation
      it can have a specific name, otherwise just "lemmaX" with X a number
      (e.g. f(X,Y)=f(Y,X) is named "commutativity") *)

type atom = atom_name * int list
  (** An atom in the meta level of reasoning. This represents a fact about
      the current proof search (presence of a theory, of a clause, of a lemma... *)

type named_formula = {
  nf_atom : atom;                       (* meta-atom for an instance of the pclause *)
  nf_pclause : Patterns.pclause;        (* the pattern of the formula itself *)
} (** A named formula is a pattern clause, plus a name (used for the datalog
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

val rule_of_lemma : lemma -> Datalog.Logic.rule
  (** Convert the lemma into a datalog rule *)

type kb = {
  mutable kb_name_idx : int;
  mutable kb_potential_lemmas : lemma list;           (** potential lemma, to explore *)
  mutable kb_patterns : named_formula Patterns.Map.t; (** named formulas, indexed by pattern *)
  kb_formulas : (atom_name, named_formula) Hashtbl.t; (** formulas, by name *)
  kb_theories : (atom_name, theory) Hashtbl.t;        (** theories, by name *)
  mutable kb_lemmas : lemma list;                     (** list of lemmas *)
  mutable kb_proofs : ProofHashSet.t;                 (** proofs already met *)
} (** a Knowledge Base for lemma and theories *)

val empty_kb : unit -> kb
  (** Create an empty Knowledge Base *)

val next_name : prefix:string -> kb -> atom_name
  (** Find a new name for a formula *)

val add_potential_lemmas : kb -> lemma list -> unit
  (** Add a potential lemma to the KB. The lemma must be checked before
      it is used. *)

val add_named : kb -> named_formula list -> unit
val add_lemmas : kb -> lemma list -> unit
val add_theories : kb -> theory list -> unit

val pp_named_formula : Format.formatter -> named_formula -> unit
val pp_lemma : Format.formatter -> lemma -> unit
val pp_theory : Format.formatter -> theory -> unit

val pp_kb : Format.formatter -> kb -> unit
  (** Pretty print content of KB *)

(* ----------------------------------------------------------------------
 * reasoning over a problem using Datalog
 * ---------------------------------------------------------------------- *)

type meta_prover = {
  meta_db : Datalog.Logic.db;
  meta_kb : kb;
  mutable meta_theories : Datalog.Logic.term list;  (* detected theories *)
  mutable meta_theory_symbols : SSet.t;
  mutable meta_theory_clauses : Datalog.Logic.term list Ptmap.t; (* clause -> list of theory terms *)
  mutable meta_ord : ordering;
  mutable meta_lemmas : hclause list;
} (** The main type used to reason over the current proof, detecting axioms
      and theories, inferring lemma... *)

val create_meta : ord:ordering -> kb -> meta_prover
  (** Create a meta_prover, using a knowledge base *)

val meta_update_ord : ord:ordering -> meta_prover -> unit
  (** Update the ordering used by the meta-prover *)

val scan_clause : meta_prover -> hclause -> hclause list
  (** Scan the given clause to recognize if it matches axioms from the KB;
      if it does, return the lemma that are newly discovered by the Datalog engine.

      It returns lemma that have been discovered by adding the clause. Those
      lemma can be safely added to the problem.
      *)

(* ----------------------------------------------------------------------
 * Some builtin theories, axioms and lemma
 * ---------------------------------------------------------------------- *)

type disjunction = Lemma of lemma | Theory of theory | Named of named_formula

val load_theory : kb -> disjunction list -> unit
  (** Add parsed content to the KB *)

(* ----------------------------------------------------------------------
 * serialization/deserialization for abstract logic structures
 * ---------------------------------------------------------------------- *)

val read_kb : lock:string -> file:string -> kb
  (** parse KB from file (or gives an empty one) *)

val save_kb : lock:string -> file:string -> kb -> unit
  (** save the KB to the file *)

val update_kb : lock:string -> file:string -> (kb -> kb) -> kb
  (** updates the KB located in given file (with given lock file),
      with the function. It returns the updated kb. *)

val clear_kb : lock:string -> file:string -> unit
  (** Erase the content of the KB (remove the file) *)

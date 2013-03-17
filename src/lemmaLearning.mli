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

(** {1 Extracting lemmas from proofs} *)

open Basic
open Symbols

(** {2 Parameters} *)

val subgraph_min_diameter : int ref     (** Min distance conclusion <-> axioms *)
val simplicity_threshold : float ref    (** Max handicap for conclusion *)
val max_lemmas : int ref                (** Max number of lemmas *)

(** {2 Cut extraction} *)

(** The idea here is, given a clause [c] in a proof graph, to find
    a cut [P] of the subgraph composed of ancestors of [c], such
    that any path from an axiom [a] to [c] contains at least one
    clause of [P].
    That means that from the conjunction of clauses in [P], [c] is provable. *)

val cut : string Proof.ProofGraph.t -> compact_clause proof ->
          compact_clause proof list
  (** Find a cut for the given proof in the graph, from its ancestors, or
      raise Not_found if no cut that covers a big enough portion
      of the proof can be found. *)

(** {2 Lemma learning} *)

val learn_empty : meta:Theories.meta_prover -> compact_clause proof -> Theories.lemma option
  (** From the given proof of the empty clause, find a cut [P] of
      its premises, and learn p_1 & p_2 & ... & p_{n-1} => p_n *)

val learn_subproof : meta:Theories.meta_prover -> compact_clause proof -> Theories.lemma option
  (** From the given proof [c], find a cut [P] of its premises,
      and learn the lemma p_1 & p_2 & ... & p_n => c *)

(** {2 Search for salient clauses *)

val salient_clauses : compact_clause proof -> compact_clause proof list
  (** Find a list of {b salient} clauses in the given proof. Salient clauses
      are small clauses that have many descendants in the proof, and are
      close to the conclusion. Those clauses should be good candidates
      for [learn_subproof].
      Implementation should rely on PageRank on the reverse graph. *)

(** {2 Batteries-included lemma learning} *)

val max_lemmas : int ref
  (** Maximal number of lemmas that can be learnt at once *)

val search_lemmas : Theories.meta_prover -> compact_clause proof ->
                    Theories.lemma list
  (** Given an empty clause (and its proof), learn a list of lemmas *)

val learn_and_update : Theories.meta_prover -> hclause -> unit
  (** Update the KB of this meta-prover by learning from
      the given (empty) clause's proof. The KB is modified
      in place. *)

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

(** Extracting lemmas from proofs *)

open Types
open Symbols

(* ----------------------------------------------------------------------
 * (heuristic) search of "interesting" lemma in a proof.
 * ---------------------------------------------------------------------- *)

val max_lemmas : int ref
  (** Maximal number of lemmas that can be learnt at once *)

val rate_clause : is_theory_symbol:(symbol -> bool) -> hclause -> float
  (** Heuristic "simplicity and elegance" measure for clauses in a proof. Theory
      symbols are less 'costly' than other symbols, as are constants.
      The smaller the result, the better. *)

val search_lemmas : Theories.meta_prover -> hclause -> (Theories.lemma * float) list
  (** Given an empty clause (and its proof), look in the proof for lemmas. *)

val learn_and_update : Theories.meta_prover -> hclause -> unit
  (** Update the KB of this meta-prover by learning from
      the given (empty) clause's proof. The KB is modified
      in place. *)

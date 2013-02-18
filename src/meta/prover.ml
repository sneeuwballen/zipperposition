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

(** {2 The meta-prover itself} *)

open Types

type t = ProverOfDoom
  (** A meta-prover, reasoning at the theory/lemma level *)

(** Fresh meta-prover, using the given KB *)
let create ~ctx kb = ProverOfDoom (* TODO *)

(** Get the current Knowledge Base of the prover *)
let get_kb prover = failwith "no KB here yet"

type result =
  | Deduced of literal array * hclause list
  | Theory of string * term list
  | Expert of Experts.expert
  (** Feedback from the meta-prover *)

(* TODO: call calculus#preprocess on resulting clauses (CNF, etc.) *)

(** Match the clause against patterns known to the KB. Matches
    are added to the Datalog engine, and if some theories and lemma
    are detected they are returned *)
let scan_clause prover lits = [] (* TODO *)

(** List of theories detected so far *)
let theories prover = Sequence.of_list []  (* TODO *)

(** Current list of experts that can be used *)
let experts prover = [] (* TODO *)


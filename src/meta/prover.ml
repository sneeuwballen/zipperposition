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

type t
  (** A meta-prover, reasoning at the theory/lemma level *)

val create : ctx:context -> KB.t -> t
  (** Fresh meta-prover, using the given KB *)

val get_kb : t -> KB.t
  (** Get the current Knowledge Base of the prover *)

type result =
  | Deduced of literal array
  | Theory of string * term list
  | Expert of Experts.expert
  (** Feedback from the meta-prover *)

(* TODO: call calculus#preprocess on resulting clauses (CNF, etc.) *)

val scan_clause : t -> literal array -> result list
  (** Match the clause against patterns known to the KB. Matches
      are added to the Datalog engine, and if some theories and lemma
      are detected they are returned *)

val theories : t -> (string * term list) Sequence.t
  (** List of theories detected so far *)

val experts : t -> Experts.expert list
  (** Current list of experts that can be used *)


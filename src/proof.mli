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

(** {1 Manipulate proofs} *)

open Basic

module IntSet : Set.S with type elt = int

(** {2 Constructors and utils} *)

val mk_axiom : compact_clause -> string -> string -> proof
val mk_proof : compact_clause -> string -> proof list -> proof

val is_axiom : proof -> bool
val is_proof : proof -> bool

val proof_clause : proof -> compact_clause  (** Clause this is a proof of *)
val proof_id : proof -> int                 (** Id of the clause *)
val proof_lits : proof -> literal array     (** literals of the clause *)

val is_proof_of : proof -> hclause -> bool
  (** Is the proof a proof of the clause? *)

val recover_clause : ctx:context -> proof -> hclause
  (** Re-build a clause from its proof *)

val traverse : ?traversed:IntSet.t ref -> proof -> (proof -> unit) -> unit
  (** Traverse the proof. Each proof node is traversed only once,
      using the integer to recognize already traversed proofs. *)

val to_seq : proof -> proof Sequence.t

val depth : proof -> int
  (** Max depth of the proof *)

(** {2 Conversion to a graph of proofs} *)

val to_graph : proof -> (proof, string) PersistentGraph.t
  (** Get a graph of the proof *)

val bij : ord:ordering -> proof Bij.t
  (** Bijection. A global table of proof steps is maintained! Use a fresh
      bijection to get a fresh proof steps table. *)

(** {2 Pretty printer for proofs} *)

val pp_proof_tstp : Format.formatter -> proof -> unit
val pp_proof_debug : Format.formatter -> proof -> unit
val pp_proof : string -> Format.formatter -> proof -> unit
  (** Prints the proof according to the given input switch *)

val pp_dot : name:string -> Format.formatter -> proof -> unit
  (** Pretty print the proof as a DOT graph *)

val pp_dot_file : ?name:string -> string -> proof -> unit
  (** print to dot into a file *)


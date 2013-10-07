
(*
Zipperposition: a functional superposition prover for prototyping
Copyright (c) 2013, Simon Cruanes
All rights reserved.

Redistribution and use in source and binary forms, with or without
modification, are permitted provided that the following conditions are met:

Redistributions of source code must retain the above copyright notice, this
list of conditions and the following disclaimer.  Redistributions in binary
form must reproduce the above copyright notice, this list of conditions and the
following disclaimer in the documentation and/or other materials provided with
the distribution.

THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS "AS IS" AND
ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED
WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE ARE
DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT HOLDER OR CONTRIBUTORS BE LIABLE
FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL
DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR
SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER
CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY,
OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE
OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
*)

(** {1 Manipulate proofs} *)

open Logtk

type t = private
  | Axiom of string * string (* filename, axiom name *)
  | InferForm of Formula.t * step
  | InferClause of CompactClause.t * step
and step = {
  rule : string;
  parents : t array;
  esa : bool;  (** Equisatisfiable step? *)
  theories : string list;
}

(** {b note}: Equality does not take the parents into account. Two
    proofs that have the same conclusion are equal. *)

val eq : t -> t -> bool
val hash : t -> int
val cmp : t -> t -> int

(** {2 Constructors and utils} *)

(** Rule must {b NOT} be "axiom" for deduction steps.
    [esa] is false by default. *)

val mk_f_axiom : Formula.t -> file:string -> name:string -> t
val mk_c_axiom : CompactClause.t -> file:string -> name:string -> t

val mk_f_step : ?theories:string list -> ?esa:bool ->
                Formula.t -> rule:string -> t list -> t
  (** Inference step that lead to the given formula. [esa = true] means
      that the inference only preserved equisatisfiability (default false),
      [theories] is a list of theories that are used by the inference
      (default [["equality"]]). *)

val mk_c_step : ?theories:string list -> ?esa:bool ->
                CompactClause.t -> rule:string -> t list -> t
  (** Inference step that lead to the given clause. [esa = true] means
      that the inference only preserved equisatisfiability (default false),
      [theories] is a list of theories that are used by the inference
      (default [["equality"]]). *)

val adapt_f : t -> Formula.t -> t
val adapt_c : t -> CompactClause.t -> t

val is_axiom : t -> bool
val is_proof_of_false : t -> bool

(** {2 Proof traversal} *)

module ProofTbl : Hashtbl.S with type key = t

type proof_set = unit ProofTbl.t

type proof_name = int ProofTbl.t

val traverse : ?traversed:proof_set -> t -> (t -> unit) -> unit
  (** Traverse the proof. Each proof node is traversed only once,
      using the set to recognize already traversed proofs. *)

val get_name : namespace:proof_name -> t -> int
  (** Unique name of the proof, within the given [namespace] *)

val to_seq : t -> t Sequence.t
  (** Traverse the subproofs, once each *)

val depth : t -> int
  (** Max depth of the proof *)

val share : t -> t
  (** Share common subproofs, physically *)

(** {2 Conversion to a graph of proofs} *)

val as_graph : (t, t, string) LazyGraph.t
  (** Get a graph of the proof *)

val bij : t Bij.t

(** {2 IO} *)

val pp_notrec : Buffer.t -> t -> unit
  (** Print the step in debug mode, but not its parents *)

val fmt : Format.formatter -> t -> unit
  (** Non recursive printing on formatter *)

val pp_tstp : Buffer.t -> t -> unit
val pp_debug : Buffer.t -> t -> unit
val pp : string -> Buffer.t -> t -> unit
  (** Prints the proof according to the given input switch *)

val as_dot_graph : (t, LazyGraph.Dot.attribute list, LazyGraph.Dot.attribute list) LazyGraph.t

val pp_dot : name:string -> Buffer.t -> t -> unit
  (** Pretty print the proof as a DOT graph *)

val pp_dot_file : ?name:string -> string -> t -> unit
  (** print to dot into a file *)


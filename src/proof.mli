
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

(** Classification of proof steps *)
type step_kind =
  | Inference of string
  | Simplification of string
  | Esa of string
  | File of string * string * string  (** role, file, name *)
  | Trivial (** trivial, or trivial within theories *)

type step_result =
  | Form of FOFormula.t
  | Clause of CompactClause.t

type t = private {
  result : step_result;       (** conclusion of the step *)
  kind : step_kind;           (** kind of step *)
  parents : t array;          (** parent proof steps *)
  theories : string list;     (** theories used for the proof step *)
  additional_info : string list;   (** additional info, prover-dependent *)
}

(** {b note}: Equality, hashing and comparison do not take the parents into
account. Two proofs that have the same conclusion are equal. *)

val eq : t -> t -> bool
val hash : t -> int
val cmp : t -> t -> int

(** {2 Constructors and utils}
In all the following constructors, [theories] defaults to the empty list.
Axiom constructors have default role "axiom" *)

val mk_f_trivial : ?info:string list -> ?theories:string list -> FOFormula.t -> t

val mk_f_file : ?info:string list -> ?theories:string list ->
                role:string -> file:string -> name:string ->
                FOFormula.t -> t

val mk_f_inference : ?info:string list -> ?theories:string list -> rule:string ->
                     FOFormula.t -> t list -> t

val mk_f_simp : ?info:string list -> ?theories:string list -> rule:string ->
                 FOFormula.t -> t list -> t

val mk_f_esa : ?info:string list -> ?theories:string list -> rule:string ->
                FOFormula.t -> t list -> t

val mk_c_trivial : ?info:string list -> ?theories:string list -> CompactClause.t -> t

val mk_c_file : ?info:string list -> ?theories:string list ->
                role:string -> file:string -> name:string ->
                CompactClause.t -> t

val mk_c_inference : ?info:string list -> ?theories:string list -> rule:string ->
                     CompactClause.t -> t list -> t

val mk_c_simp : ?info:string list -> ?theories:string list -> rule:string ->
                CompactClause.t -> t list -> t

val mk_c_esa : ?info:string list -> ?theories:string list -> rule:string ->
                CompactClause.t -> t list -> t

val adapt_f : t -> FOFormula.t -> t
val adapt_c : t -> CompactClause.t -> t

val is_trivial : t -> bool
val is_file : t -> bool
val is_axiom : t -> bool
val is_proof_of_false : t -> bool

val rule : t -> string option
  (** Rule name for Esa/Simplification/Inference steps *)

val role : t -> string
  (** TSTP role of the proof step ("plain" for inferences/simp/esa) *)

module Theories : sig
  val eq : string list
  val arith : string list
end

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
  (** TODO, not implemented *)

(** {2 IO} *)

val pp_kind : Buffer.t -> step_kind -> unit
val pp_kind_tstp : Buffer.t -> step_kind -> unit
val pp_result : Buffer.t -> step_result -> unit

val pp_result_of : Buffer.t -> t -> unit
val pp_notrec : Buffer.t -> t -> unit
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

val pp_dot_seq : name:string -> Buffer.t -> t Sequence.t -> unit
  (** Print a set of proofs as a DOT graph, sharing common subproofs *)

val pp_dot_seq_file : ?name:string -> string -> t Sequence.t -> unit
  (** same as {!pp_dot_seq} but into a file *)

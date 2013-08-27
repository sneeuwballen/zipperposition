
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
  | Axiom of CompactClause.t * string * string (** file, axiom name *)
  | Infer of CompactClause.t * string * t list (** Inference *)

(** {2 Constructors and utils} *)

val mk_axiom : CompactClause.t -> string -> string -> t
val mk_infer : CompactClause.t -> string -> t list -> t

val is_axiom : t -> bool
val is_infer : t -> bool

val proof_clause : t -> CompactClause.t   (** Clause this is a proof of *)
val proof_id : t -> int                   (** Id of the clause *)
val proof_lits : t -> Literal.t array     (** literals of the clause *)

val is_proof_of : t -> CompactClause.t -> bool
  (** Is the proof a proof of the clause? *)

module IntSet : Set.S with type elt = int

val traverse : ?traversed:IntSet.t ref -> t -> (t -> unit) -> unit
  (** Traverse the proof. Each proof node is traversed only once,
      using the integer to recognize already traversed proofs. *)

val to_seq : t -> t Sequence.t

val depth : t -> int
  (** Max depth of the proof *)

(** {2 Conversion to a graph of proofs} *)

val to_graph : t -> (t, string) PersistentGraph.t
  (** Get a graph of the proof *)

val bij : ord:Ordering.t -> t Bij.t
  (** Bijection. A global table of proof steps is maintained! Use a fresh
      bijection to get a fresh proof steps table. *)

(** {2 IO} *)

val pp_tstp : Buffer.t -> t -> unit
val pp_debug : Buffer.t -> t -> unit
val pp : string -> Buffer.t -> t -> unit
  (** Prints the proof according to the given input switch *)

val pp_dot : name:string -> Buffer.t -> t -> unit
  (** Pretty print the proof as a DOT graph *)

val pp_dot_file : ?name:string -> string -> t -> unit
  (** print to dot into a file *)



(*
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

(** {6 Trace of a TSTP prover} *)

type id = Ast_tptp.name

type t =
  | Axiom of string * string (* filename, axiom name *)
  | Theory of string (* a theory used to do an inference *)
  | InferForm of FOFormula.t * step lazy_t
  | InferClause of FOFormula.t list * step lazy_t
and step = {
  id : id;
  rule : string;
  parents : t array;
  esa : bool;  (** Equisatisfiable step? *)
}

val eq : t -> t -> bool
val hash : t -> int
val cmp : t -> t -> int

val mk_f_axiom : id:id -> FOFormula.t -> file:string -> name:string -> t
val mk_c_axiom : id:id -> FOFormula.t list -> file:string -> name:string -> t
val mk_f_step : ?esa:bool -> id:id -> FOFormula.t -> rule:string -> t list -> t
val mk_c_step : ?esa:bool -> id:id -> FOFormula.t list -> rule:string -> t list -> t

val is_axiom : t -> bool
val is_theory : t -> bool
val is_step : t -> bool
val is_proof_of_false : t -> bool

val get_id : t -> id
  (** Obtain the ID of the proof step.
      @raise Invalid_argument if the step is Axiom or Theory *)

val force : t -> unit
  (** Force proof, if any *)

(** {3 Proof traversal} *)

module StepTbl : Hashtbl.S with type key = t

type proof_set = unit StepTbl.t

val is_dag : t -> bool
  (** Is the proof a proper DAG? *)

val traverse : ?traversed:proof_set -> t -> (t -> unit) -> unit
  (** Traverse the proof. Each proof node is traversed only once,
      using the set to recognize already traversed proofs. *)

val to_seq : t -> t Sequence.t
  (** Traversal of parent proofs *)

val depth : t -> int
  (** Max depth of the proof *)

val size : t -> int
  (** Number of nodes in the proof *)

(** {3 IO} *)

val of_decls : Ast_tptp.declaration Sequence.t -> t option
  (** Try to extract a proof from a list of TSTP statements *)

val parse : ?recursive:bool -> string -> t option
  (** Try to parse a proof from a file. May rise the same IO errors
      as {!Util_tptp.parse_file}. *)

val pp_tstp : Buffer.t -> t -> unit
  (** Print proof in TSTP format *)

val pp0 : Buffer.t -> t -> unit
  (** debug printing, non recursive *)

val pp1 : Buffer.t -> t -> unit
  (** Print proof step, and its parents *)

val fmt : Format.formatter -> t -> unit

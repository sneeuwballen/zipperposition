
(* This file is free software, part of Zipperposition. See file "license" for more details. *)

(** {1 Manipulate proofs} *)

open Libzipperposition

type form = TypedSTerm.t
type 'a sequence = ('a -> unit) -> unit

(** Classification of proof steps *)
type step_kind =
  | Inference of string
  | Simplification of string
  | Esa of string
  | File of StatementSrc.t
  | Trivial (** trivial, or trivial within theories *)

type step_result =
  | Form of form
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

val equal : t -> t -> bool
include Interfaces.HASH with type t := t
val compare : t -> t -> int

val result : t -> step_result
val kind : t -> step_kind
val parents : t -> t array
val theories : t -> string list
val additional_info : t -> string list

(** {2 Constructors and utils}
    In all the following constructors, [theories] defaults to the empty list.
    Axiom constructors have default role "axiom" *)

val mk_f_trivial : ?info:string list -> ?theories:string list -> form -> t

val mk_f_file : ?conjecture:bool -> ?info:string list -> ?theories:string list ->
  file:string -> name:string ->
  form -> t

val mk_f_inference : ?info:string list -> ?theories:string list -> rule:string ->
  form -> t list -> t

val mk_f_simp : ?info:string list -> ?theories:string list -> rule:string ->
  form -> t list -> t

val mk_f_esa : ?info:string list -> ?theories:string list -> rule:string ->
  form -> t list -> t

val mk_c_trivial : ?info:string list -> ?theories:string list -> CompactClause.t -> t

val mk_c_file : ?conjecture:bool -> ?info:string list -> ?theories:string list ->
  file:string -> name:string ->
  CompactClause.t -> t

val mk_c_src : ?info:string list -> ?theories:string list ->
  src:StatementSrc.t ->
  CompactClause.t -> t

val mk_c_inference : ?info:string list -> ?theories:string list -> rule:string ->
  CompactClause.t -> t list -> t

val mk_c_simp : ?info:string list -> ?theories:string list -> rule:string ->
  CompactClause.t -> t list -> t

val mk_c_esa : ?info:string list -> ?theories:string list -> rule:string ->
  CompactClause.t -> t list -> t

val adapt_f : t -> form -> t
val adapt_c : t -> CompactClause.t -> t

val is_trivial : t -> bool
val is_file : t -> bool
val is_proof_of_false : t -> bool

val rule : t -> string option
(** Rule name for Esa/Simplification/Inference steps *)

val is_conjecture : t -> bool
(** Is the proof a conjecture from a file? *)

module Theories : sig
  val eq : string list
  val arith : string list
end

(** {2 Proof traversal} *)

module ProofTbl : Hashtbl.S with type key = t

type proof_set = unit ProofTbl.t

type proof_name = int ProofTbl.t

val traverse : ?traversed:proof_set -> t -> t sequence
(** Traverse the proof. Each proof node is traversed only once,
    using the set to recognize already traversed proofs. *)

val traverse_depth : ?traversed:proof_set -> t -> (t * int) sequence
(** Traverse the proof, yielding each proof node along with its
    depth from the initial proof. Each proof node is traversed only once,
    using the set to recognize already traversed proofs. *)

val distance_to_conjecture : t -> int option
(** [distance_to_conjecture p] returns [None] if [p] has no ancestor
    that is a conjecture (including [p] itself). It returns [Some d]
    if [d] is the distance, in the proof graph, to the closest
    conjecture ancestor of [p] *)

val get_name : namespace:proof_name -> t -> int
(** Unique name of the proof, within the given [namespace] *)

val to_seq : t -> t Sequence.t
(** Traverse the subproofs, once each *)

val depth : t -> int
(** Max depth of the proof *)

val share : t -> t
(** Share common subproofs, physically *)

(** {2 Conversion to a graph of proofs} *)

val as_graph : (t, t * (string * t)) CCGraph.t
(** Get a graph of the proof *)

(** {2 IO} *)

val pp_kind : step_kind CCFormat.printer
val pp_kind_tstp : step_kind CCFormat.printer
val pp_result : step_result CCFormat.printer

val pp_result_of : t CCFormat.printer
val pp_notrec : t CCFormat.printer
(** Non recursive printing on formatter *)

val pp_tstp : t CCFormat.printer
val pp_debug : t CCFormat.printer
val pp : string -> t CCFormat.printer
(** Prints the proof according to the given input switch *)
(* TODO use variant *)

val pp_dot : name:string -> t CCFormat.printer
(** Pretty print the proof as a DOT graph *)

val pp_dot_file : ?name:string -> string -> t -> unit
(** print to dot into a file *)

val pp_dot_seq : name:string -> t Sequence.t CCFormat.printer
(** Print a set of proofs as a DOT graph, sharing common subproofs *)

val pp_dot_seq_file : ?name:string -> string -> t Sequence.t -> unit
(** same as {!pp_dot_seq} but into a file *)

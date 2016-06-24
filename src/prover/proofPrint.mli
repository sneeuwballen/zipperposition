
(* This file is free software, part of Zipperposition. See file "license" for more details. *)

(** {1 Manipulate proofs} *)

open Libzipperposition

type t = ProofStep.of_

val is_proof_of_false : t -> bool

(** {2 Conversion to a graph of proofs} *)

module Tbl : module type of ProofStep.PTbl

val as_graph : (t, t * ProofStep.rule * t) CCGraph.t
(** Get a graph of the proof *)

val traverse :
  ?traversed:unit Tbl.t ->
  t ->
  t Sequence.t

(** {2 IO} *)

val pp_result : ProofStep.result CCFormat.printer

val pp_result_of : t CCFormat.printer
val pp_notrec : t CCFormat.printer
(** Non recursive printing on formatter *)

val pp_tstp : t CCFormat.printer
val pp_normal : t CCFormat.printer
val pp : Options.print_format -> t CCFormat.printer
(** Prints the proof according to the given input switch *)

val pp_dot : name:string -> t CCFormat.printer
(** Pretty print the proof as a DOT graph *)

val pp_dot_file : ?name:string -> string -> t -> unit
(** print to dot into a file *)

val pp_dot_seq : name:string -> t Sequence.t CCFormat.printer
(** Print a set of proofs as a DOT graph, sharing common subproofs *)

val pp_dot_seq_file : ?name:string -> string -> t Sequence.t -> unit
(** same as {!pp_dot_seq} but into a file *)

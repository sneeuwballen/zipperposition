(* This file is free software, part of Zipperposition. See file "license" for more details. *)

(** {1 Proof Trace Encoder} *)

open Logtk

type t

val create : #Zipperposition_mdag.Encode.output -> t
val close : t -> unit

type stats = {
  n_steps: int;
  n_terms: int;
}

val emit_proof :
  t ->
  get_lits:(Proof.t -> Literal.t array) ->
  Proof.t ->
  Zipperposition_mdag.Encode.offset * stats
(** Encode a proof DAG and write footer. [get_lits] extracts clause literals
    from a proof. *)

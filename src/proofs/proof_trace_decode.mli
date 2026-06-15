(* This file is free software, part of Zipperposition. See file "license" for more details. *)

(** {1 Proof Trace Decoder — Clause-Based} *)

open Logtk

type t
(** Decoder state, holds the binary data and caches *)

val create : string -> t
(** Create decoder from binary file contents (as a string) *)

val decode_proof : t -> LLProof.t * (string * string) list
(** Decode the full proof DAG into an [LLProof.t] directly. Returns the proof
    and footer key/value pairs. Raises [Zipperposition_mdag.Decode.Fail] on
    malformed data. *)

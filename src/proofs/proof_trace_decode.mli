(* This file is free software, part of Zipperposition. See file "license" for more details. *)

(** {1 Proof Trace Decoder} *)

open Logtk

type t
(** Decoder state, holds the binary data and caches *)

val create : string -> t
(** Create decoder from binary file contents (as a string) *)

val decode_proof : t -> Proof.t * (string * string) list
(** Decode the full proof DAG and return footer key/value pairs. Raises
    Zipperposition_mdag.Decode.Fail on malformed data. *)

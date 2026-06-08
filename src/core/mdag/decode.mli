type t
(** Decoder *)

val create : string -> t

val raw_string : t -> string
(** Return the underlying string *)

type node_decoder
(** Local state to read the data for a node *)

type offset = int

val read_node : t -> offset -> (node_decoder -> string -> 'a) -> 'a
(** Decode node at offset. The callback gets a decoder for the arguments as well
    as the command's name. *)

exception Fail of string * offset

type value =
  | Stop  (** No other value left *)
  | Null
  | Bool of bool
  | Int64 of int64
  | Float of float
  | String of string
  | Blob of string
  | Ref of int

val read : node_decoder -> value
(** [read dec] returns a value.
    @raise Fail in case of malformed data. *)

val read_int : node_decoder -> int
val read_string : node_decoder -> string
val read_blob : node_decoder -> string
val read_ref : node_decoder -> offset

val read_all_refs : node_decoder -> offset list
(** Convenience readers that assert the expected value type and convert.
    @raise Fail on type mismatch. *)

val iter_nodes : t -> (offset -> string -> value list -> unit) -> unit
(** Stream forward from offset 0, calling [f offset cmd args] for each node.
    [args] does not include the trailing [Stop] marker.
    @raise Fail if the stream is malformed. *)

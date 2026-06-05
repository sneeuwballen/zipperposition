type t
(** Encoder *)

class type output = object
  method write : bytes -> int -> int -> unit
end

val create : out:#output -> unit -> t

val flush : t -> unit
(** Write all data to the internal output *)

type node_encoder
(** Local state to write the data for a node *)

type offset = private int

val write_node : t -> string -> (node_encoder -> unit) -> offset
(** [write_node enc command f] starts a new node with "command", calls [f] to
    add the arguments, and returns the offset of this new node. The node encoder
    must not escape. *)

val null : node_encoder -> unit
val bool : node_encoder -> bool -> unit
val int64 : node_encoder -> int64 -> unit
val int : node_encoder -> int -> unit
val float : node_encoder -> float -> unit
val string : node_encoder -> string -> unit
val blob : node_encoder -> string -> unit
val ref : node_encoder -> offset -> unit

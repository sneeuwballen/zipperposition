
(* This file is free software, part of Libzipperposition. See file "license" for more details. *)

(** {1 Unique Identifiers}

    An {!ID.t} is a unique identifier (an integer) with a human-readable name.
    We use those to give names to variables that are not hashconsed (the hashconsing
    does not play nice with names)

    @since NEXT_RELEASE *)

type t = private {
  id: int;
  name: string;
  mutable payload: exn list; (** Use [exn] as an open type for user-defined payload *)
}

val make : string -> t
(** Makes a fresh ID *)

val copy : t -> t
(** Copy with a new ID *)

val id : t -> int
val name : t -> string
val payload : t -> exn list

val add_payload : t -> exn -> unit

include Interfaces.HASH with type t := t
include Interfaces.ORD with type t := t
include Interfaces.PRINT with type t := t

(** NOTE: default printer does not display the {!id} field *)

val pp_full : t CCFormat.printer
(** Prints the ID with its internal number *)

val pp_fullc : t CCFormat.printer
(** Prints the ID with its internal number colored in gray (better for
    readability). Only use for debugging. *)

val gensym : unit -> t
(** Generate a new ID with a new, unique name *)

module Map : CCMap.S with type key = t
module Set : CCSet.S with type elt = t
module Tbl : CCHashtbl.S with type key = t



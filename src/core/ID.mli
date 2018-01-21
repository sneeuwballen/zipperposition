
(* This file is free software, part of Logtk. See file "license" for more details. *)

(** {1 Unique Identifiers} *)

(** An {!ID.t} is a unique identifier (an integer) with a human-readable name.
    We use those to give names to variables that are not hashconsed (the hashconsing
    does not play nice with names).

    An identifier is primarily determined by its [id] (a unique number for
    this identifier), and contains a string name for readability.
    Sometimes we display identifiers as "name/id".

    Identifiers are {b generative}: you can easily create new ones
    or copy them.

    Identifiers can carry some {b payload} (values, of type {!exn} because
    it's extensible). It is useful to remember easily some
    information about the identifier (e.g. special sugar notation,
    whether it's a skolem, etc.)

    @since 1.5
*)

type t = private {
  id: int;
  name: string;
  mutable payload: exn list; (** Use [exn] as an open type for user-defined payload *)
}

val make : string -> t
(** Makes a fresh ID *)

val makef : ('a, Format.formatter, unit, t) format4 -> 'a

val copy : t -> t
(** Copy with a new ID *)

val id : t -> int
val name : t -> string
val payload : t -> exn list

val payload_find: f:(exn -> 'a option) -> t -> 'a option

val payload_pred: f:(exn -> bool) -> t -> bool

val set_payload : ?can_erase:(exn -> bool) -> t -> exn -> unit
(** Set given exception as payload.
    @param can_erase if provided, checks whether an existing value
      is to be replaced instead of adding a new entry *)

include Interfaces.HASH with type t := t
include Interfaces.ORD with type t := t
include Interfaces.PRINT with type t := t

(** NOTE: default printer does not display the {!id} field *)

val pp_full : t CCFormat.printer
(** Prints the ID with its internal number *)

val pp_fullc : t CCFormat.printer
(** Prints the ID with its internal number colored in gray (better for
    readability). Only use for debugging. *)

val pp_tstp : t CCFormat.printer
val pp_zf : t CCFormat.printer

val gensym : unit -> t
(** Generate a new ID with a new, unique name *)

module Map : CCMap.S with type key = t
module Set : CCSet.S with type elt = t
module Tbl : CCHashtbl.S with type key = t


exception Attr_infix of string
(** Infix name for pretty-printing *)

exception Attr_prefix of string
(** Prefix name for pretty-printing *)

exception Attr_parameter of int
(** Parameter, used for HO unif *)

type skolem_kind = K_normal | K_ind (* inductive *)

exception Attr_skolem of skolem_kind * int

exception Attr_distinct

val as_infix : t -> string option
val is_infix : t -> bool

val as_prefix : t -> string option
val is_prefix : t -> bool

val as_parameter : t -> int option
val is_parameter : t -> bool

val is_skolem : t -> bool
(** [is_skolem id] returns [true] iff [id] is a Skolem symbol *)

val as_skolem : t -> skolem_kind option

val num_mandatory_args : t -> int
(** number of mandatory arguments of a skolem constant or 0 otherwise *)

val is_distinct_object : t -> bool
(** whether the identifier is a distinct object (as defined in TPTP syntax) *)

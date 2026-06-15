(* This file is free software, part of Zipperposition. See file "license" for more details. *)

(** {1 Stream} *)

(** Streams are potentially infinite lists of clauses *)

open Logtk

val stat_stream_create : Util.stat

type t = private {
  id: int;
  parents: Clause.t list;
  mutable penalty: int;
  mutable hits: int;
  mutable stm: Clause.t option OSeq.t;
}

exception Empty_Stream
exception Drip_n_Unfinished of Clause.t option list * int * int

val make : ?penalty:int -> parents:Clause.t list -> Clause.t option OSeq.t -> t
val equal : t -> t -> bool
val compare : t -> t -> int
val hash : t -> int
val id : t -> int
val is_empty : t -> bool
val penalty : t -> int
val drip : t -> Clause.t option
val drip_n : t -> int -> int -> Clause.t option list
val pp : t CCFormat.printer

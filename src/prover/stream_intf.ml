
(* This file is free software, part of Zipperposition. See file "license" for more details. *)

module type S = sig
  module Ctx : Ctx.S
  module C : Clause.S with module Ctx = Ctx

type t = private {
  id : int; (** unique ID of the stream *)
  penalty: int; (** heuristic penalty *)
  mutable stm : C.t option OSeq.t; (** the stream itself *)
}

(** {2 Basics} *)

val make : ?penalty:int -> C.t option OSeq.t -> t

val equal : t -> t -> bool
val compare : t -> t -> int
val hash : t -> int
val id : t -> int

val is_empty : t -> bool

val length : t -> int

val penalty : t -> int

(** {2 IO} *)

val pp : t CCFormat.printer

end


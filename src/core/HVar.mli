
(* This file is free software, part of Logtk. See file "license" for more details. *)

(** {1 Hashconsed Variable}

  A variable for hashconsed terms. It can be either a "free" variable with
  a name (an integer, here, for efficiency), or a "bound" variable
  using De Bruijn indices.
*)

type t = private int

val make : int -> t
val id : t -> int

include Interfaces.HASH with type t := t
include Interfaces.ORD with type t := t
include Interfaces.PRINT with type t := t

module Set : CCSet.S with type elt = t
module Map : CCMap.S with type key = t


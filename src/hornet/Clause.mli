
(* This file is free software, part of Zipperposition. See file "license" for more details. *)

(** {1 General representation of Clauses} *)

open Libzipperposition

type t

val make : Lit.t IArray.t -> t

include Interfaces.EQ with type t := t
include Interfaces.HASH with type t := t
include Interfaces.PRINT with type t := t



(* This file is free software, part of Zipperposition. See file "license" for more details. *)

(** {1 Proof Result} *)

(** The various results of a proof *)

type t = Hornet_types.proof_res

include Interfaces.HASH with type t := t
include Interfaces.PRINT with type t := t

module Tbl : CCHashtbl.S with type key = t


(* This file is free software, part of Zipperposition. See file "license" for more details. *)

(** {1 Rewriting}

    Deal with definitions as rewrite rules *)
open Libzipperposition

module Make(E : Env_intf.S) : sig
  val setup : ?ctx_narrow:bool -> has_rw:bool -> unit -> unit
end

val extension : Extensions.t

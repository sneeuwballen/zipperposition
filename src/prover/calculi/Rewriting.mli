
(* This file is free software, part of Zipperposition. See file "license" for more details. *)

(** {1 Rewriting}

    Deal with definitions as rewrite rules *)

open Logtk

module Make(E : Env_intf.S) : sig
  val setup : has_rw:bool -> Rewrite_lit.Set.t -> unit
end

module Key : sig
  val rules : Rewrite_lit.Set.t Flex_state.key
end

val extension : Extensions.t

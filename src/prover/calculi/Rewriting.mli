
(* This file is free software, part of Zipperposition. See file "license" for more details. *)

(** {1 Rewriting}

    Deal with definitions as rewrite rules *)

open Logtk

module Make(E : Env_intf.S) : sig
  val setup : Rewrite_rule.Set.t -> unit
end

module Key : sig
  val rules : Rewrite_rule.Set.t Flex_state.key
end

val extension : Extensions.t

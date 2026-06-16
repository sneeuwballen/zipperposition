(* This file is free software, part of Zipperposition. See file "license" for more details. *)

(** {1 AC redundancy} *)

open Logtk

type spec = AC_intf.spec

module type S = AC_intf.S

module Make (E : Env.S) : S with module E = E

val key_ac : (module S) Flex_state.key
val extension : Extensions.t

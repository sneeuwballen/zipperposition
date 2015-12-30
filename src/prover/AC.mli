
(* This file is free software, part of Zipperposition. See file "license" for more details. *)

(** {1 AC redundancy} *)

type spec = AC_intf.spec

module type S = AC_intf.S

module Make(Env: Env.S) : S with module Env = Env

val extension : Extensions.t


(* This file is free software, part of Zipperposition. See file "license" for more details. *)

(** {1 Induction through Cut} *)

open Libzipperposition

module type S = Induction_intf.S

module Make
    (E: Env.S)
    (A : Avatar_intf.S with module E = E)
  : S
    with module Env = E

val extension : Extensions.t

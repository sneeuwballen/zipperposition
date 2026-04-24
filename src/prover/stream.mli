(* This file is free software, part of Zipperposition. See file "license" for more details. *)

(** {1 Stream} *)

(** Streams are potentially infinite lists of clauses *)

open Logtk

val stat_stream_create : Util.stat

module type S = Stream_intf.S

module type ARG = sig
  module Ctx : Ctx.S
  module C : Clause.S with module Ctx = Ctx
end

(** {2 Build a new Stream} *)
module Make (A : ARG) : S with module Ctx = A.Ctx and module C = A.C


(* This file is free software, part of Zipperposition. See file "license" for more details. *)

(** {1 Stream} *)

(** Streams are potentially infinite lists of clauses *)

open Logtk

val stat_stream_create : Util.stat

module type S = Stream_intf.S

(** {2 Build a new Stream} *)
module Make(X : sig
    module Ctx : Ctx.S
  end) : sig
  include S with module Ctx = X.Ctx
end

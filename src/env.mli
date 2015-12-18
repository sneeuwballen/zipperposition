
(* This file is free software, part of Zipperposition. See file "license" for more details. *)

(** {1 Global environment for an instance of the prover} *)

open Logtk

(** {2 Signature} *)
module type S = Env_intf.S

(** {2 Build a new Environment} *)
module Make(X : sig
    module Ctx : Ctx.S
    val params : Params.t
  end) : S with module Ctx = X.Ctx

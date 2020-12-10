
(* This file is free software, part of Zipperposition. See file "license" for more details. *)

(** {1 Global environment for an instance of the prover} *)

open Logtk

(** {2 Signature} *)
module type S = Env_intf.S

type 'a packed = (module S with type C.t = 'a)

(** {2 Build a new Environment} *)
module Make(X : sig
    module Ctx : Ctx.S
    val params : Params.t
    val flex_state : Flex_state.t
  end) : sig
  include S with module Ctx = X.Ctx
end

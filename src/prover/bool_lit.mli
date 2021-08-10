
(* This file is free software, part of Zipperposition. See file "license" for more details. *)

(** {1 Boolean Literal}

    The boolean literal carries a payload of type ['a] *)

module type S = Bool_lit_intf.S

module type PAYLOAD = sig
  type t
  val dummy : t
end

module Make(Payload : PAYLOAD) : S with type payload = Payload.t

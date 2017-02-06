
(* This file is free software, part of Zipperposition. See file "license" for more details. *)

module type S = Bool_lit_intf.S

module type PROOF = sig
  type t
end

module Make(Proof:PROOF)(X : sig end) : S with type proof = Proof.t

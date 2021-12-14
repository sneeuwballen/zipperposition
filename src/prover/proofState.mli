
(* This file is free software, part of Zipperposition. See file "license" for more details. *)

(** {1 The state of a proof, contains a set of active clauses (processed),
    a set of passive clauses (to be processed), and an ordering
    that is used for redundancy elimination.} *)

module type S = ProofState_intf.S

(** {2 Create a Proof State} *)

module Make(C : Clause.S) : S with module C = C and module Ctx = C.Ctx

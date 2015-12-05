
(* This file is free software, part of Logtk. See file "license" for more details. *)

(** {1 Unification and Matching} *)

type subst = Substs.t
type 'a sequence = ('a -> unit) -> unit

exception Fail
(** Raised when a unification/matching attempt fails *)

(** {2 Signatures} *)

module type UNARY = Unif_intf.UNARY
module type NARY = Unif_intf.NARY

(** {2 Base (scoped terms)} *)

module Nary : NARY with type term = InnerTerm.t

module Unary : UNARY with type term = InnerTerm.t
(** To be used only on terms without {!InnerTerm.Multiset} constructor *)

(** {2 Specializations} *)

module Ty : UNARY with type term = Type.t
module FO : UNARY with type term = FOTerm.t
module HO : NARY with type term = HOTerm.t

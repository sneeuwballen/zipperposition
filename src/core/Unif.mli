
(* This file is free software, part of Libzipperposition. See file "license" for more details. *)

(** {1 Unification and Matching} *)

type subst = Subst.t
type 'a sequence = ('a -> unit) -> unit

exception Fail
(** Raised when a unification/matching attempt fails *)

val occurs_check : depth:int -> subst ->
  InnerTerm.t HVar.t Scoped.t -> InnerTerm.t Scoped.t -> bool

(** Generic unification over two arrays (of the same size) *)
val unif_array_com :
  subst ->
  op:(subst -> 'a Scoped.t -> 'a Scoped.t -> subst Sequence.t) ->
  'a array Scoped.t ->
  'a array Scoped.t ->
  subst Sequence.t

(** {2 Signatures} *)

module type S = Unif_intf.S

(** {2 Base (scoped terms)} *)

module Inner : S with type term = InnerTerm.t and type ty = InnerTerm.t
(** To be used only on terms without {!InnerTerm.Multiset} constructor *)

(** {2 Specializations} *)

module Ty : S with type term = Type.t and type ty = Type.t
module FO : S with type term = FOTerm.t and type ty = Type.t

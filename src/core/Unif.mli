
(* This file is free software, part of Logtk. See file "license" for more details. *)

(** {1 Unification and Matching} *)

type subst = Subst.t
type 'a sequence = ('a -> unit) -> unit

exception Fail
(** Raised when a unification/matching attempt fails *)

val occurs_check : depth:int -> subst ->
  InnerTerm.t HVar.t Scoped.t -> InnerTerm.t Scoped.t -> bool

(** Generic unification over two arrays (of the same size, or the first
    one must be smaller or equal) *)
val unif_array_com :
  ?size:[`Same | `Smaller] ->
  'subst ->
  op:('subst -> 'a Scoped.t -> 'a Scoped.t -> 'subst Sequence.t) ->
  'a array Scoped.t ->
  'a array Scoped.t ->
  'subst Sequence.t

(** Generic unification over two lists (of the same size) *)
val unif_list :
  'subst ->
  op:('subst -> 'a Scoped.t -> 'a Scoped.t -> 'subst Sequence.t) ->
  'a list Scoped.t ->
  'a list Scoped.t ->
  'subst Sequence.t

(** Generic unification over two lists (of the same size or smaller) *)
val unif_list_com :
  ?size:[`Same | `Smaller] ->
  'subst ->
  op:('subst -> 'a Scoped.t -> 'a Scoped.t -> 'subst Sequence.t) ->
  'a list Scoped.t ->
  'a list Scoped.t ->
  'subst Sequence.t

(** {2 Signatures} *)

module type S = Unif_intf.S

(** {2 Base (scoped terms)} *)

module Inner : S with type term = InnerTerm.t and type ty = InnerTerm.t
(** To be used only on terms without {!InnerTerm.Multiset} constructor *)

(** {2 Specializations} *)

module Ty : S with type term = Type.t and type ty = Type.t
module FO : sig
  include S with type term = Term.t and type ty = Type.t

  val anti_unify : term -> term -> (term * term) list option
  (** anti-unification of the two terms, returning disagreement pairs *)
end

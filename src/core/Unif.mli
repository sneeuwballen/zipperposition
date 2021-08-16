
(** {1 Unification and Matching} *)

(* This file is free software, part of Logtk. See file "license" for more details. *)


type unif_subst = Unif_subst.t
type subst = Subst.t
type term = InnerTerm.t
type ty = InnerTerm.t
type 'a sequence = ('a -> unit) -> unit

exception Fail
(** Raised when a unification/matching attempt fails *)

val _allow_pattern_unif : bool ref
val _unif_bool : bool ref

val norm_logical_disagreements : 
  ?mode:[< `Conservative | `Off | `Pragmatic > `Conservative ] ->
  Builtin.t -> Term.t list -> Term.t list -> Term.t list * Term.t list

val occurs_check : depth:int -> subst ->
  InnerTerm.t HVar.t Scoped.t -> InnerTerm.t Scoped.t -> bool

(** Generic unification over two arrays (of the same size, or the first
    one must be smaller or equal) *)
val unif_array_com :
  ?size:[`Same | `Smaller] ->
  'subst ->
  op:('subst -> 'a Scoped.t -> 'a Scoped.t -> 'subst Iter.t) ->
  'a array Scoped.t ->
  'a array Scoped.t ->
  'subst Iter.t

(** Generic unification over two lists (of the same size) *)
val unif_list :
  'subst ->
  op:('subst -> 'a Scoped.t -> 'a Scoped.t -> 'subst Iter.t) ->
  'a list Scoped.t ->
  'a list Scoped.t ->
  'subst Iter.t

(** Generic unification over two lists (of the same size or smaller) *)
val unif_list_com :
  ?size:[`Same | `Smaller] ->
  'subst ->
  op:('subst -> 'a Scoped.t -> 'a Scoped.t -> 'subst Iter.t) ->
  'a list Scoped.t ->
  'a list Scoped.t ->
  'subst Iter.t

val pair_lists_right : term -> term list -> term -> term list -> term list * term list
(** in HO, we have [f1 l1] and [f2 l2], where application is left-associative.
    we need to unify from the right (the outermost application is on
    the right) so this returns pairs to unify (including heads). *)

val pair_lists_left : term list -> term -> term list -> term -> term list * term list
(** in HO, we have [l1 -> ret1] and [l2 -> ret2], where [->] is right-associative.
    we need to unify from the left,
    so this returns pairs to unify (including return types). *)

(** {2 Signatures} *)

module type S = Unif_intf.S

(** {2 Base (scoped terms)} *)

module Inner : S with type term = InnerTerm.t and type ty = InnerTerm.t
(** To be used only on terms without {!InnerTerm.Multiset} constructor *)

(** {2 Specializations} *)

module Ty : sig
  include S with type term = Type.t and type ty = Type.t

  val type_is_unifiable : term -> bool
  (** Can we (syntactically) unify terms of this type? *)
end

module FO : sig
  include S with type term = Term.t and type ty = Type.t

  val bind_or_update : ?check:bool -> subst -> ty HVar.t Scoped.t -> term Scoped.t -> subst

  val anti_unify : ?cut:int -> term -> term -> (term * term) list option
  (** anti-unification of the two terms, returning disagreement pairs
      @param cut if [cut=n], then the returned list will have length
      at most [n] (if it's too long then [None] is returned) *)

  val pair_lists : term -> term list -> term -> term list -> term list * term list
end

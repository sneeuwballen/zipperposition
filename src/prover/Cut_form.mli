
(* This file is free software, part of Zipperposition. See file "license" for more details. *)

(** {1 Universally Quantified Conjunction of Clauses} *)

open Logtk

type var = Term.var
type term = Term.t
type clause = Literals.t
type form = clause list

(** A formula of the form [forall vars. \bigand_i C_i].
    The [C_i] are clauses with free variables in [vars] *)
type t = private {
  vars: Term.VarSet.t;
  cs: form;
}
type cut_form = t

val make : Literals.t list -> t
val trivial : t

include Interfaces.HASH with type t := t
include Interfaces.ORD with type t := t
include Interfaces.PRINT with type t := t
val pp_tstp : t CCFormat.printer
val pp_zf : t CCFormat.printer

val vars : t -> Term.VarSet.t
val cs : t -> Literals.t list

val ind_vars : t -> var list
(** subset of {!vars} that have an inductive type *)

val subst1 : var -> term -> t -> t
(** Substitution of one variable *)

val apply_subst : Subst.Renaming.t -> Subst.t -> t Scoped.t -> t

val are_variant : t -> t -> bool
(** Are these two cut formulas alpha-equivalent? *)

val normalize : t -> t
(** Use rewriting to normalize the formula *)

val to_s_form : t -> TypedSTerm.Form.t
(** Convert to input formula *)

module Pos : sig
  val at : t -> Position.t -> term
  (** Return the subterm at the given position, or
      @raise Invalid_argument if the position is not valid *)

  val lit_at : t -> Position.t -> Literal.t * Position.t
  (** Lookup which literal the position is about, return it
      and the rest of the position.
      @raise Invalid_argument if the position is not valid *)

  val clause_at : t -> Position.t -> clause * Position.t
  (** Lookup which clause the position is about, return it
      and the rest of the position.
      @raise Invalid_argument if the position is not valid *)

  val replace : t -> at:Position.t -> by:term -> t
  (** In-place modification of the array, in which the subterm at given
      position is replaced by the [by] term.
      @raise Invalid_argument if the position is not valid *)

  val replace_many : t -> term Position.Map.t -> t
  (** In-place modification of the array, in which the subterm at given
      position is replaced by the [by] term.
      @raise Invalid_argument if the position is not valid *)
end

module Seq : sig
  val terms : t -> term Sequence.t
  val terms_with_pos : ?subterms:bool -> t -> term Position.With.t Sequence.t
end

(** {2 Structure for Sets of cut forms, indexed modulo α-eq} *)
module FV_tbl(X : Map.OrderedType) : sig
  type value = X.t
  type t

  val create : unit -> t

  val add : t -> cut_form -> value -> unit

  val mem : t -> cut_form -> bool

  val get : t -> cut_form -> value option

  val to_seq : t -> (cut_form * X.t) Sequence.t
end


(* This file is free software, part of Zipperposition. See file "license" for more details. *)

(** {1 Arithmetic Rational Literal} *)

(** Atomic formulas for linear rational arithmetic.
    Similar to {!Int_lit}. *)

type term = Term.t

(** {2 Type Decls} *)

type op =
  | Equal
  | Less

(** Arithmetic literal (on rationals) *)
type t = private {
  op: op;
  left: Q.t Monome.t;
  right: Q.t Monome.t;
}

type lit = t

(** {2 Basics} *)

val equal_com : t -> t -> bool
val compare : t -> t -> int

include Interfaces.HASH with type t := t

val make : op -> Q.t Monome.t -> Q.t Monome.t -> t

val mk_eq : Q.t Monome.t -> Q.t Monome.t -> t
val mk_less : Q.t Monome.t -> Q.t Monome.t -> t

val is_eq : t -> bool
val is_less : t -> bool

val to_term : t -> term
(** Back to terms (for negation) *)

val pp : t CCFormat.printer
val pp_tstp : t CCFormat.printer
val pp_zf : t CCFormat.printer
val to_string : t -> string

(** {2 Operators} *)

val fold : ('a -> term -> 'a) -> 'a -> t -> 'a

val map : (term -> term) -> t -> t (** functor *)

type ('subst,'a) unif =
  subst:'subst -> 'a Scoped.t -> 'a Scoped.t -> 'subst Sequence.t

val generic_unif: ('subst, Q.t Monome.t) unif -> ('subst,t) unif
(** Generic unification/matching/variant, given such an operation on monomes *)

val apply_subst : Subst.Renaming.t -> Subst.t -> t Scoped.t -> t

val apply_subst_no_simp : Subst.Renaming.t -> Subst.t -> t Scoped.t -> t
(** Same as {!apply_subst} but takes care {B NOT} simplifying the
    literal. In practice, mostly useful for comparison purpose. *)

val matching : ?subst:Subst.t -> t Scoped.t -> t Scoped.t ->
  Subst.t Sequence.t
(** checks whether subst(lit_a) is equal to lit_b. Returns alternative
    substitutions s such that s(lit_a) = lit_b and s contains subst. *)

val variant : ?subst:Subst.t -> t Scoped.t -> t Scoped.t ->
  Subst.t Sequence.t

val unify : ?subst:Unif_subst.t -> t Scoped.t -> t Scoped.t -> Unif_subst.t Sequence.t

val subsumes : ?subst:Subst.t -> t Scoped.t -> t Scoped.t -> Subst.t Sequence.t
(** Find substitutions such that [subst(lit_a)] implies [lit_b]. This is
    more general than matching. *)

val are_variant : t -> t -> bool

val is_trivial : t -> bool
val is_absurd : t -> bool

val fold_terms : ?pos:Position.t -> ?vars:bool -> ?ty_args:bool ->
  which:[<`Max|`All] ->
  ord:Ordering.t -> subterms:bool ->
  t -> (term * Position.t) Sequence.t

val max_terms : ord:Ordering.t -> t -> term list
(** Maximal terms of the literal *)

val to_form : t -> Term.t SLiteral.t
(** Conversion into a simple literal *)

(** {2 Iterators} *)

module Seq : sig
  val terms : t -> term Sequence.t
  val vars : t -> Term.var Sequence.t
  val to_multiset : t -> (term * Q.t) Sequence.t
end

(** {2 Focus on a Term} *)

module Focus : sig
  (** focus on a term in one of the two monomes *)
  type t =
    | Left of op * Q.t Monome.Focus.t * Q.t Monome.t
    | Right of op * Q.t Monome.t * Q.t Monome.Focus.t

  val mk_left : op -> Q.t Monome.Focus.t -> Q.t Monome.t -> t
  val mk_right : op -> Q.t Monome.t -> Q.t Monome.Focus.t -> t

  val get : lit -> Position.t -> t option

  val get_exn : lit -> Position.t -> t
  (** @raise Invalid_argument if the position doesn't fit *)

  val focus_term : lit -> term -> t option
  (** Attempt to focus on the given atomic term, if it occurs directly
      under the arith literal *)

  val focus_term_exn : lit -> term -> t

  val replace : t -> Q.t Monome.t -> lit
  (** [replace a m] replaces [mf.coeff × mf.term] with [m] where [mf] is the
      focused monome in [a], and return the resulting literal *)

  val term : t -> term
  (** Focused term *)

  val focused_monome : t -> Q.t Monome.Focus.t
  (** The focused monome *)

  val opposite_monome : t -> Q.t Monome.t
  (** The opposite monome (in [Left] and [Right]) *)

  val is_max : ord:Ordering.t -> t -> bool
  (** Is the focused term maximal in the literal? *)

  val is_strictly_max : ord:Ordering.t -> t -> bool
  (** Is the focused term maximal in the literal, ie is it greater
      than all the othe terms? *)

  val map_lit :
    f_m:(Q.t Monome.t -> Q.t Monome.t) ->
    f_mf:(Q.t Monome.Focus.t -> Q.t Monome.Focus.t) ->
    t -> t
  (** Apply a function to the monomes and focused monomes *)

  val product : t -> Q.t -> t
  (** Product by a constant *)

  val scale : t -> t -> t * t
  (** Multiply the two literals by some constant so that their focused
      literals have the same coefficient *)

  val apply_subst : Subst.Renaming.t -> Subst.t -> t Scoped.t -> t
  (** Apply a substitution *)

  val unify : ?subst:Unif_subst.t -> t Scoped.t -> t Scoped.t ->
    (t * t * Unif_subst.t) Sequence.t
  (** Unify the two focused terms, and possibly other terms of their
      respective focused monomes; yield the new literals accounting for
      the unification along with the unifier *)

  val fold_terms :
    ?pos:Position.t -> lit -> (t * Position.t) Sequence.t
  (** Fold on focused terms in the literal, one by one, with
      the position of the focused term *)

  val op : t -> op

  val unfocus : t -> lit
  (** Conversion back to a literal *)

  val pp : t CCFormat.printer
  val to_string : t -> string
end

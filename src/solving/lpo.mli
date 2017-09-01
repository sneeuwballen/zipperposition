
(* This file is free software, part of Zipperposition. See file "license" for more details. *)

(** {1 Constraint Solving for LPO} *)

open Logtk

(** {6 Constraints} *)

module Constraint : sig
  type expr = ID.t

  type t =
    | EQ of expr * expr
    | LE of expr * expr
    | LT of expr * expr
    | And of t list
    | Or of t list
    | Not of t
    | True   (* tautology *)
    | False  (* impossible constraint *)

  val eq : expr -> expr -> t
  val neq : expr -> expr -> t
  val le : expr -> expr -> t
  val lt : expr -> expr -> t
  val gt : expr -> expr -> t
  val ge : expr -> expr -> t
  val and_ : t list -> t
  val or_ : t list -> t
  val not_ : t -> t
  val imply : t -> t -> t
  val true_ : t
  val false_ : t

  module Seq : sig
    val exprs : t -> expr Sequence.t
    (** Expressions that occur in the constraint *)
  end

  include Interfaces.PRINT with type t := t

  val simplify : t -> t
  (** Basic simplifications *)
end

(** {2 Solutions to constraint problems} *)

module Solution : sig
  type t = (ID.t * ID.t) list
  (** A precedence on symbol. Each pair means that thG
      first symbol is bigger than the second one. *)

  val neg_to_constraint : t -> Constraint.t
  (** Constraint that explicitely eliminate this solution *)

  include Interfaces.PRINT with type t := t
end

val solve_multiple : Constraint.t list -> Solution.t LazyList.t
(** A lazy list of partial orders over symbols, that satisfy the given
    list of constraints *)

(** {6 Search for a LPO ordering} *)

module FO : sig
  type term = Term.t

  val orient_lpo : term -> term -> Constraint.t
  (** [orient a b] generates a constraint that is sufficient for [a]
      to be bigger than [b] in LPO orderings satisfying the
      constraints *)

  val orient_lpo_list : (term * term) list -> Constraint.t list
  (** Orient a list of pairs *)
end

module TypedSTerm : sig
  type term = TypedSTerm.t

  val orient_lpo : term -> term -> Constraint.t
  (** [orient a b] generates a constraint that is sufficient for [a]
      to be bigger than [b] in LPO orderings satisfying the
      constraints *)

  val orient_lpo_list : (term * term) list -> Constraint.t list
  (** Orient a list of pairs *)
end

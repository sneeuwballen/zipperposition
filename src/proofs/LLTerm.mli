
(* This file is free software, part of Zipperposition. See file "license" for more details. *)

(** {1 Terms For Proofs} *)

open Logtk

type t

type var = t HVar.t

module Int_op : sig
  type t = Leq0 | Geq0 | Lt0 | Gt0 | Eq0 | Neq0 | Divisible_by of Z.t | Not_div_by of Z.t
  val not : t -> t
  val equal : t -> t -> bool
  val hash : t -> int
  val pp : t CCFormat.printer
end

module Rat_op : sig
  type t = Leq0 | Geq0 | Lt0 | Gt0 | Eq0 | Neq0
  val not : t -> t
  val equal : t -> t -> bool
  val hash : t -> int
  val pp : t CCFormat.printer
end

type view =
  | Type
  | Const of ID.t
  | App of t * t (** curried application *)
  | Arrow of t * t (** functional arrow *)
  | Var of var (** bound var *)
  | Bind of {
      binder: Binder.t;
      ty_var: t;
      body: t;
    }
  | AppBuiltin of Builtin.t * t list
  | Ite of t * t * t
  | Int_pred of Z.t linexp * Int_op.t
  | Rat_pred of Q.t linexp * Rat_op.t

and 'a linexp (** linear expression with coeffs of type 'a *)

type term = t
type ty = t

(** linear expressions *)
module type LINEXP = sig
  type num
  type t = num linexp
  val zero : t
  val is_zero : t -> bool
  val is_const : t -> bool
  val ( + ) : t -> t -> t
  val ( - ) : t -> t -> t
  val ( * ) : num -> t -> t
  val add : num -> term -> t -> t
  val const : num -> t
  val monomial : num -> term -> t
  val monomial1 : term -> t
  val equal : t -> t -> bool
  val map : (term -> term) -> t -> t
  val subterms : t -> term Sequence.t
  val pp : term CCFormat.printer -> t CCFormat.printer
end

module Linexp_int : LINEXP with type num = Z.t
module Linexp_rat : LINEXP with type num = Q.t

val view : t -> view
val ty : t -> ty option
val ty_exn : t -> ty
val equal : t -> t -> bool
val hash : t -> int
val compare : t -> t -> int

val is_type : t -> bool

val t_type : ty
val var : var -> t
val const : ty:ty -> ID.t -> t
val app : t -> t -> t
val app_l : t -> t list -> t
val arrow : t -> t -> t
val arrow_l : t list -> t -> t
val bind : ty:ty -> Binder.t -> ty_var:ty -> t -> t
val app_builtin : ty:ty -> Builtin.t -> t list -> t
val builtin : ty:ty -> Builtin.t -> t
val ite : t -> t -> t -> t
val int_pred : Linexp_int.t -> Int_op.t -> t
val rat_pred : Linexp_rat.t -> Rat_op.t -> t

val bool : ty
val box_opaque : t -> t
val lambda : ty_var:ty -> t -> t

val db_eval : sub:t -> t -> t
(** [db_eval ~sub t] replaces De Bruijn 0 in [t] by [sub] *)

val pp : t CCFormat.printer
val pp_inner : t CCFormat.printer

module Form : sig
  type t = term
  type view = private
    | True
    | False
    | Or of t list
    | And of t list
    | Not of t
    | Equiv of t * t
    | Xor of t * t
    | Imply of t * t
    | Atom of t
    | Eq of t * t
    | Neq of t * t
    | Int_pred of Z.t linexp * Int_op.t
    | Rat_pred of Q.t linexp * Rat_op.t
    | Forall of {ty_var: ty; body: t}
    | Exists of {ty_var: ty; body: t}

  val view : t -> view
  val pp : t CCFormat.printer

  val true_ : t
  val false_ : t
  val eq : t -> t -> t
  val neq : t -> t -> t
  val not_ : t -> t
  val and_ : t list -> t
  val or_ : t list -> t
  val imply : t -> t -> t
  val equiv : t -> t -> t
  val xor : t -> t -> t
  val int_pred : Linexp_int.t -> Int_op.t -> t
  val rat_pred : Linexp_rat.t -> Rat_op.t -> t
  val forall : ty_var:ty -> t -> t
  val exists : ty_var:ty -> t -> t
end

module Set : CCSet.S with type elt = t

module Conv : sig
  type ctx

  val create : unit -> ctx

  val of_term : ctx -> TypedSTerm.t -> t
end

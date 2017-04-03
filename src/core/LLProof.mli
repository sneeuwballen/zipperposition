
(* This file is free software, part of Zipperposition. See file "license" for more details. *)

(** {1 Low Level Proofs} *)

type term = TypedSTerm.t
type ty = term
type form = TypedSTerm.Form.t
type subst = (term, ty) Var.Subst.t

type name = string

type t = {
  id: int; (* unique ID *)
  concl: form;
  step: step;
}
and step =
  | Goal
  | Assert
  | Negated_goal of t
  | Instantiate of subst * t
  | Esa of name * t list
  | Inference of name * t list
  | No_check of name * t list (* NOTE: avoid if possible *)

val premises : t -> t list

val pp_step : step CCFormat.printer

val pp : t CCFormat.printer
(** Print only this step *)

val pp_dag : t CCFormat.printer
(** Print the whole DAG *)

val equal : t -> t -> bool
val compare : t -> t -> int
val hash : t -> int

val goal : form -> t
val negated_goal : form -> t -> t
val assert_ : form -> t
val instantiate : form -> subst -> t -> t
val esa : form -> name -> t list -> t
val inference : form -> name -> t list -> t
val no_check : form -> name -> t list -> t

module Tbl : CCHashtbl.S with type key = t

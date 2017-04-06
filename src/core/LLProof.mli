
(* This file is free software, part of Zipperposition. See file "license" for more details. *)

(** {1 Low Level Proofs} *)

type term = TypedSTerm.t
type ty = term
type form = TypedSTerm.Form.t
type subst = (term, ty) Var.Subst.t

type name = string

type check_info =
  | C_check of form list (* additional inputs *)
  | C_no_check
  | C_other

type t

type step =
  | Goal
  | Assert
  | Negated_goal of t
  | Trivial
  | By_def of ID.t
  | Instantiate of subst * t
  | Esa of name * t list * check_info
  | Inference of name * t list * check_info

val id : t -> int
val concl : t -> form
val step : t -> step
val premises : t -> t list

val check_info : t -> check_info

val pp_step : step CCFormat.printer

val pp_id : t CCFormat.printer
val pp_res : t CCFormat.printer

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
val trivial : form -> t
val by_def : ID.t -> form -> t
val instantiate : form -> subst -> t -> t
val esa : [`No_check | `Check | `Check_with of form list] -> form -> name -> t list -> t
val inference : [`No_check | `Check | `Check_with of form list] -> form -> name -> t list -> t

module Tbl : CCHashtbl.S with type key = t

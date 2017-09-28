
(* This file is free software, part of Zipperposition. See file "license" for more details. *)

(** {1 Low Level Proofs} *)

(** Low level proofs, intended for mechanical proof checking.

    Instantiations (substitutions) are explicit because that should make
    the job of the checker easier.

    NOTE: this is still uncooked, and will probably change.
*)

val section : Util.Section.t

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
  | Define of ID.t
  | Instantiate of subst * t
  | Esa of name * t list * check_info
  | Inference of name * parent list * check_info

and parent =
  | P_of of t
  | P_instantiate of t * subst

val id : t -> int
val concl : t -> form
val step : t -> step
val parents : t -> parent list
val premises : t -> t list

val p_of : t -> parent
val p_instantiate : t -> subst -> parent

val check_info : t -> check_info

val pp_step : step CCFormat.printer
val pp_parent : parent CCFormat.printer

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
val define : ID.t -> form -> t
val instantiate : form -> subst -> t -> t
val esa :
  [`No_check | `Check | `Check_with of form list] ->
  form -> name -> t list -> t
val inference :
  [`No_check | `Check | `Check_with of form list] ->
  form -> name -> parent list -> t

module Tbl : CCHashtbl.S with type key = t

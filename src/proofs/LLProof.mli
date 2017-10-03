
(* This file is free software, part of Zipperposition. See file "license" for more details. *)

(** {1 Low Level Proofs} *)

(** Low level proofs, intended for mechanical proof checking.

    Instantiations (substitutions) are explicit because that should make
    the job of the checker easier.

    NOTE: this is still uncooked, and will probably change.
*)

open Logtk

val section : Util.Section.t

type term = TypedSTerm.t
type ty = term
type form = term
type var = ty Var.t
type inst = term list (** Instantiate some binder with the following terms. Order matters. *)
type tag = Proof.tag

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
  | Instantiate of t * inst
  | Esa of name * t list * check_info
  | Inference of {
      intros: term list; (* local renaming, with fresh constants *)
      name: name;
      parents: parent list;
      check: check_info;
      tags: tag list;
    }

and parent = {
  p_proof: t;
  p_inst: inst; (* instantiate [forall] variables *)
}

val id : t -> int
val concl : t -> form
val step : t -> step
val parents : t -> parent list
val premises : t -> t list

val p_of : t -> parent
val p_inst : t -> inst -> parent

val check_info : t -> check_info

val pp_step : step CCFormat.printer
val pp_parent : parent CCFormat.printer

val pp_id : t CCFormat.printer
val pp_res : t CCFormat.printer

val pp : t CCFormat.printer
(** Print only this step *)

val pp_dag : t CCFormat.printer
(** Print the whole DAG *)

val pp_inst : inst CCFormat.printer

val equal : t -> t -> bool
val compare : t -> t -> int
val hash : t -> int

val goal : form -> t
val negated_goal : form -> t -> t
val assert_ : form -> t
val trivial : form -> t
val by_def : ID.t -> form -> t
val define : ID.t -> form -> t
val instantiate : form -> t -> inst -> t
val esa :
  [`No_check | `Check | `Check_with of form list] ->
  form -> name -> t list -> t
val inference :
  [`No_check | `Check | `Check_with of form list] ->
  intros:term list ->
  tags:tag list ->
  form -> name -> parent list -> t

module Tbl : CCHashtbl.S with type key = t


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
type inst = term list (** Instantiate some binder with the following terms. Order matters. *)
type tag = Proof.tag

type name = string

type t

type step =
  | Goal
  | Assert
  | Negated_goal of t
  | Trivial
  | By_def of ID.t
  | Define of ID.t
  | Instantiate of {
      form: t;
      inst: inst;
      tags: tag list;
    }
  | Esa of name * t list
  | Inference of {
      intros: term list; (* local renaming for the conclusion's foralls, with fresh constants *)
      local_intros: term list; (* variables introduced between hypothesis, not in conclusion *)
      name: name;
      parents: parent list;
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
val instantiate : ?tags:tag list -> form -> t -> inst -> t
val esa :
  form -> name -> t list -> t
val inference :
  intros:term list ->
  local_intros:term list ->
  tags:tag list ->
  form -> name -> parent list -> t


(** {2 Checking steps} *)

type check_res =
  | R_ok
  | R_fail
  | R_skip

val get_check_res : t -> check_res option

val set_check_res : t -> check_res -> unit

val pp_check_res : check_res CCFormat.printer

(** {2 Printing} *)

module Tbl : CCHashtbl.S with type key = t

module Dot : sig
  val pp_dot : name:string -> t CCFormat.printer
  (** Pretty print the proof as a DOT graph *)

  val pp_dot_file : ?name:string -> string -> t -> unit
  (** print to dot into a file *)

  val pp_dot_seq : name:string -> t Sequence.t CCFormat.printer
  (** Print a set of proofs as a DOT graph, sharing common subproofs *)

  val pp_dot_seq_file : ?name:string -> string -> t Sequence.t -> unit
  (** same as {!pp_dot_seq} but into a file *)
end

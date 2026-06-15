(* This file is free software, part of Zipperposition. See file "license" for more details. *)

(** {1 Low Level Proofs — Clause-Based} *)

(** Low level proofs using clauses ([Literal.t array]) directly. Free variables
    in clauses are implicitly universal. Instantiations carry
    [(HVar.t * Term.t) list] substitution pairs. *)

open Logtk

val section : Util.Section.t

type clause = Literal.t array
(** A clause: disjunction of literals. Free variables are implicitly universal.
*)

type inst = (Type.t HVar.t * Term.t) list
(** Instantiation: pairs of (variable, replacement term). Identity pairs (v → v)
    should be omitted. *)

type tag = Proof.tag
type name = string
type t

type step =
  | Goal
  | Assert
  | Trivial
  | By_def of Name.t
  | Define of Name.t
  | Esa of name * t list
  | Inference of {
      name: name;
      tags: tag list;
      parents: parent list;
    }

and parent = {
  p_proof: t;
  p_inst: inst;
}

val id : t -> int
val concl : t -> clause
val step : t -> step
val parents : t -> parent list
val premises : t -> t list
val p_of : t -> parent
val p_inst : t -> inst -> parent
val pp_step : step CCFormat.printer
val pp_parent : parent CCFormat.printer
val pp_id : t CCFormat.printer
val pp_res : t CCFormat.printer
val pp_clause : clause CCFormat.printer
val pp : t CCFormat.printer
val pp_dag : t CCFormat.printer
val pp_inst : inst CCFormat.printer
val equal : t -> t -> bool
val compare : t -> t -> int
val hash : t -> int
val goal : clause -> t
val assert_ : clause -> t
val trivial : clause -> t
val by_def : Name.t -> clause -> t
val define : Name.t -> clause -> t
val esa : clause -> name -> t list -> t
val inference : tags:tag list -> clause -> name -> parent list -> t

val mk_ : clause -> step -> t
(** Low-level constructor for custom step types (e.g. Esa) *)

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
  val pp_dot_file : ?name:string -> string -> t -> unit
  val pp_dot_seq : name:string -> t Iter.t CCFormat.printer
  val pp_dot_seq_file : ?name:string -> string -> t Iter.t -> unit
end

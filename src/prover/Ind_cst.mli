
(* This file is free software, part of Zipperposition. See file "license" for more details. *)

(** {1 Inductive Constants and Cases}

    Skolem constants of an inductive type, coversets, etc. required for
    inductive reasoning. *)

open Logtk

type t
(** A ground term of an inductive type. It must correspond to a
    term built with the corresponding {!t} only.
    For instance, a constant of type [nat] should be equal to
    [s^n(0)] in any model. *)

exception InvalidDecl of string

exception NotAnInductiveConstant of ID.t

val id_as_cst : ID.t -> t option

val id_as_cst_exn : ID.t -> t
(** Unsafe version of {!as_cst}
    @raise NotAnInductiveConstant if it fails *)

val id_is_cst : ID.t -> bool
(** Check whether the given constant is an inductive constant *)

val on_new_cst : t Signal.t
(** Triggered with new inductive constants *)

val make_skolem : Type.t -> ID.t

val make : ?depth:int -> is_sub:bool -> Type.t -> t
(** Make a new constant of the given type *)

val is_sub : t -> bool
(** Is the constant a sub-constant (i.e. a subterm of a case in a coverset)? *)

val id_is_sub : ID.t -> bool

val equal : t -> t -> bool
val compare : t -> t -> int
val hash : t -> int

val id : t -> ID.t
val to_term : t -> Term.t
val ty : t -> Type.t

val same_type : t -> t -> bool
(** Do these two inductive constants have the same type? *)

val pp : t CCFormat.printer

val depth : t -> int

val dominates : t -> t -> bool
(** [dominates c1 c2] if [depth c1 < depth c2]. This way, in coversets,
    the top constant dominates all sub-constants *)

module Cst_set : CCSet.S with type elt = t
(** Set of constants *)

(** {2 Inductive Skolems} *)

type ind_skolem = ID.t * Type.t

val ind_skolem_compare : ind_skolem -> ind_skolem -> int
val ind_skolem_equal : ind_skolem -> ind_skolem -> bool

val id_is_ind_skolem : ID.t -> Type.t -> bool
(** [id_is_potential_cst id ty] returns [true] if [id:ty] is
    a skolem constant of an inductive type, or
    if it is already an inductive constant. *)

val find_ind_skolems : Term.t -> ind_skolem Sequence.t
(** [find_ind_skolem term] searches subterms of [term] for constants
    that are of an inductive type and that are skolems or
    (already) inductive constants. *)

val ind_skolem_depth : ID.t -> int
(** depth of the skolem (0 if not an inductive constant) *)

(**/**)

exception Payload_cst of t
(**/**)

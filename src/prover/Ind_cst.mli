
(* This file is free software, part of Zipperposition. See file "license" for more details. *)

(** {1 Inductive Constants and Cases}

    Skolem constants of an inductive type, coversets, etc. required for
    inductive reasoning. *)

open Logtk

exception InvalidDecl of string

type t

(** {6 Inductive Constants}

    A ground term of an inductive type. It must correspond to a
    term built with the corresponding {!t} only.
    For instance, a constant of type [nat] should be equal to
    [s^n(0)] in any model. *)

exception Payload_cst of t

val id_as_cst : ID.t -> t option

val id_as_cst_exn : ID.t -> t
(** Unsafe version of {!as_cst}
    @raise NotAnInductiveConstant if it fails *)

val id_is_cst : ID.t -> bool
(** Check whether the given constant is an inductive constant *)

val on_new_cst : t Signal.t
(** Triggered with new inductive constants *)

val make_skolem : Type.t -> ID.t

val make : ?depth:int -> Type.t -> t
(** Make a new constant of the given type *)

(* TODO: remove this, right? or rename to [make] *)
val declare : ?depth:int -> ID.t -> ty:Type.t -> t
(** Adds the constant to the set of inductive constants, make a coverset...
    @raise AlreadyDeclaredConstant if the constant is declared already
    @raise NotAnInductiveType if [ty] is not an inductive type
    @param depth depth of constant
*)

(* TODO: remove, we only need [make] to build new constants *)
val of_term : FOTerm.t -> t option
(** [cst_of_term t] returns a new or existing constant for this term, if any.
    @return None if the term is not to be converted into a constant
    @raise InvalidDecl if the term is not ground nor of an inductive type *)

(* TODO: remove, we only need [make] to build new constants *)
val of_id : ID.t -> Type.t -> t
(** [cst_of_id id ty] returns a new or existing constant for this id.
    @raise InvalidDecl if the type is not an inductive type *)

val equal : t -> t -> bool
val compare : t -> t -> int
val hash : t -> int

val id : t -> ID.t
val to_term : t -> FOTerm.t
val ty : t -> Type.t

val same_type : t -> t -> bool
(** Do these two inductive constants have the same type? *)

val pp : t CCFormat.printer

val depth : t -> int

val id_is_potential_cst : ID.t -> Type.t -> bool
(** [id_is_potential_cst id ty] returns [true] if [id:ty] is already an
    inductive constant, or if it can be turned into one *)

(* TODO: change this, should just return skolems with inductive type *)
val find_in_term : FOTerm.t -> t Sequence.t
(** [find_in_lits term] searches subterms of [term] for constants
    that are of an inductive type and that are not constructors.
    It returns the sequence of such inductive constants. *)

val dominates : t -> t -> bool
(** [dominates c1 c2] if [depth c1 < depth c2]. This way, in coversets,
    the top constant dominates all sub-constants *)

module Cst_set : CCSet.S with type elt = t
(** Set of constants *)

(**/**)
val max_depth_: int ref
(**/**)

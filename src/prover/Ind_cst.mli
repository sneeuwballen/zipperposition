
(* This file is free software, part of Zipperposition. See file "license" for more details. *)

(** {1 Inductive Constants and Cases}

    Skolem constants of an inductive type, coversets, etc. required for
    inductive reasoning. *)

open Libzipperposition

exception InvalidDecl of string

(** {6 Inductive Case}

    An inductive case is a term that belongs to the coverset of some
    inductive constant. The inductive constant must fall into one
    of the cases in its coverset.

    Every case starts with a constructor of its type. *)

type case = private {
  case_term : FOTerm.t;
  case_kind: [`Base | `Rec];
  case_sub: ID.Set.t; (* set of sub-constants *)
}

type cover_set

val case_equal : case -> case -> bool
val case_compare : case -> case -> int
val case_hash : case -> int

val pp_case : case CCFormat.printer

val case_is_rec : case -> bool
val case_is_base : case -> bool

(** {6 Inductive Constants}

    A ground term of an inductive type. It must correspond to a
    term built with the corresponding {!t} only.
    For instance, a constant of type [nat] should be equal to
    [s^n(0)] in any model. *)

type cst = private {
  cst_id: ID.t;
  cst_ty: Type.t;
  cst_ity: Ind_ty.t; (* the corresponding inductive type *)
  cst_coverset: cover_set; (* the coverset for this constant *)
}

exception Payload_cst of cst

val as_cst : ID.t -> cst option

val as_cst_exn : ID.t -> cst
(** Unsafe version of {!as_cst}
    @raise NotAnInductiveConstant if it fails *)

val is_cst : ID.t -> bool
(** Check whether the given constant is an inductive skolem *)

val on_new_cst : cst Signal.t
(** Triggered with new inductive constants *)

val declare_cst : ?cover_set_depth:int -> ID.t -> ty:Type.t -> cst
(** Adds the constant to the set of inductive constants, make a coverset...
    @param cover_set_depth depth of cover_set terms; the deeper, the
      larger the cover set will be
    @raise NotAnInductiveType if [ty] is not an inductive type *)

val declarations_of_cst : cst -> (ID.t * Type.t) Sequence.t
(** [declarations_of_cst c] returns a list of type declarations that should
    be made if [c] is new (declare the subcases of its coverset) *)

val cst_equal : cst -> cst -> bool
val cst_compare : cst -> cst -> int
val cst_hash : cst -> int

val cst_to_term : cst -> FOTerm.t

val pp_cst : cst CCFormat.printer

val cover_set : cst -> cover_set
(** Get the cover set of this constant:

    a set of ground terms [[t1,...,tn]] with fresh
    constants inside (that are not declared as inductive!) such that
    [bigor_{i in 1...n} t=ti] is the skolemized version of the
    exhaustivity axiom on [t]'s type. *)

val cases : ?which:[`Rec|`Base|`All] -> cover_set -> case Sequence.t
(** Cases of the cover set *)

val find_cst_in_term : FOTerm.t -> (ID.t * Ind_ty.t * Type.t) Sequence.t
(** [find_cst_in_lits term] searches subterms of [term] for constants
    that are of an inductive type, that are not constructors nor already
    declared, and that are Skolem symbols or sub-constants *)

(** {6 Sub-Constants} *)

(** A subterm of some {!case} that has the same (inductive) type *)
type sub_cst = private {
  sub_cst_id: ID.t;
  sub_cst_ty: Type.t;
  sub_cst_case: case;
  sub_cst_cst: cst;
}

exception Payload_sub_cst of sub_cst

val case_sub : case -> sub_cst Sequence.t

val as_sub_cst : ID.t -> sub_cst option

val as_sub_cst_exn : ID.t -> sub_cst
(** @raise NotAnInductiveSubConstant in case of failure *)

val is_sub_cst : ID.t -> bool
(** Is the term a constant that was created within a cover set? *)

val is_sub_cst_of : ID.t -> cst -> bool

val as_sub_cst_of : ID.t -> cst -> sub_cst option
(** downcasts iff [t] is a sub-constant of [cst] *)

val inductive_cst_of_sub_cst : sub_cst -> cst * case
(** [inductive_cst_of_sub_cst t] finds a pair [c, t'] such
    that [c] is an inductive const, [t'] belongs to a coverset
    of [c], and [t] is a sub-constant within [t'].
    @raise Not_found if [t] isn't an inductive constant *)

val sub_constants : cover_set -> sub_cst Sequence.t
(** All sub-constants of a given inductive constant *)

val sub_constants_case : case -> sub_cst Sequence.t
(** All sub-constants that are subterms of a specific case *)

val term_of_sub_cst: sub_cst -> FOTerm.t

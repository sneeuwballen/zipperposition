
(* This file is free software, part of Zipperposition. See file "license" for more details. *)

(** {1 Inductive Constants and Cases}

    Skolem constants of an inductive type, coversets, etc. required for
    inductive reasoning. *)

open Logtk

exception InvalidDecl of string

type cst

type case

type path_cell = private {
  path_cst: cst;
  path_case: case;
  path_clauses: ClauseContext.t list;
}

type path = path_cell list

type cover_set = case list

(** {6 Inductive Case}

    An inductive case is a term that belongs to the coverset of some
    inductive constant. The inductive constant must fall into one
    of the cases in its coverset.

    Every case starts with a constructor of its type. *)

val case_equal : case -> case -> bool
val case_compare : case -> case -> int
val case_hash : case -> int

val pp_case : case CCFormat.printer

val case_to_term : case -> FOTerm.t

val case_is_rec : case -> bool
val case_is_base : case -> bool

val case_sub_constants : case -> cst Sequence.t
(** All sub-constants that are subterms of a specific case *)

val cover_set_cases : ?which:[`Rec|`Base|`All] -> cover_set -> case Sequence.t
(** Cases of the cover set *)

(** {6 Inductive Constants}

    A ground term of an inductive type. It must correspond to a
    term built with the corresponding {!t} only.
    For instance, a constant of type [nat] should be equal to
    [s^n(0)] in any model. *)

exception Payload_cst of cst

val as_cst : ID.t -> cst option

val as_cst_exn : ID.t -> cst
(** Unsafe version of {!as_cst}
    @raise NotAnInductiveConstant if it fails *)

val is_cst : ID.t -> bool
(** Check whether the given constant is an inductive constant *)

val on_new_cst : cst Signal.t
(** Triggered with new inductive constants *)

val declare_cst :
  ?cover_set_depth:int ->
  ID.t ->
  ty:Type.t ->
  cst
(** Adds the constant to the set of inductive constants, make a coverset...
    @param cover_set_depth depth of cover_set terms; the deeper, the
      larger the cover set will be (default 1)
    @raise AlreadyDeclaredConstant if the constant is declared already
    @raise NotAnInductiveType if [ty] is not an inductive type *)

val cst_of_term : FOTerm.t -> cst option
(** [cst_of_term t] returns a new or existing constant for this term, if any.
    @return None if the term is not to be converted into a constant
    @raise InvalidDecl if the term is not ground nor of an inductive type *)

val cst_of_id : ID.t -> Type.t -> cst
(** [cst_of_id id ty] returns a new or existing constant for this id.
    @raise InvalidDecl if the type is not an inductive type *)

val declarations_of_cst : cst -> (ID.t * Type.t) Sequence.t
(** [declarations_of_cst c] returns a list of type declarations that should
    be made if [c] is new (declare the subcases of its coverset) *)

val cst_equal : cst -> cst -> bool
val cst_compare : cst -> cst -> int
val cst_hash : cst -> int

val cst_id : cst -> ID.t
val cst_to_term : cst -> FOTerm.t
val cst_ty : cst -> Type.t

val cst_same_type : cst -> cst -> bool

val pp_cst : cst CCFormat.printer

val cst_depth : cst -> int

val cst_cover_set : cst -> cover_set option
(** Get the cover set of this constant:

    a set of ground terms [[t1,...,tn]] with fresh
    constants inside (that are not declared as inductive!) such that
    [bigor_{i in 1...n} t=ti] is the skolemized version of the
    exhaustivity axiom on [t]'s type. *)

val is_potential_cst : ID.t -> Type.t -> bool
(** [is_potential_cst id ty] returns [true] if [id:ty] is already an
    inductive constant, or if it can be turned into one *)

val find_cst_in_term : FOTerm.t -> cst Sequence.t
(** [find_cst_in_lits term] searches subterms of [term] for constants
    that are of an inductive type and that are not constructors.
    It returns the sequence of such inductive constants. *)

val is_sub_cst : ID.t -> bool
(** Is the term a constant that was created within a cover set? *)

val is_sub_cst_of : ID.t -> cst -> bool

val as_sub_cst_of : ID.t -> cst -> cst option
(** downcasts iff [t] is a sub-constant of [cst] *)

val cover_set_sub_constants : cover_set -> cst Sequence.t
(** All sub-constants of a given inductive constant *)

val dominates : cst -> cst -> bool
(** [dominates c sub] is true if [sub] is a sub-constant of [c],
    or if some sub-constant of [c] dominates [sub] transitively *)

(** {6 Path}

    A path is a sequence of nested inductions on distinct constants
    and corresponding {b sets} of clause contexts. *)

val path_equal : path -> path -> bool
val path_compare : path -> path -> int
val path_hash : path -> int

val path_empty : path
val path_cons : cst -> case -> ClauseContext.t list -> path -> path

val path_length : path -> int

val path_dominates : path -> path -> bool
(** [path_dominates a b] is true if [b] is a suffix of [a]. In other words,
    [a] is a path to a subtree of what [b] is a path to. *)

val path_contains_cst : path -> cst -> bool
(** Does the path contain this inductive constant? *)

val pp_path : path CCFormat.printer

val lits_of_path : path -> Literals.t
(** Extract the raw equality literals from this path *)

(**/**)
val max_depth_: int ref
(**/**)

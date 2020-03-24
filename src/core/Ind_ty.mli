
(* This file is free software, part of Zipperposition. See file "license" for more details. *)

(** {1 Inductive Types} *)

(** An inductive datatype, defined by a list of constructors
    (and associated projectors that are defined as partial functions).
    Inductive types can be mutually recursive. *)

val section : Util.Section.t

(** Constructor for an inductive type *)
type constructor = private {
  cstor_name: ID.t;
  cstor_ty: Type.t;
  cstor_args: (Type.t * projector) list;
}

(** A projector for a given constructor and argument position *)
and projector = private {
  p_id: ID.t;
  p_ty: Type.t;
  p_index: int; (* index of projected argument *)
  p_cstor: constructor lazy_t;
}

(** {6 Inductive Types} *)

(** An inductive type, along with its set of constructors *)
type t = private {
  ty_id: ID.t; (* name *)
  ty_vars: Type.t HVar.t list; (* list of variables *)
  ty_pattern: Type.t; (* equal to  [id ty_vars] *)
  ty_constructors : constructor list;
  (* constructors, all returning [pattern] and containing
     no other type variables than [ty_vars] *)
  ty_is_rec: bool lazy_t;
  (* true iff the type is (mutually) recursive *)
  ty_proof: Proof.t;
}

val pp : t CCFormat.printer

exception InvalidDecl of string

exception NotAnInductiveType of ID.t

exception NotAnInductiveConstructor of ID.t

val declare_ty : ID.t -> ty_vars:Type.t HVar.t list -> constructor list -> proof:Proof.t -> t
(** Declare the given inductive type.
    @raise InvalidDecl if the type is already declared, or the list
      of constructors is empty *)

val as_inductive_ty : ID.t -> t option

val as_inductive_ty_exn : ID.t -> t
(** Unsafe version of {!as_inductive_ty}
    @raise NotAnInductiveType if the ID is not an inductive type *)

val is_inductive_ty : ID.t -> bool

val as_inductive_type : Type.t -> (t * Type.t list) option
(** [as_inductive_ty (list int)] will return [list, [int]] as an
    inductive type applied to some arguments *)

val as_inductive_type_exn : Type.t -> t * Type.t list

val is_inductive_type : Type.t -> bool
(** [is_inductive_type ty] holds iff [ty] is an instance of some
    registered type (registered with {!declare_ty}). *)

val is_inductive_simple_type : TypedSTerm.t -> bool

val is_recursive : t -> bool

val proof : t -> Proof.t

(** {6 Constructors} *)

val mk_constructor : ID.t -> Type.t -> (Type.t * (ID.t * Type.t)) list -> constructor

val is_constructor : ID.t -> bool
(** true if the symbol is an inductive constructor (zero, successor...) *)

val as_constructor : ID.t -> (constructor * t) option
(** if [id] is a constructor of [ity], then [as_constructor id]
    returns [Some (cstor, ity)] *)

val as_constructor_exn : ID.t -> constructor * t
(** Unsafe version of {!as_constructor}
    @raise NotAnInductiveConstructor if it fails *)

val contains_inductive_types : Term.t -> bool
(** [true] iff the term contains at least one subterm with
    an inductive type *)

(** {6 Projectors} *)

val projector_id: projector -> ID.t
val projector_ty: projector -> Type.t
val projector_idx: projector -> int
val projector_cstor: projector -> constructor

val as_projector : ID.t -> projector option


(**/**)

(** Exceptions used to store information in IDs *)

exception Payload_ind_type of t
exception Payload_ind_cstor of constructor * t
exception Payload_ind_projector of projector

(**/**)

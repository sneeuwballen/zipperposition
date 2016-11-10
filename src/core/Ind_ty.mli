
(* This file is free software, part of Zipperposition. See file "license" for more details. *)

(** {1 Inductive Types} *)

val section : Util.Section.t

(** Constructor for an inductive type *)
type constructor = {
  cstor_name: ID.t;
  cstor_ty: Type.t;
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
}

val pp : t CCFormat.printer

exception InvalidDecl of string

exception NotAnInductiveType of ID.t

exception NotAnInductiveConstructor of ID.t

val declare_ty : ID.t -> ty_vars:Type.t HVar.t list -> constructor list -> t
(** Declare the given inductive type.
    @raise InvalidDecl if the type is already declared, or the list
      of constructors is empty *)

val as_inductive_ty : ID.t -> t option

val as_inductive_ty_exn : ID.t -> t
(** Unsafe version of {!as_inductive_ty}
    @raise NotAnInductiveType if the ID is not an inductive type *)

val is_inductive_ty : ID.t -> bool

val as_inductive_type : Type.t -> t option

val is_inductive_type : Type.t -> bool
(** [is_inductive_type ty] holds iff [ty] is an instance of some
    registered type (registered with {!declare_ty}). *)

(** {6 Constructors} *)

val is_constructor : ID.t -> bool
(** true if the symbol is an inductive constructor (zero, successor...) *)

val as_constructor : ID.t -> (constructor * t) option
(** if [id] is a constructor of [ity], then [as_constructor id]
    returns [Some (cstor, ity)] *)

val as_constructor_exn : ID.t -> constructor * t
(** Unsafe version of {!as_constructor}
    @raise NotAnInductiveConstructor if it fails *)

val contains_inductive_types : FOTerm.t -> bool
(** [true] iff the term contains at least one subterm with
    an inductive type *)

(** {6 Constants with Inductive Type} *)

val is_inductive_constant : ID.t -> bool
(** An ID whose type is inductive; nothing more *)

val declare_inductive_constant : ID.t -> unit
(** Make the ID satisfy {!is_inductive_constant} *)

val scan_for_constant : ID.t -> Type.t -> unit
(** Check whether [id : ty] has an inductive type; if yes, then
    call {!declare_inductive_constant} *)

(** {6 Scan Declarations} *)

val scan_stmt : (_, _, Type.t, _) Statement.t -> unit
(** [scan_stmt stmt] examines [stmt], and, if the statement is a
    declaration of inductive types or constants,
    it declares them using {!declare_ty} or {!declare_inductive_constant}. *)

(**/**)

(** Exceptions used to store information in IDs *)

exception Payload_ind_type of t
exception Payload_ind_cstor of constructor * t
exception Payload_ind_constant
exception Payload_ind_projector of ID.t

(**/**)

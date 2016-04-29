
(* This file is free software, part of Zipperposition. See file "license" for more details. *)

(** {1 Statement}

    The input problem is made of {b statements}. Each statement can declare
    a type, assert a formula, or a conjecture, define a term, etc.

    Those statements do not necessarily reflect exactly statements in the input
    language(s) (e.g., TPTP). *)

(** A datatype declaration *)
type 'ty data = {
  data_id: ID.t;
    (** Name of the type *)
  data_args: 'ty Var.t list;
    (** type parameters *)
  data_ty: 'ty;
    (** type of Id, that is,   [type -> type -> ... -> type] *)
  data_cstors: (ID.t * 'ty) list;
    (** Each constructor is [id, ty]. [ty] must be of the form
     [ty1 -> ty2 -> ... -> id args] *)
}

type attr =
  | A_AC

type attrs = attr list

type ('f, 't, 'ty) view =
  | TyDecl of ID.t * 'ty (** id: ty *)
  | Data of 'ty data list
  | Def of ID.t * 'ty * 't
  | RewriteTerm of ID.t * 'ty * 't list * 't (* args, rhs *)
  | RewriteForm of 't SLiteral.t * 'f list (* lhs atomic form, rhs conjunction *)
  | Assert of 'f (** assert form *)
  | Goal of 'f (** goal to prove *)
  | NegatedGoal of 'f list (** goal after negation *)

type ('f, 't, 'ty, 'meta) t = {
  view: ('f, 't, 'ty) view;
  attrs: attrs;
  src: 'meta; (** additional data *)
}

type ('f, 't, 'ty) sourced_t = ('f, 't, 'ty, StatementSrc.t) t

type clause = FOTerm.t SLiteral.t list
type clause_t = (clause, FOTerm.t, Type.t) sourced_t

val view : ('f, 't, 'ty, _) t -> ('f, 't, 'ty) view
val attrs : (_, _, _, _) t -> attrs
val src : (_, _, _, 'src) t -> 'src

val mk_data : ID.t -> args:'ty Var.t list -> 'ty -> (ID.t * 'ty) list -> 'ty data

val ty_decl : ?attrs:attrs -> src:'src -> ID.t -> 'ty -> (_, _, 'ty, 'src) t
val def : ?attrs:attrs -> src:'src -> ID.t -> 'ty -> 't -> (_, 't, 'ty, 'src) t
val rewrite_term : ?attrs:attrs -> src:'src -> ID.t -> 'ty -> 't list -> 't -> (_, 't, 'ty, 'src) t
val rewrite_form : ?attrs:attrs -> src:'src -> 't SLiteral.t -> 'f list -> ('f, 't, _, 'src) t
val data : ?attrs:attrs -> src:'src -> 'ty data list -> (_, _, 'ty, 'src) t
val assert_ : ?attrs:attrs -> src:'src -> 'f -> ('f, _, _, 'src) t
val goal : ?attrs:attrs -> src:'src -> 'f -> ('f, _, _, 'src) t
val neg_goal : ?attrs:attrs -> src:'src -> 'f list -> ('f, _, _, 'src) t

val signature : ?init:Signature.t -> (_, _, Type.t, _) t Sequence.t -> Signature.t
(** Compute signature when the types are using {!Type} *)

val add_src :
  file:string ->
  ('f, 't, 'ty, UntypedAST.attrs) t ->
  ('f, 't, 'ty, StatementSrc.t) t

val map_data : ty:('ty1 -> 'ty2) -> 'ty1 data -> 'ty2 data

val map :
  form:('f1 -> 'f2) ->
  term:('t1 -> 't2) ->
  ty:('ty1 -> 'ty2) ->
  ('f1, 't1, 'ty1, 'src) t ->
  ('f2, 't2, 'ty2, 'src) t

val map_src : f:('a -> 'b) -> ('f, 't, 'ty, 'a) t -> ('f, 't, 'ty, 'b) t

(** {2 Defined Constants} *)

val as_defined_cst: ID.t -> int option
(** [as_defined_cst id] returns [Some level] if [id] is a constant
    defined at stratification level [level], [None] otherwise *)

val is_defined_cst: ID.t -> bool

val declare_defined_cst : ID.t -> int -> unit

val scan_stmt_for_defined_cst : (_, FOTerm.t, _, _) t -> unit
(** Try and declare defined constants in the given statement *)

(**/**)
exception Payload_defined_cst of int
(** Annotation on IDs that are defined (int: stratification level) *)

(**/**)

(** {2 Iterators} *)

module Seq : sig
  val ty_decls : (_, _, 'ty, _) t -> (ID.t * 'ty) Sequence.t
  val forms : ('f, _, _, _) t -> 'f Sequence.t
  val lits : (clause, _, _, _) t -> FOTerm.t SLiteral.t Sequence.t
  val terms : (clause, _, _, _) t -> FOTerm.t Sequence.t
  val symbols : (clause, FOTerm.t, Type.t, _) t -> ID.t Sequence.t
end

(** {2 IO} *)

val pp :
  'a CCFormat.printer ->
  'b CCFormat.printer ->
  'c CCFormat.printer ->
  ('a,'b,'c,_) t CCFormat.printer
val to_string :
  'a CCFormat.printer ->
  'b CCFormat.printer ->
  'c CCFormat.printer ->
  ('a,'b,'c,_) t ->
  string

val pp_clause : clause_t CCFormat.printer

module TPTP : sig
  include Interfaces.PRINT3 with type ('a, 'b, 'c) t := ('a, 'b, 'c) sourced_t
end




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
  data_cstors: (ID.t * 'ty * ('ty * (ID.t * 'ty)) list) list;
  (** Each constructor is [id, ty, args].
      [ty] must be of the form [ty1 -> ty2 -> ... -> id args].
      [args] has the form [(ty1, p1), (ty2,p2), …] where each [p]
      is a projector. *)
}

type attr =
  | A_AC
  | A_infix of string
  | A_prefix of string
  | A_sos (** set of support *)

type attrs = attr list

type 'ty skolem = ID.t * 'ty

type ('t, 'ty) term_rule = 'ty Var.t list * ID.t * 'ty * 't list * 't
(** [forall vars, id args = rhs] *)

(** polarity for rewrite rules *)
type polarity = [`Equiv | `Imply]

type ('f, 't, 'ty) form_rule = 'ty Var.t list * 't SLiteral.t * 'f list * polarity
(** [forall vars, lhs op bigand rhs] where [op] depends on
    [polarity] (in [{=>, <=>, <=}]) *)

type ('f, 't, 'ty) def_rule =
  | Def_term of ('t, 'ty) term_rule
  | Def_form of ('f, 't, 'ty) form_rule

type ('f, 't, 'ty) def = {
  def_id: ID.t;
  def_ty: 'ty; (* def_ty = def_vars -> def_ty_ret *)
  def_rules: ('f, 't, 'ty) def_rule list;
  def_rewrite: bool; (* rewrite rule or mere assertion? *)
}

type ('f, 't, 'ty) view =
  | TyDecl of ID.t * 'ty (** id: ty *)
  | Data of 'ty data list
  | Def of ('f, 't, 'ty) def list
  | Rewrite of ('f,'t,'ty) def_rule
  | Assert of 'f (** assert form *)
  | Lemma of 'f list (** lemma to prove and use, using Avatar cut *)
  | Goal of 'f (** goal to prove *)
  | NegatedGoal of 'ty skolem list * 'f list (** goal after negation, with skolems *)

(* a statement in a file *)
type from_file = {
  file : string;
  name : string option;
  loc: ParseLocation.t option;
}

type lit = Term.t SLiteral.t
type formula = TypedSTerm.t
type input_def = (TypedSTerm.t,TypedSTerm.t,TypedSTerm.t) def
type clause = lit list

type role =
  | R_assert
  | R_goal
  | R_def
  | R_decl

type ('f, 't, 'ty) t = {
  id: int;
  view: ('f, 't, 'ty) view;
  attrs: attrs;
  src: source;
}

and source = private {
  src_id: int;
  src_view: source_view;
}
and source_view =
  | Input of UntypedAST.attrs * role
  | From_file of from_file * role
  | Internal of role
  | Neg of sourced_t
  | CNF of sourced_t
  | Renaming of sourced_t * ID.t * formula (* renamed this formula *)
  | Define of ID.t
  | Preprocess of sourced_t * sourced_t list * string (* stmt, definitions, info *)

and result =
  | Sourced_input of TypedSTerm.t
  | Sourced_clause of clause
  | Sourced_statement of input_t
  | Sourced_clause_stmt of clause_t

and sourced_t = result * source

and input_t = (TypedSTerm.t, TypedSTerm.t, TypedSTerm.t) t
and clause_t = (clause, Term.t, Type.t) t

val compare : (_, _, _) t -> (_, _, _) t -> int

val view : ('f, 't, 'ty) t -> ('f, 't, 'ty) view
val attrs : (_, _, _) t -> attrs
val src : (_, _, _) t -> source

val mk_data : ID.t -> args:'ty Var.t list -> 'ty ->
  (ID.t * 'ty * ('ty * (ID.t * 'ty)) list) list -> 'ty data
val mk_def : ?rewrite:bool -> ID.t -> 'ty -> ('f,'t,'ty) def_rule list -> ('f,'t,'ty) def

val ty_decl : ?attrs:attrs -> src:source -> ID.t -> 'ty -> (_, _, 'ty) t
val def : ?attrs:attrs -> src:source -> ('f,'t,'ty) def list -> ('f, 't, 'ty) t
val rewrite : ?attrs:attrs -> src:source -> ('f,'t,'ty) def_rule -> ('f,'t,'ty) t
val rewrite_term : ?attrs:attrs -> src:source -> ('t, 'ty) term_rule -> (_, 't, 'ty) t
val rewrite_form : ?attrs:attrs -> src:source -> ('f, 't, 'ty) form_rule -> ('f, 't, 'ty) t
val data : ?attrs:attrs -> src:source -> 'ty data list -> (_, _, 'ty) t
val assert_ : ?attrs:attrs -> src:source -> 'f -> ('f, _, _) t
val lemma : ?attrs:attrs -> src:source -> 'f list -> ('f, _, _) t
val goal : ?attrs:attrs -> src:source -> 'f -> ('f, _, _) t
val neg_goal :
  ?attrs:attrs -> src:source -> skolems:'ty skolem list -> 'f list -> ('f, _, 'ty) t

val signature : ?init:Signature.t -> (_, _, Type.t) t Sequence.t -> Signature.t
(** Compute signature when the types are using {!Type} *)

val conv_attrs : UntypedAST.attrs -> attrs
val attr_to_ua : attr -> UntypedAST.attr

val add_src : file:string -> ('f, 't, 'ty) t -> ('f, 't, 'ty) t

val as_sourced : input_t -> sourced_t
(** Just wrap the statement with its source *)

val as_sourced_clause : clause_t -> sourced_t

val map_data : ty:('ty1 -> 'ty2) -> 'ty1 data -> 'ty2 data

val map_def :
  form:('f1 -> 'f2) ->
  term:('t1 -> 't2) ->
  ty:('ty1 -> 'ty2) ->
  ('f1, 't1, 'ty1) def ->
  ('f2, 't2, 'ty2) def

val map :
  form:('f1 -> 'f2) ->
  term:('t1 -> 't2) ->
  ty:('ty1 -> 'ty2) ->
  ('f1, 't1, 'ty1) t ->
  ('f2, 't2, 'ty2) t

(** {2 Defined Constants} *)

type definition = Rewrite.rule_set

val as_defined_cst: ID.t -> (int * definition) option
(** [as_defined_cst id] returns [Some level] if [id] is a constant
    defined at stratification level [level], [None] otherwise *)

val as_defined_cst_level : ID.t -> int option

val is_defined_cst: ID.t -> bool

val declare_defined_cst : ID.t -> level:int -> definition -> unit
(** [declare_defined_cst id ~level] states that [id] is a defined
    constant of given [level]. It means that it is defined based only
    on constants of strictly lower levels *)

val scan_stmt_for_defined_cst : (clause, Term.t, Type.t) t -> unit
(** Try and declare defined constants in the given statement *)

(** {2 Inductive Types} *)

val scan_stmt_for_ind_ty : (_, _, Type.t) t -> unit
(** [scan_stmt_for_ind_ty stmt] examines [stmt], and, if the statement is a
    declaration of inductive types or constants,
    it declares them using {!declare_ty} or {!declare_inductive_constant}. *)

val scan_simple_stmt_for_ind_ty : (_, _, TypedSTerm.t) t -> unit
(** Same as {!scan_stmt} but on earlier statements *)

(** {2 Sourced Statements} *)

(** {2 Statement Source}

    Where a statement originally comes from (file, location, named statement,
    or result of some transformations, etc.) *)
module Src : sig
  type t = source

  val equal : t -> t -> bool
  val hash : t -> int

  val view : t -> source_view

  val file : from_file -> string
  val name : from_file -> string option
  val loc : from_file -> ParseLocation.t option

  val from_input : UntypedAST.attrs -> role -> t

  val from_file : ?loc:ParseLocation.t -> ?name:string -> string -> role -> t
  (** make a new sourced item. Default [is_conjecture] is [false]. *)

  val internal : role -> t

  val neg : sourced_t -> t
  val cnf : sourced_t -> t
  val preprocess : sourced_t -> sourced_t list -> string -> t
  val renaming : sourced_t -> ID.t -> formula -> t
  val define : ID.t -> t

  val neg_input : TypedSTerm.t -> source -> t
  val neg_clause : clause -> source -> t

  val cnf_input : TypedSTerm.t -> source -> t
  val cnf_clause : clause -> source -> t

  val preprocess_input : input_t -> sourced_t list -> string -> t
  val renaming_input : input_t -> ID.t -> formula -> t

  val pp_from_file : from_file CCFormat.printer
  (* include Interfaces.PRINT with type t := t *)

  val pp_role : role CCFormat.printer

  val pp : t CCFormat.printer
  val pp_tstp : t CCFormat.printer
  val to_attr : t -> UntypedAST.attr
end

(** {2 Iterators} *)

module Seq : sig
  val to_seq : ('f,'t,'ty) t ->
    [`Term of 't | `Form of 'f | `Ty of 'ty | `ID of ID.t] Sequence.t
  val ty_decls : (_, _, 'ty) t -> (ID.t * 'ty) Sequence.t
  val forms : ('f, _, _) t -> 'f Sequence.t
  val lits : (clause, _, _) t -> Term.t SLiteral.t Sequence.t
  val terms : (clause, Term.t, _) t -> Term.t Sequence.t
  val symbols : (clause, Term.t, Type.t) t -> ID.t Sequence.t
end

(** {2 IO} *)

val pp_def_rule :
  'a CCFormat.printer ->
  'b CCFormat.printer ->
  'c CCFormat.printer ->
  ('a,'b,'c) def_rule CCFormat.printer

val pp_def :
  'a CCFormat.printer ->
  'b CCFormat.printer ->
  'c CCFormat.printer ->
  ('a,'b,'c) def CCFormat.printer

val pp :
  'a CCFormat.printer ->
  'b CCFormat.printer ->
  'c CCFormat.printer ->
  ('a,'b,'c) t CCFormat.printer

val to_string :
  'a CCFormat.printer ->
  'b CCFormat.printer ->
  'c CCFormat.printer ->
  ('a,'b,'c) t ->
  string

val pp_clause : clause_t CCFormat.printer
val pp_input : input_t CCFormat.printer

module ZF : sig
  include Interfaces.PRINT3 with type ('a, 'b, 'c) t := ('a, 'b, 'c) t
end

module TPTP : sig
  include Interfaces.PRINT3 with type ('a, 'b, 'c) t := ('a, 'b, 'c) t
end



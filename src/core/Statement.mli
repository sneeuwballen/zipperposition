
(* This file is free software, part of Zipperposition. See file "license" for more details. *)

(** {1 Statement} *)

(** The input problem is made of {b statements}. Each statement can declare
    a type, assert a formula, or a conjecture, define a term, add
    a rewrite rule, etc.

    Those statements do not necessarily reflect exactly statements in the input
    language(s) (e.g., TPTP).
*)

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

(** polarity for rewrite rules *)
type polarity = [`Equiv | `Imply]

type ('f, 't, 'ty) def_rule =
  | Def_term of {
      vars: 'ty Var.t list;
      id: ID.t;
      ty: 'ty;
      args: 't list;
      rhs: 't;
      as_form: 'f;
    } (** [forall vars, id args = rhs] *)

  | Def_form of {
      vars: 'ty Var.t list;
      lhs: 't SLiteral.t;
      rhs: 'f list;
      polarity: polarity;
      as_form: 'f list;
    } (** [forall vars, lhs op bigand rhs] where [op] depends on
          [polarity] (in [{=>, <=>, <=}]) *)

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

type lit = Term.t SLiteral.t
type formula = TypedSTerm.t
type input_def = (TypedSTerm.t,TypedSTerm.t,TypedSTerm.t) def
type clause = lit list

type ('f, 't, 'ty) t = private {
  id: int;
  view: ('f, 't, 'ty) view;
  attrs: attrs;
  proof : proof;
  mutable name: string option;
}

and proof = Proof.Step.t
and input_t = (TypedSTerm.t, TypedSTerm.t, TypedSTerm.t) t
and clause_t = (clause, Term.t, Type.t) t

val compare : (_, _, _) t -> (_, _, _) t -> int

val view : ('f, 't, 'ty) t -> ('f, 't, 'ty) view
val attrs : (_, _, _) t -> attrs
val proof_step : (_, _, _) t -> proof

val name : (_,_,_) t -> string
(** Retrieve a name from the proof, or generate+save a new one *)

val as_proof_i : input_t -> Proof.t
val res_tc_i : input_t Proof.result_tc
val as_proof_c : clause_t -> Proof.t
val res_tc_c : clause_t Proof.result_tc

val mk_data : ID.t -> args:'ty Var.t list -> 'ty ->
  (ID.t * 'ty * ('ty * (ID.t * 'ty)) list) list -> 'ty data
val mk_def : ?rewrite:bool -> ID.t -> 'ty -> ('f,'t,'ty) def_rule list -> ('f,'t,'ty) def

val attrs_ua : (_,_,_) t -> UntypedAST.attrs
(** All attributes, included these in the proof *)

val ty_decl : ?attrs:attrs -> proof:proof -> ID.t -> 'ty -> (_, _, 'ty) t
val def : ?attrs:attrs -> proof:proof -> ('f,'t,'ty) def list -> ('f, 't, 'ty) t
val rewrite : ?attrs:attrs -> proof:proof -> ('f,'t,'ty) def_rule -> ('f,'t,'ty) t
val data : ?attrs:attrs -> proof:proof -> 'ty data list -> (_, _, 'ty) t
val assert_ : ?attrs:attrs -> proof:proof -> 'f -> ('f, _, _) t
val lemma : ?attrs:attrs -> proof:proof -> 'f list -> ('f, _, _) t
val goal : ?attrs:attrs -> proof:proof -> 'f -> ('f, _, _) t
val neg_goal :
  ?attrs:attrs -> proof:proof -> skolems:'ty skolem list -> 'f list -> ('f, _, 'ty) t

val signature : ?init:Signature.t -> (_, _, Type.t) t Sequence.t -> Signature.t
(** Compute signature when the types are using {!Type} *)

val conv_attrs : UntypedAST.attrs -> attrs
val attr_to_ua : attr -> UntypedAST.attr

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

val scan_stmt_for_defined_cst : clause_t -> unit
(** Try and declare defined constants in the given statement *)

(** {2 Inductive Types} *)

val scan_stmt_for_ind_ty : clause_t -> unit
(** [scan_stmt_for_ind_ty stmt] examines [stmt], and, if the statement is a
    declaration of inductive types or constants,
    it declares them using {!declare_ty} or {!declare_inductive_constant}. *)

val scan_simple_stmt_for_ind_ty : input_t -> unit
(** Same as {!scan_stmt} but on earlier statements *)

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

val pp_clause_in : Output_format.t -> clause_t CCFormat.printer
val pp_input_in : Output_format.t -> input_t CCFormat.printer



(* This file is free software, part of Zipperposition. See file "license" for more details. *)

(** {1 Manipulate proofs} *)

module Loc = ParseLocation

type term = TypedSTerm.t
type form = TypedSTerm.t
type 'a sequence = ('a -> unit) -> unit

val section : Util.Section.t

type rule

(** Tag for checking an inference. Each tag describes an extension of FO
    that is used in the inference *)
type tag = Builtin.Tag.t

type attrs = UntypedAST.attrs

(** Classification of proof steps *)
type kind =
  | Intro of source * role
  | Inference of rule * tag list
  | Simplification of rule * tag list
  | Esa of rule
  | Trivial (** trivial, or trivial within theories *)
  | Define of ID.t * source (** definition *)
  | By_def of ID.t (** following from the def of ID *)

(** Source of leaves (from some input problem, or internal def) *)
and source = private {
  src_id: int;
  src_view: source_view;
}
and source_view =
  | From_file of from_file * attrs
  | Internal of attrs

(** Intro role *)
and role =
  | R_assert
  | R_goal
  | R_def
  | R_decl
  | R_lemma

(* a statement in a file *)
and from_file = {
  file : string;
  name : string option;
  loc: ParseLocation.t option;
}

(** Typeclass for the result of a proof step *)
type 'a result_tc

(** result of an inference *)
type result = Res : 'a result_tc * exn -> result

(** A proof step, without the conclusion *)
type step

(** Proof Step with its conclusion *)
type proof

type t = proof

type parent =
  | P_of of t
  | P_subst of t * Subst.Projection.t

type info = UntypedAST.attr

type infos = info list

module Tag = Builtin.Tag

(** {2 Rule} *)

(** A rule is a name for some specific inference or transformation rule
    that is used to deduce formulas from other formulas.
*)
module Rule : sig
  type t = rule

  val pp: t CCFormat.printer

  val name : t -> string

  val mk: string -> t

  val mkf : ('a, Format.formatter, unit, t) format4 -> 'a
end

(** {2 Kind} *)

module Kind : sig
  type t = kind

  val pp : t CCFormat.printer
end

(** {2 Source}

    Where a statement/object originally comes from
    (file, location, named statement, etc.)
*)
module Src : sig
  type t = source

  val equal : t -> t -> bool
  val hash : t -> int

  val view : t -> source_view

  val file : from_file -> string
  val name : from_file -> string option
  val loc : from_file -> ParseLocation.t option

  val from_file :
    ?loc:ParseLocation.t ->
    ?name:string ->
    ?attrs:UntypedAST.attrs ->
    string ->
    t

  val internal : attrs -> t

  val pp_from_file : from_file CCFormat.printer
  (* include Interfaces.PRINT with type t := t *)

  val pp_role : role CCFormat.printer

  val pp : t CCFormat.printer
  val pp_tstp : t CCFormat.printer
  val to_attrs : t -> UntypedAST.attrs
end

(** {2 Proof Results} *)

(** A proof is used to deduce some results. We can handle diverse results
    a different stages of the proof (starting with formulas, ending with clauses) *)

module Result : sig
  type t = result

  type 'a tc = 'a result_tc

  type flavor =
    [ `Pure_bool
    | `Absurd_lits
    | `Proof_of_false
    | `Vanilla
    | `Def
    ]

  (** A mapping used during instantiation, to map pre-instantiation
      variables to post-instantiation terms *)
  type inst_subst = (term, term) Var.Subst.t

  val make_tc :
    of_exn:(exn -> 'a option) ->
    to_exn:('a -> exn) ->
    compare:('a -> 'a -> int) ->
    to_form:(ctx:Term.Conv.ctx -> 'a -> form) ->
    ?to_form_subst:(ctx:Term.Conv.ctx -> Subst.Projection.t -> 'a -> form * inst_subst) ->
    pp_in:(Output_format.t -> 'a CCFormat.printer) ->
    ?name:('a -> string) ->
    ?is_stmt:bool ->
    ?flavor:('a -> flavor) ->
    unit ->
    'a tc
  (** Make a result typeclass, for considering values of type ['a] as proof
      results.
      @param pp_in print in given syntax
      @param is_stmt true only if ['a] is a toplevel statement (default false)
      @param name returns the name of the result. Typically, a name from
        the input file
      @param to_form_subst apply substitution, then convert to form.
      If not provided, will fail.
  *)

  val make : 'a tc -> 'a -> t

  val form_tc : form tc

  val of_form : form -> t

  include Interfaces.ORD with type t := t
  include Interfaces.EQ with type t := t
  val pp_in : Output_format.t -> t CCFormat.printer
  val pp : t CCFormat.printer
  val is_stmt : t -> bool
  val to_form : ?ctx:Term.Conv.ctx -> t -> form

  val to_form_subst : ?ctx:Term.Conv.ctx -> Subst.Projection.t -> t -> form * inst_subst
  (** instantiated form + bindings for vars *)

  val flavor : t -> flavor
  val name : t -> string option
end

(** {2 A proof step} *)

(** An inference step is composed of a set of premises, a rule,
    a status (theorem/trivial/equisatisfiable…), and is used to
    deduce new {!result} using these premises and metadata.

    A single step can be used to deduce several results.
*)
module Step : sig
  type t = step

  val kind : t -> kind
  val parents : t -> parent list
  val infos : t -> infos
  val compare : t -> t -> int
  val hash : t -> int
  val equal : t -> t -> bool

  val src : t -> source option

  val trivial : t

  val by_def : ID.t -> t

  val define : ID.t -> source -> parent list -> t
  val define_internal : ID.t -> parent list -> t

  val lemma : source -> t

  val intro : source -> role -> t

  val assert_ : source -> t
  val assert' : ?loc:Loc.t -> file:string -> name:string -> unit -> t

  val goal : source -> t
  val goal' : ?loc:Loc.t -> file:string -> name:string -> unit -> t

  val inference : ?infos:infos -> ?tags:tag list -> rule:rule -> parent list -> t

  val simp : ?infos:infos -> ?tags:tag list -> rule:rule -> parent list -> t

  val esa : ?infos:infos -> rule:rule -> parent list -> t

  val to_attrs : t -> UntypedAST.attrs

  val is_trivial : t -> bool
  val is_by_def : t -> bool

  val is_assert : t -> bool
  (** Proof: the statement was asserted in some file *)

  val is_goal : t -> bool
  (** The statement comes from the negation of a goal in some file *)

  val rule : t -> rule option
  (** Rule name for Esa/Simplification/Inference steps *)

  val distance_to_goal : t -> int option
  (** [distance_to_conjecture p] returns [None] if [p] has no ancestor
      that is a conjecture (including [p] itself). It returns [Some d]
      if [d] is the distance, in the proof graph, to the closest
      conjecture ancestor of [p] *)

  val pp : t CCFormat.printer
end

(** {2 Parent} *)

(** The link between a proof step and some intermediate results used
    to prove its result *)

module Parent : sig
  type t = parent

  val from : proof -> t
  val from_subst_proj : proof -> Subst.Projection.t -> t
  val from_subst : Subst.Renaming.t -> proof Scoped.t -> Subst.t -> t
  val proof : t -> proof
  val subst : t -> Subst.Projection.t option
end

val pp_parent : Parent.t CCFormat.printer

val pp_tag : tag CCFormat.printer
val pp_tags : tag list CCFormat.printer

(** {2 Proof} *)

(** A proof is a pair of a result, with its proof step.
    Typically, a refutation will be a proof of false from axioms and the
    negated goal.
*)

module S : sig
  type t = proof

  val result : t -> result
  val step : t -> step

  val compare: t -> t -> int
  val equal: t -> t -> bool
  val hash: t -> int

  val compare_by_result : t -> t -> int
  (** Compare proofs by their result *)

  module Tbl : CCHashtbl.S with type key = t

  (** {2 Constructors and utils}
      In all the following constructors, [theories] defaults to the empty list.
      Axiom constructors have default role "axiom" *)

  val mk : step -> Result.t -> t
  (** Main constructor *)

  val mk_f : step -> form -> t

  val mk_f_trivial : form -> t
  val mk_f_by_def : ID.t -> form -> t

  val mk_f_inference : rule:rule -> form -> parent list -> t

  val mk_f_simp : rule:rule -> form -> parent list -> t

  val mk_f_esa : rule:rule -> form -> parent list -> t

  val adapt : t -> Result.t -> t

  val adapt_f : t -> form -> t

  val is_proof_of_false : t -> bool

  (** {6 Conversion to a graph of proofs} *)

  val as_graph : (t, rule * Subst.Projection.t option * infos) CCGraph.t
  (** Get a graph of the proof *)

  val traverse :
    ?traversed:unit Tbl.t ->
    order:[`BFS | `DFS] ->
    t ->
    t Sequence.t

  (** {6 IO} *)

  val pp_result_of : t CCFormat.printer
  val pp_notrec : t CCFormat.printer
  (** Non recursive printing on formatter *)

  val pp_notrec1 : t CCFormat.printer
  (** Non recursive printing on formatter, including parents *)

  val pp_tstp : t CCFormat.printer
  val pp_normal : t CCFormat.printer
  val pp_zf : t CCFormat.printer
  val pp_in : Options.print_format -> t CCFormat.printer
  (** Prints the proof according to the given input switch *)

  val pp_dot : name:string -> t CCFormat.printer
  (** Pretty print the proof as a DOT graph *)

  val pp_dot_file : ?name:string -> string -> t -> unit
  (** print to dot into a file *)

  val pp_dot_seq : name:string -> t Sequence.t CCFormat.printer
  (** Print a set of proofs as a DOT graph, sharing common subproofs *)

  val pp_dot_seq_file : ?name:string -> string -> t Sequence.t -> unit
  (** same as {!pp_dot_seq} but into a file *)
end


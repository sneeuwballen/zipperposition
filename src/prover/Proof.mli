
(* This file is free software, part of Zipperposition. See file "license" for more details. *)

(** {1 Manipulate proofs} *)

open Logtk

module Loc = ParseLocation

type form = TypedSTerm.t
type bool_lit = BBox.Lit.t
type 'a sequence = ('a -> unit) -> unit

val section : Util.Section.t

type statement_src = Statement.source

type rule

(** How do we check a step? *)
type check = [`No_check | `Check | `Check_with of form list]

(** Classification of proof steps *)
type kind =
  | Inference of rule * string option * check
  | Simplification of rule * string option * check
  | Esa of rule * string option * check
  | Assert of statement_src
  | Goal of statement_src
  | Lemma
  | Data of statement_src * Type.t Statement.data
  | Trivial (** trivial, or trivial within theories *)
  | By_def of ID.t

type result =
  | Form of form
  | Clause of SClause.t
  | BoolClause of bool_lit list
  | Stmt of Statement.input_t
  | C_stmt of Statement.clause_t

(** A proof step, without the conclusion *)
type step

(** Proof Step with its conclusion *)
type proof

type t = proof

type parent

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

(** {2 Proof Results} *)

(** A proof is used to deduce some results. We can handle diverse results
    a different stages of the proof (starting with formulas, ending with clauses) *)

module Result : sig
  type t = result

  include Interfaces.ORD with type t := t
  include Interfaces.EQ with type t := t
  val pp : t CCFormat.printer
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
  val compare : t -> t -> int
  val hash : t -> int
  val equal : t -> t -> bool


  val trivial : t

  val by_def : ID.t -> t

  val data : statement_src -> Type.t Statement.data -> t

  val assert_ : statement_src -> t

  val goal : statement_src -> t

  val lemma : t

  val assert' : ?loc:Loc.t -> file:string -> name:string -> unit -> t

  val goal' : ?loc:Loc.t -> file:string -> name:string -> unit -> t

  val inference : ?check:check -> ?comment:string -> rule:rule -> parent list -> t

  val simp : ?check:check -> ?comment:string -> rule:rule -> parent list -> t

  val esa : ?check:check -> ?comment:string -> rule:rule -> parent list -> t

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
  val from_subst : proof Scoped.t -> Subst.t -> t
  val proof : t -> proof
end

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
  val mk_f : step -> form -> t

  val mk_f_trivial : form -> t
  val mk_f_by_def : ID.t -> form -> t

  val mk_f_inference : ?check:check -> rule:rule -> form -> parent list -> t

  val mk_f_simp : ?check:check -> rule:rule -> form -> parent list -> t

  val mk_f_esa : ?check:check -> rule:rule -> form -> parent list -> t

  val mk_c : step -> SClause.t -> t

  val mk_bc : step -> bool_lit list -> t

  val mk_stmt : step -> Statement.input_t -> t

  val adapt_f : t -> form -> t
  val adapt_c : t -> SClause.t -> t

  val to_llproof : t -> LLProof.t
  (** Convert to low level t *)

  val is_proof_of_false : t -> bool

  (** {6 Conversion to a graph of proofs} *)

  val as_graph : (t, rule) CCGraph.t
  (** Get a graph of the proof *)

  val traverse :
    ?traversed:unit Tbl.t ->
    t ->
    t Sequence.t

  (** {6 IO} *)

  val pp_result_of : t CCFormat.printer
  val pp_notrec : t CCFormat.printer
  (** Non recursive printing on formatter *)

  val pp_tstp : t CCFormat.printer
  val pp_normal : t CCFormat.printer
  val pp : Options.print_format -> t CCFormat.printer
  (** Prints the proof according to the given input switch *)

  val pp_dot : name:string -> t CCFormat.printer
  (** Pretty print the proof as a DOT graph *)

  val pp_dot_file : ?name:string -> string -> t -> unit
  (** print to dot into a file *)

  val pp_dot_seq : name:string -> t Sequence.t CCFormat.printer
  (** Print a set of proofs as a DOT graph, sharing common subproofs *)

  val pp_dot_seq_file : ?name:string -> string -> t Sequence.t -> unit
  (** same as {!pp_dot_seq} but into a file *)

  val step_of_src : Statement.Src.t -> Step.t
  val parent_of_sourced : Statement.sourced_t -> Parent.t
end


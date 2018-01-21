
(* This file is free software, part of Zipperposition. See file "license" for more details. *)

(** {1 Rewriting on Terms and Literals} *)

(** Rewriting is a (relatively) efficient way of computing on terms
    and literals using rules that come from the input.
    Each rule is a pair [lhs, rhs] (left-hand side, right-hand side)
    that means that objects matching [lhs] should be rewritten into [rhs].

    For term rules, [lhs] and [rhs] are both terms (with [vars rhs ⊆ vars lhs]).

    For literal rules, [lhs] is an atomic literal,
    and [rhs] is a list of clauses.

    This is used for {b Deduction Modulo} in the prover.
*)

type term = Term.t

type defined_cst
(** Payload of a defined function symbol or type *)

type proof = Proof.step

val section : Util.Section.t

(** {2 Rewriting on Terms} *)
module Term : sig
  type rule

  module Rule : sig
    type t = rule

    val lhs : t -> term
    val rhs : t -> term
    val vars : t -> Term.VarSet.t
    val vars_l : t -> Type.t HVar.t list
    val head_id : t -> ID.t
    val args : t -> term list
    val arity : t -> int
    val proof : t -> proof

    val as_lit : t -> Literal.t

    val make_const : proof:Proof.t -> ID.t -> Type.t -> term -> t
    (** [make_const id ty rhs] is the same as [T.const id ty --> rhs] *)

    val make : proof:Proof.t -> ID.t -> Type.t -> term list -> term -> t
    (** [make id ty args rhs] is the same as [T.app (T.const id ty) args --> rhs] *)

    include Interfaces.HASH with type t := t
    include Interfaces.ORD with type t := t
    include Interfaces.PRINT with type t := t
  end

  module Set : sig
    include CCSet.S with type elt = rule
    val pp : t CCFormat.printer
  end

  type rule_set = Set.t

  (** Set of rules with their instantiation *)
  module Rule_inst_set : sig
    include CCSet.S with type elt = rule * Subst.t * Scoped.scope
    val pp : t CCFormat.printer
  end

  val normalize_term : ?max_steps:int -> term -> term * Rule_inst_set.t
  (** [normalize t] computes the normal form of [t] w.r.t the set
      of rewrite rules stored in IDs.
      Returns the new term and the set of rules that were used
      @param max_steps number of steps after which we stop *)

  val normalize_term_fst : ?max_steps:int -> term -> term
  (** Same as {!normalize_term} but ignores the set of rules *)

  (* TODO: [app f l] which is the same as [T.app f l], but also reduces
     whenever [f] is a defined constant with one rule which matches [l] *)

  val narrow_term :
    ?subst:Unif_subst.t ->
    scope_rules:Scoped.scope ->
    term Scoped.t ->
    (rule * Unif_subst.t) Sequence.t
    (** [narrow_term ~scope_rule t] finds the set of rules [(l --> r)]
        in IDs and substitutions [sigma] such that [sigma(l) = sigma(t)]
        @param scope_rules used for rules (LEFT) *)
end

(** {2 Rewriting on Literals and Clauses} *)

module Lit : sig
  type rule

  module Rule : sig
    type t = rule
    val lhs : t -> Literal.t
    val rhs : t -> Literal.t list list
    val proof : t -> proof
    val make : proof:Proof.t -> Literal.t -> Literal.t list list -> t
    val is_equational : t -> bool
    val as_clauses : t -> Literals.t list
    val head_id : t -> ID.t option
    val compare : t -> t -> int
    val pp : t CCFormat.printer
  end

  val normalize_clause :
    Literals.t ->
    (Literals.t list * rule * Subst.t * Scoped.scope *
       Subst.Renaming.t * Proof.tag list) option
  (** normalize literals of the clause w.r.t. rules, or return [None]
      if no rule applies. The input clause lives in scope 0. *)

  val narrow_lit :
    ?subst:Unif_subst.t ->
    scope_rules:Scoped.scope ->
    Literal.t Scoped.t ->
    (rule * Unif_subst.t * Proof.tag list) Sequence.t
    (** [narrow_term rules lit] finds the set of rules [(l --> clauses) in rules]
        and substitutions [sigma] such that [sigma(l) = sigma(lit)]
        @param scope_rules used for rules (LEFT) *)
end

(** {2 Rules in General} *)

type rule =
  | T_rule of Term.rule
  | L_rule of Lit.rule

module Rule : sig
  type t = rule
  val of_term : Term.Rule.t -> t
  val of_lit : Lit.Rule.t -> t
  val proof : t -> proof
  val pp : t CCFormat.printer

  val as_proof : t -> Proof.t

  val lit_as_proof_parent_subst :
    Subst.Renaming.t -> Subst.t -> Lit.Rule.t Scoped.t -> Proof.parent
  (** Helper for clause rewriting *)

  val set_as_proof_parents : Term.Rule_inst_set.t -> Proof.parent list
  (** Proof parents from a set of rules instances *)

  val make_lit : proof:Proof.t -> Literal.t -> Literal.t list list -> t
  (** Make a literal rule *)
end

module Rule_set : CCSet.S with type elt = rule

type rule_set = Rule_set.t

(** {2 Defined Constant}
    A constant that is defined by at least one rewrite rule *)

module Defined_cst : sig
  type t = defined_cst

  val ty : t -> Type.t

  val rules : t -> rule_set

  val rules_seq : t -> rule Sequence.t

  val rules_term_seq : t -> Term.rule Sequence.t

  val rules_lit_seq : t -> Lit.rule Sequence.t

  val defined_positions : t -> Defined_pos.Arr.t

  val level : t -> int

  val declare : ?level:int -> ID.t -> rule_set -> t
  (** [declare id rules] makes [id] a defined constant
      with the given (initial) set of rules
      @raise Invalid_argument if the ID is already a skolem or a constructor,
        or if the list of rules is empty
  *)

  val declare_or_add : ID.t -> rule -> unit
  (** [declare_or_add id rule] defines [id] if it's not already a
      defined constant, and add [rule] to it *)

  val declare_proj : proof:Proof.t -> Ind_ty.projector -> unit
  (** Declare an inductive projector *)

  val declare_cstor : proof:Proof.t -> Ind_ty.constructor -> unit
  (** Add a rewrite rule [cstor (proj1 x)…(projn x) --> x] *)

  val add_term_rule : t -> Term.rule -> unit
  val add_term_rule_l : t -> Term.rule list -> unit
  val add_lit_rule : t -> Lit.rule -> unit
  val add_lit_rule_l : t -> Lit.rule list -> unit

  val add_eq_rule : Lit.rule -> unit
  (** Add a rule on (dis)equality *)

  val add_eq_rule_l : Lit.rule list -> unit

  include Interfaces.PRINT with type t := t
end

val as_defined_cst : ID.t -> defined_cst option

val is_defined_cst : ID.t -> bool

val all_cst : Defined_cst.t Sequence.t
val all_rules : Rule.t Sequence.t

(**/**)
exception Payload_defined_cst of defined_cst
(**/**)

(* This file is free software, part of Logtk. See file "license" for more details. *)

(** {1 Reduction to CNF and Simplifications} *)

(** {!CNF} allows to transition from a free-form AST (with statements containing
    formulas as {!TypedSTerm.t}) into an AST using clauses
    and without some constructs such as "if/then/else" or "match" or "let".
    The output is more suitable for a Superposition-like prover.

    There also are conversion functions to go from clauses that use
    {!TypedSTerm.t}, into clauses that use the {!Term.t} (hashconsed,
    and usable in unification, indexing, etc.).

    We follow chapter 6 "Computing small clause normal forms"
    of the "handbook of automated reasoning" for the theoretical part.

    A few notes:

    In worst case, normal CNF transformation can lead to an exponential
    number of clauses, which is prohibitive. To avoid that, we use
    the Tseitin trick to {b name} some intermediate formulas
    by introducing fresh symbols (herein named "proxies") and
    defining them to be equivalent to the formula they define.

    This is done only if we estimate that adding the proxy will
    reduce the final number of clauses (See [Estimation] module).

    Before doing CNF we remove all the high-level constructs
    such as pattern-matching and "let" by introducing
    new symbols and defining the subterm to eliminate using
    this new symbol (See [Flatten] module).
    It is important to capture variables properly in this
    phase (as in closure conversion).
*)


type term = TypedSTerm.t
type form = TypedSTerm.t
type type_ = TypedSTerm.t
type lit = term SLiteral.t

(** See "computing small normal forms", in the handbook of automated reasoning.
    All transformations are made on curried terms and formulas. *)

exception Error of string

exception NotCNF of form

val miniscope : ?distribute_exists:bool -> form -> form
(** Apply miniscoping transformation to the term.
    @param distribute_exists see whether ?X:(p(X)|q(X)) should be
      transformed into (?X: p(X) | ?X: q(X)). Default: [false] *)

(** Options are used to tune the behavior of the CNF conversion. *)
type options =
  | DistributeExists
  (** if enabled, will distribute existential quantifiers over
      disjunctions. This can make skolem symbols smaller (smaller arity) but
      introduce more of them. *)

  | DisableRenaming
  (** disables formula renaming. Can re-introduce the worst-case
      exponential behavior of CNF. *)

  | InitialProcessing of (form -> form)
  (** any processing, at the beginning, before CNF starts  *)

  | PostNNF of (form -> form)
  (** any processing that keeps negation at leaves,
      just after reduction to NNF. Its output
      must not break the NNF form (negation at root only). *)

  | PostSkolem of (form -> form)
  (** transformation applied just after skolemization. It must not
      break skolemization nor NNF (no quantifier, no non-leaf negation). *)

type clause = lit list
(** Basic clause representation, as list of literals *)

val clause_to_fo :
  ?ctx:Term.Conv.ctx ->
  clause ->
  Term.t SLiteral.t list

type f_statement = (term, term, type_) Statement.t
(** A statement before CNF *)

type c_statement = (clause, term, type_) Statement.t
(** A statement after CNF *)

val pp_f_statement : f_statement CCFormat.printer
val pp_c_statement : c_statement CCFormat.printer
val pp_fo_c_statement : (Term.t SLiteral.t list, Term.t, Type.t) Statement.t CCFormat.printer


val is_clause : form -> bool
val is_cnf : form -> bool

(** {2 Main Interface} *)

val cnf_of :
  ?opts:options list ->
  ?ctx:Skolem.ctx ->
  f_statement ->
  c_statement CCVector.ro_vector
(** Transform the statement into proper CNF; returns a list of statements,
    including type declarations for new Skolem symbols or formulas proxies.
    Options are used to tune the behavior. *)

val cnf_of_seq :
  ?opts:options list ->
  ?ctx:Skolem.ctx ->
  f_statement Sequence.t ->
  c_statement CCVector.ro_vector

val type_declarations :
  c_statement Sequence.t ->
  type_ ID.Map.t
(** Compute the types declared in the statement sequence *)

(** {2 Conversions} *)

val convert :
  c_statement Sequence.t ->
  Statement.clause_t CCVector.ro_vector
(** Converts statements based on {!TypedSTerm} into statements
    based on {!Term} and {!Type} *)


(* This file is free software, part of Zipperposition. See file "license" for more details. *)

(** {1 Array of literals} *)

(** An array of literals is one of the major components of a clause.

    It defines (implicitly) a scope for its variables; applying a substitution
    should always be done with the same {!Subst.Renaming.t} for
    all literals in the array.

    This also provides printers, comparison, matching, positions,
    simplifications, etc.
    for such arrays of literals.
*)

type term = Term.t

type t = Literal.t array
(** Array of literals *)

val equal_com : t -> t -> bool
val compare : t -> t -> int

val compare_multiset :  ord:Ordering.t -> t -> t -> Comparison.t

include Interfaces.HASH with type t := t

val variant :
  ?subst:Subst.t -> t Scoped.t -> t Scoped.t ->
  (Subst.t * Builtin.Tag.t list) Iter.t
(** Variant checking (alpha-equivalence). It can reorder literals to do its
    check, so that might be computationnally expensive (a bit
    like subsumption). *)

val are_variant : t -> t -> bool
(** Simple interface on top of {!variant} with distinc scopes *)

val matching :
  ?subst:Subst.t -> pattern:t Scoped.t -> t Scoped.t ->
  (Subst.t * Builtin.Tag.t list) Iter.t

val matches : t -> t -> bool

val weight : t -> int
val ho_weight : t -> int
val depth : t -> int
val vars : t -> Type.t HVar.t list
val is_ground : t -> bool       (** all the literals are ground? *)

val to_form : t -> term SLiteral.t list
(** Make a 'or' formula from literals *)

val apply_subst : Subst.Renaming.t -> Subst.t -> t Scoped.t -> t

val of_unif_subst : Subst.Renaming.t -> Unif_subst.t -> t

val map : (term -> term) -> t -> t

val pos : t -> CCBV.t
val neg : t -> CCBV.t

val maxlits : ord:Ordering.t -> t -> CCBV.t
(** Bitvector of positions of maximal literals *)

val maxlits_l : ord:Ordering.t -> t -> (Literal.t * int) list
(** List of maximal literals, with their index, among the array *)

val is_max : ord:Ordering.t -> t -> int -> bool
(** Is the i-th literal maximal in the ordering? *)

val is_trivial : t -> bool
(** Tautology? (simple syntactic criterion only) *)

val is_absurd : t -> bool
(** All literals are false, or there are no literals *)

val apply_subst : Subst.Renaming.t -> Subst.t -> t Scoped.t -> t

module Seq : sig
  val vars : t -> Type.t HVar.t Iter.t
  val terms : t -> term Iter.t
  val to_form : t -> term SLiteral.t Iter.t
end

(** {3 High order combinators} *)

module Pos : sig
  val at : t -> Position.t -> term
  (** Return the subterm at the given position, or
      @raise Invalid_argument if the position is not valid *)

  val lit_at : t -> Position.t -> Literal.t * Position.t
  (** Lookup which literal the position is about, return it
      and the rest of the position.
      @raise Invalid_argument if the position is not valid *)

  val replace : t -> at:Position.t -> by:term -> unit
  (** In-place modification of the array, in which the subterm at given
      position is replaced by the [by] term.
      @raise Invalid_argument if the position is not valid *)

  val idx : Position.t -> int
  (** Index in the literal array
      @raise Invalid_argument if the position is incorrect *)

  val tail : Position.t -> Position.t
  (** sub-position
      @raise Invalid_argument if the position is incorrect *)

  val cut : Position.t -> int * Position.t
  (** Index + literal position *)
end

module Conv : sig
  val of_forms : ?hooks:Literal.Conv.hook_from list -> term SLiteral.t list -> t
  (** Convert a list of atoms into literals *)

  val to_forms : ?hooks:Literal.Conv.hook_to list -> t -> term SLiteral.t list
  (** To list of formulas *)

  val to_s_form :
    ?allow_free_db:bool ->
    ?ctx:Term.Conv.ctx ->
    ?hooks:Literal.Conv.hook_to list ->
    t ->
    TypedSTerm.Form.t

  val to_tst : t -> TypedSTerm.t
end

module View : sig
  val get_eqn : t -> Position.t -> (term * term * bool) option
  (** get the term l at given position in clause, and r such that l ?= r
      is the Literal.t at the given position.
      @raise Invalid_argument if the position is not valid in the *)

  (** The following functions will raise [Invalid_argument] if the
      position is not valid or if the literal isn't what's asked for *)

  val get_eqn_exn : t -> Position.t -> (term * term * bool)
end

val fold_lits : eligible:(int -> Literal.t -> bool) ->
  t -> (Literal.t * int) Iter.t
(** Fold over literals who satisfy [eligible]. The folded function
    is given the literal and its index. *)

val fold_eqn : ?both:bool -> ?sign:bool -> ord:Ordering.t ->
  eligible:(int -> Literal.t -> bool) ->
  t -> (term * term * bool * Position.t) Iter.t
(** fold f over all literals sides, with their positions.
    NB: REPORTED SIGN IS THE SAME AS IF Lit.is_pos WAS CALLED!
    f is given [(left side, right side, sign, position of left side)]
    if [ord] is present, then only the max side of an oriented
      equation will be visited, otherwise they will both be explored.
    if [both = true], then both sides of a non-oriented equation
      will be visited, otherwise only one side.
    if [sign = true], then only positive equations are visited; if it's
      [false], only negative ones; if it's not defined, both. *)

val fold_eqn_simple : ?sign:bool -> t -> (term * term * bool * Position.t) Iter.t
(** Like previous version but simpler: it visits all equations only in the orientation l = r  *)

val fold_terms : ?vars:bool -> ?var_args:bool -> ?fun_bodies:bool -> ?ty_args:bool -> which:[<`Max|`All] ->
  ord:Ordering.t -> subterms:bool ->
  eligible:(int -> Literal.t -> bool) ->
  t -> term Position.With.t Iter.t
(** See {!Literal.fold_terms}, which is the same but for the
    [eligible] argument *)

val symbols : ?init:ID.Set.t -> ?include_types:bool -> t -> ID.Set.t

(** {2 IO} *)

val pp : t CCFormat.printer
val pp_vars : t CCFormat.printer
val pp_tstp : t CCFormat.printer
val pp_tstp_closed : t CCFormat.printer
val pp_zf : t CCFormat.printer
val pp_zf_closed : t CCFormat.printer
val to_string : t -> string

(** {2 Special kinds of literal arrays} *)

val is_RR_horn_clause : t -> bool
(** Recognized whether the clause is a Range-Restricted Horn clause *)

val is_horn : t -> bool
(** Recognizes Horn clauses (at most one positive literal) *)

val is_unique_max_horn_clause : ord:Ordering.t -> t -> bool
(** Recognized whether the clause is a Range-Restricted Horn clause *)

(** {2 Shielded Variables} *)

val is_shielded : Term.var -> t -> bool
(** Is this variable shielded in this clause? *)

val unshielded_vars : ?filter:(Term.var -> bool) -> t -> Term.var list
(** Set of variables occurring unshielded *)

val vars_distinct : t -> bool

val ground_lits : t -> t

val num_predicate : t -> int
(** Number of non-predicate literals *)

val num_equational : t -> int
(** Number of non-predicate literals *)

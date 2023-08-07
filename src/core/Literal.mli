(* This file is free software, part of Zipperposition. See file "license" for more details. *)

(** {1 Literals} *)

(** Literals are the representation of atomic formulas in the clausal
    world of resolution/superposition provers.

    A literal is an atomic formula (equality or predicate), paired with a sign
    that carries negation.
*)

type term = Term.t

(** a literal, that is, a signed atomic formula *)
type t = private
  | True
  | False
  | Equation of term * term * bool

val equal_com : t -> t -> bool     (** commutative equality of lits *)

val compare : t -> t -> int     (** lexicographic comparison of literals *)

include Interfaces.HASH with type t := t

val hash : t -> int               (** hashing of literal *)

val weight : t -> int             (** weight of the lit (sum of weights of terms) *)
val ho_weight : t -> int          (** ho weight of the lit (sum of weights of terms,
                                      ignoring applied variables and lambda prefixes *)
val heuristic_weight : (term -> int) -> t -> int   (** heuristic difficulty to eliminate lit *)

val depth : t -> int              (** depth of literal *)

val eqn_sign : t -> bool          (** equation sign: T for s=t and F for s!=t *)

val is_positivoid : t -> bool     (** is the literal s=t where neither s nor t are T or F or of the form s = T? *)

val is_negativoid : t -> bool     (** is the literal of the form s!=t or of the form s = F *)

val is_eqn : t -> bool            (** is the literal a proper (in)equation or prop? *)

val is_prop : t -> bool           (** is the literal a boolean proposition? *)

val is_eq : t -> bool             (** is the literal of the form a = b? *)

val is_neq : t -> bool            (** is the literal of the form a != b? *)

val mk_eq : term -> term -> t
(** build literals. If sides so not have the same sort,
    a SortError will be raised. An ordering must be provided *)

val mk_neq : term -> term -> t

val mk_lit : term -> term -> bool -> t

val mk_prop : term -> bool -> t   (** proposition *)

val mk_true : term -> t     (** true proposition *)

val mk_false : term -> t    (** false proposition *)

val mk_tauto : t (** tautological literal *)

val mk_absurd : t (** absurd literal, like ~ true *)

val no_prop_invariant : t -> bool

val mk_constraint : term -> term -> t
(** [mk_constraint t u] makes a disequation or a HO constraint depending
    on how [t] and [u] look. *)

val matching : ?subst:Subst.t -> pattern:t Scoped.t -> t Scoped.t ->
  (Subst.t * Builtin.Tag.t list) Iter.t
(** checks whether subst(lit_a) matches lit_b. Returns alternative
    substitutions s such that s(lit_a) = lit_b and s contains subst. *)

val subsumes : ?subst:Subst.t -> t Scoped.t -> t Scoped.t ->
  (Subst.t * Builtin.Tag.t list) Iter.t
(** More general version of {!matching}, yields [subst]
    such that [subst(lit_a) => lit_b]. *)

val are_opposite_subst : subst:Subst.t -> t Scoped.t -> t Scoped.t -> bool
(** Literals are equal according to given substitution for variables,
    but are of opposite sign  *)

val are_opposite_same_sc: t -> t -> bool
(** Literals are equal, but are of opposite signs *)

val variant : ?subst:Subst.t -> t Scoped.t -> t Scoped.t ->
  (Subst.t * Builtin.Tag.t list) Iter.t

val unify :
  ?subst:Unif_subst.t -> t Scoped.t -> t Scoped.t ->
  (Unif_subst.t * Builtin.Tag.t list) Iter.t

val are_variant : t -> t -> bool

val apply_subst : Subst.Renaming.t -> Subst.t -> t Scoped.t -> t

val apply_subst_no_simp : Subst.Renaming.t -> Subst.t -> t Scoped.t -> t

val apply_subst_list : Subst.Renaming.t -> Subst.t -> t list Scoped.t -> t list

exception Lit_is_constraint

val negate : t -> t
(** negate literal *)

val is_constraint : t -> bool
(** Is the literal a constraint? *)

val is_ho_constraint : t -> bool

val of_unif_subst: Subst.Renaming.t -> Unif_subst.t -> t list
(** Make a list of (negative) literals out of the unification constraints
    contained in this substitution. *)

val map : (term -> term) -> t -> t (** functor *)

val fold : ('a -> term -> 'a) -> 'a -> t -> 'a  (** basic fold *)

val for_all : (term -> bool) -> t -> bool  (** for the term or both terms of the literal *)

val vars : t -> Type.t HVar.t list (** gather variables *)

val var_occurs : Type.t HVar.t -> t -> bool

val is_ground : t -> bool
val symbols : ?include_types:bool -> t -> ID.Set.t
val root_terms : t -> term list (** all the terms immediatly under the lit *)

val to_ho_term : t -> term option
(** Conversion to higher-order term using {!Term.Form} *)

val as_ho_predicate : t -> (Term.var * term * term list * bool) option
(** View on literals [F t1…tn] and [¬ (F t1…tn)] *)

val is_ho_predicate : t -> bool
(** Does {!as_ho_predicate} return Some? *)

module Set : CCSet.S with type elt = t

(** {2 Basic semantic checks} *)

val is_trivial : t -> bool

val is_absurd : t -> bool

val is_absurd_tags : t -> Proof.tag list (** if [is_absurd lit], return why *)
val is_app_var_eq : t -> bool

val is_type_pred : t -> bool
val is_typex_pred : t -> bool (* like in E, type predicate with multiple variables *)

val is_predicate_lit : t -> bool

val as_inj_def : t -> (ID.t * (Term.var * Term.var) list) option
val is_pure_var : t -> bool
val as_pos_pure_var : t -> (Term.var * Term.var) option

val max_term_positions : ord:Ordering.t -> t -> int

val fold_terms :
  ?position:Position.t -> ?vars:bool -> ?var_args:bool -> ?fun_bodies:bool -> ?ty_args:bool -> 
  which:[<`Max|`All] ->
  ?ord:Ordering.t ->
  subterms:bool ->
  t ->
  term Position.With.t Iter.t
(** Iterate on terms, maybe subterms, of the literal.
    Variables are ignored if [vars] is [false].

    [vars] decides whether variables are iterated on too (default [false])
    [var_args] decides whether arguments of applied variables are iterated on too
    [fun_bodies] decides whether bodies of lambda-expressions are iterated on too
    [ty_args] decides whether type arguments are iterated on too
    [subterms] decides whether strict subterms, not only terms that
    occur directly under the literal, are explored.

    [which] is used to decide which terms to visit:
    - if [which] is [`Max], only the maximal terms are explored
    - if [which] is [`All], all root terms are explored *)

val normalize_eq : t -> t option

(** {2 Comparisons} *)
module Comp : sig
  val max_terms : ord:Ordering.t -> t -> term list
  (** Maximal terms of the literal *)

  val compare : ord:Ordering.t -> t -> t -> Comparison.t
  (** partial comparison of literals under the given term ordering *)
end

module Seq : sig
  val terms : t -> term Iter.t
  val vars :  t -> Type.t HVar.t Iter.t
  val symbols : ?include_types:bool -> t -> ID.t Iter.t
end

(** {2 Positions} *)
module Pos : sig
  type split = {
    lit_pos : Position.t;
    term_pos : Position.t;
    term : term;
  }
  (** Full description of a position in a literal. It contains:
      - [lit_pos]: the literal-prefix of the position
      - [term_pos]: the suffix that describes a subterm position
      - [term]: the term root, just under the literal itself.
        given this, applying T.Pos.at to the subterm position and
        the root term we obtain the sub-term itself. *)

  val split : t -> Position.t -> split
  (** @raise Invalid_argument if the position is incorrect *)

  val at : t -> Position.t -> term
  (** Subterm at given position, or
      @raise Invalid_argument if the position is invalid *)

  val replace : t -> at:Position.t -> by:term -> t
  (** Replace subterm, or
      @raise Invalid_argument if the position is invalid *)

  val cut : t -> Position.t -> Position.t * Position.t
  (** cut the subterm position off. For instance a position "left.1.2.stop"
      in an equation "l=r" will yield
      "left.stop", "1.2.stop".

      it always holds that [let a,b = cut p in Position.append a b = p] *)

  val root_term : t -> Position.t -> term
  (** Obtain the term at the given position, at the root of the literal.
      It should hold that
      [root_term lit p = [at lit (fst (cut p))]]. *)

  val term_pos : t -> Position.t -> Position.t
  (** [term_pos lit p = snd (cut lit p)], the subterm position. *)

  val is_max_term : ord:Ordering.t -> t -> Position.t -> bool
  (** Is the term at the given position, maximal in the literal w.r.t this
      ordering? In other words, if the term is replaced by a smaller term,
      can the whole literal becomes smaller? *)
end

val replace : t -> old:term -> by:term -> t
(** [replace lit ~old ~by] syntactically replaces all occurrences of [old]
    in [lit] by the term [by]. *)

(** {2 Specific views} *)
module View : sig
  val as_eqn : t -> (term * term * bool) option

  val get_eqn : t -> Position.t -> (term * term * bool) option
  (** View of a Prop or Equation literal, oriented by the position. If the
      position selects its left term, return l, r, otherwise r, l.
      for propositions it will always be p, true.
      @return None for other literals
      @raise Invalid_argument if the position doesn't match the literal. *)

  val get_lhs : t -> term option
  val get_rhs : t -> term option
end

(** {2 Conversions} *)
module Conv : sig
  type hook_from = term SLiteral.t -> t option
  type hook_to = t -> term SLiteral.t option

  val of_form : ?hooks:hook_from list -> term SLiteral.t -> t
  (** Conversion from a formula. By default no ordering is considered.
      @raise Invalid_argument if the formula is not atomic. *)

  val to_form : ?hooks:hook_to list -> t -> term SLiteral.t

  val to_s_form :
    ?allow_free_db:bool -> ?ctx:Term.Conv.ctx -> ?hooks:hook_to list ->
    t -> TypedSTerm.Form.t

  val lit_to_tst : ?ctx:Term.Conv.ctx -> term SLiteral.t  -> TypedSTerm.t
end

(** {2 IO} *)

type print_hook = CCFormat.t -> t -> bool
(** might print the literal on the given buffer.
    @return true if it printed, false otherwise *)

val add_default_hook : print_hook -> unit

val pp_debug : ?hooks:print_hook list -> t CCFormat.printer

val pp : t CCFormat.printer
val pp_zf : t CCFormat.printer
val pp_tstp : t CCFormat.printer
val to_string : t -> string


(* This file is free software, part of Zipperposition. See file "license" for more details. *)

open Logtk

type proof_step = Proof.Step.t
type proof = Proof.S.t

module type S = sig
  module Ctx : Ctx.S

  type t
  type clause = t

  (** {2 Flags} *)

  type flag = SClause.flag

  val set_flag : flag -> t -> bool -> unit (** set boolean flag *)
  val get_flag : flag -> t -> bool (** get value of boolean flag *)

  val mark_redundant : t -> unit
  val is_redundant : t -> bool
  val mark_backward_simplified : t -> unit
  val is_backward_simplified : t -> bool

  (** {2 Basics} *)

  include Interfaces.EQ with type t := t
  include Interfaces.HASH with type t := t
  val compare : t -> t -> int

  val id : t -> int
  val lits : t -> Literal.t array

  val is_ground : t -> bool
  val weight : t -> int

  module Tbl : CCHashtbl.S with type key = t

  val is_goal : t -> bool
  (** Looking at the clause's proof, return [true] iff the clause is an
      initial (negated) goal from the problem *)

  val distance_to_goal : t -> int option
  (** See {!Proof.distance_to_goal}, applied to the clause's proof *)

  val comes_from_goal : t -> bool
  (** [true] iff the clause is (indirectly) deduced from a goal or lemma *)

  (** {2 Boolean Abstraction} *)

  val pp_trail : Trail.t CCFormat.printer
  (** Printer for boolean trails, that uses {!Ctx} to display boxes *)

  val has_trail : t -> bool
  (** Has a non-empty trail? *)

  val trail : t -> Trail.t
  (** Get the clause's trail *)

  val trail_l : t list -> Trail.t
  (** Merge the trails of several clauses *)

  val update_trail : (Trail.t -> Trail.t) -> t -> t
  (** Change the trail. The resulting clause has same parents, proof
      and literals as the input one *)

  val trail_subsumes : t -> t -> bool
  (** [trail_subsumes c1 c2 = Trail.subsumes (get_trail c1) (get_trail c2)] *)

  val is_active : t -> v:Trail.valuation -> bool
  (** True if the clause's trail is active in this valuation *)

  (** {2 Constructors} *)

  val create :
    penalty:int ->
    trail:Trail.t ->
    Literal.t list ->
    proof_step ->
    t
  (** Build a new clause from the given literals.
      @param trail boolean trail
      @param penalty heuristic penalty due to history of the clause
        (the higher, the less likely the clause is to be picked soon)
      also takes a list of literals and a proof builder *)

  val create_a :
    penalty:int ->
    trail:Trail.t ->
    Literal.t array ->
    proof_step ->
    t
  (** Build a new clause from the given literals. *)

  val of_sclause :
    ?penalty:int ->
    SClause.t ->
    proof_step ->
    t

  val of_forms :
    ?penalty:int ->
    trail:Trail.t ->
    Term.t SLiteral.t list ->
    proof_step ->
    t
  (** Directly from list of formulas *)

  val of_forms_axiom :
    ?penalty:int ->
    file:string -> name:string ->
    Term.t SLiteral.t list -> t
  (** Construction from formulas as axiom (initial clause) *)

  val of_statement : Statement.clause_t -> t list
  (** Extract a clause from a statement, if any *)

  val proof_step : t -> proof_step
  (** Extract its proof from the clause *)

  val proof : t -> proof
  (** Obtain the pair [conclusion, step] *)

  val proof_parent : t -> Proof.Parent.t

  val proof_parent_subst : Subst.Renaming.t -> t Scoped.t -> Subst.t -> Proof.Parent.t

  val update_proof : t -> (proof_step -> proof_step) -> t
  (** [update_proof c f] creates a new clause that is
      similar to [c] in all aspects, but with
      the proof [f (proof_step c)] *)

  val is_empty : t -> bool
  (** Is the clause an empty clause? *)

  val length : t -> int
  (** Number of literals *)

  val maxlits : t Scoped.t -> Subst.t -> CCBV.t
  (** List of maximal literals *)

  val is_maxlit : t Scoped.t -> Subst.t -> idx:int -> bool
  (** Is the i-th literal maximal in subst(clause)? Equivalent to
      Bitvector.get (maxlits ~ord c subst) i *)

  val eligible_res : t Scoped.t -> Subst.t -> CCBV.t
  (** Bitvector that indicates which of the literals of [subst(clause)]
      are eligible for resolution. THe literal has to be either maximal
      among selected literals of the same sign, if some literal is selected,
      or maximal if none is selected. *)

  val eligible_res_no_subst : t -> CCBV.t
  (** More efficient version of {!eligible_res} with [Subst.empty] *)

  val eligible_param : t Scoped.t -> Subst.t -> CCBV.t
  (** Bitvector that indicates which of the literals of [subst(clause)]
      are eligible for paramodulation. That means the literal
      is positive, no literal is selecteed, and the literal
      is maximal among literals of [subst(clause)]. *)

  val is_eligible_param : t Scoped.t -> Subst.t -> idx:int -> bool
  (** Check whether the [idx]-th literal is eligible for paramodulation *)

  val has_selected_lits : t -> bool
  (** does the clause have some selected literals? *)

  val is_selected : t -> int -> bool
  (** check whether a literal is selected *)

  val selected_lits : t -> (Literal.t * int) list
  (** get the list of selected literals *)

  val penalty : t -> int

  val is_unit_clause : t -> bool
  (** is the clause a unit clause? *)

  val is_oriented_rule : t -> bool
  (** Is the clause a positive oriented clause? *)

  val symbols : ?init:ID.Set.t -> t Sequence.t -> ID.Set.t
  (** symbols that occur in the clause *)

  val to_sclause : t -> SClause.t

  val to_forms : t -> Term.t SLiteral.t list
  (** Easy iteration on an abstract view of literals *)

  val to_s_form : t -> TypedSTerm.Form.t

  (** {2 Iterators} *)

  module Seq : sig
    val lits : t -> Literal.t Sequence.t
    val terms : t -> Term.t Sequence.t
    val vars : t -> Type.t HVar.t Sequence.t
  end

  (** {2 Filter literals} *)

  module Eligible : sig
    type t = int -> Literal.t -> bool
    (** Eligibility criterion for a literal *)

    val res : clause -> t
    (** Only literals that are eligible for resolution *)

    val param : clause -> t
    (** Only literals that are eligible for paramodulation *)

    val eq : t
    (** Equations *)

    val arith : t

    val filter : (Literal.t -> bool) -> t

    val max : clause -> t
    (** Maximal literals of the clause *)

    val pos : t
    (** Only positive literals *)

    val neg : t
    (** Only negative literals *)

    val always : t
    (** All literals *)

    val combine : t list -> t
    (** Logical "and" of the given eligibility criteria. A literal is
        eligible only if all elements of the list say so. *)

    val ( ** ) : t -> t -> t
    (** Logical "and" *)

    val ( ++ ) : t -> t -> t
    (** Logical "or" *)

    val ( ~~ ) : t -> t
    (** Logical "not" *)
  end

  (** {2 Set of clauses} *)

  (** Simple set *)
  module ClauseSet : CCSet.S with type elt = t

  (** {2 Position} *)

  module Pos : sig
    val at : t -> Position.t -> Term.t
  end

  (** {2 Clauses with more data} *)

  (** Clause within which a subterm (and its position) are hilighted *)
  module WithPos : sig
    type t = {
      clause : clause;
      pos : Position.t;
      term : Term.t;
    }

    val compare : t -> t -> int
    val pp : t CCFormat.printer
  end

  (** {2 IO} *)

  val pp : t CCFormat.printer
  val pp_tstp : t CCFormat.printer
  val pp_tstp_full : t CCFormat.printer  (** Print in a cnf() statement *)

  val to_string : t -> string (** Debug printing to a  string *)

  val pp_set : ClauseSet.t CCFormat.printer
  val pp_set_tstp : ClauseSet.t CCFormat.printer

  (**/**)
  val check_types : t -> unit
  (**/**)
end


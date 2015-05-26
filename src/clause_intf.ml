
(*
Zipperposition: a functional superposition prover for prototyping
Copyright (c) 2013, Simon Cruanes
All rights reserved.

Redistribution and use in source and binary forms, with or without
modification, are permitted provided that the following conditions are met:

Redistributions of source code must retain the above copyright notice, this
list of conditions and the following disclaimer.  Redistributions in binary
form must reproduce the above copyright notice, this list of conditions and the
following disclaimer in the documentation and/or other materials provided with
the distribution.

THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS "AS IS" AND
ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED
WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE ARE
DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT HOLDER OR CONTRIBUTORS BE LIABLE
FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL
DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR
SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER
CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY,
OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE
OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
*)

open Logtk

type scope = Substs.scope

module type S = sig
  module Ctx : Ctx.S

  type t
  type clause = t

  (** {2 Flags} *)

  val flag_ground : int                       (** clause is ground *)
  val flag_lemma : int                        (** clause is a lemma *)
  val flag_persistent : int                   (** clause cannot be redundant *)

  val set_flag : int -> t -> bool -> unit     (** set boolean flag *)
  val get_flag : int -> t -> bool             (** get value of boolean flag *)
  val new_flag : unit -> int                  (** new flag that can be used on clauses *)

  (** {2 Basics} *)

  include Interfaces.EQ with type t := t
  include Interfaces.HASH with type t := t
  val compare : t -> t -> int

  val id : t -> int
  val lits : t -> Literal.t array
  val parents : t -> t list

  val compact : t -> CompactClause.t (** Turn into a compact clause *)
  val is_ground : t -> bool
  val weight : t -> int

  module CHashtbl : Hashtbl.S with type key = t

  module CHashSet : sig
    type t
    val create : unit -> t
    val is_empty : t -> bool
    val member : t -> clause -> bool
    val iter : t -> (clause -> unit) -> unit
    val add : t -> clause -> unit
    val to_list : t -> clause list
  end

  val is_child_of : child:t -> t -> unit
    (** [is_child_of ~child c] is to be called to remember that [child] is a child
        of [c], is has been infered/simplified from [c] *)

  val follow_simpl : t -> t
    (** Follow the "hcsimplto" links until the clause has None *)

  val simpl_to : from:t -> into:t -> unit
    (** [simpl_to ~from ~into] sets the link of [from] to [into], so that
        the simplification of [from] into [into] is cached. *)

  val is_conjecture : t -> bool
    (** Looking at the clause's proof, return [true] iff the clause is an
        initial conjecture from the problem *)

  val distance_to_conjecture : t -> int option
    (** See {!Proof.distance_to_conjecture}, applied to the clause's proof *)

  (** {2 Boolean Abstraction} *)

  module Trail : sig
    type t

    val equal : t -> t -> bool
    val hash_fun : t -> int64 -> int64
    val hash : t -> int
    val compare : t -> t -> int

    type bool_lit = Ctx.BoolLit.t

    val empty : t
    val singleton : bool_lit -> t
    val mem : bool_lit -> t -> bool
    val add : bool_lit -> t -> t
    val remove : bool_lit -> t -> t
    val map : (bool_lit -> bool_lit) -> t -> t
    val for_all : (bool_lit -> bool) -> t -> bool
    val exists : (bool_lit -> bool) -> t -> bool
    val of_list : bool_lit list -> t
    val to_list : t -> bool_lit list
    val to_seq : t -> bool_lit Sequence.t

    val subsumes : t -> t -> bool
    (** [subsumes a b] is true iff [a] is a subset of [b] *)

    val is_empty : t -> bool
    (** Empty trail? *)

    val is_trivial : t -> bool
    (** returns [true] iff the trail contains both [i] and [-i]. *)

    val merge : t list -> t
    (** Merge several trails (e.g. from different clauses) *)

    val filter : (bool_lit -> bool) -> t -> t
    (** Only keep a subset of boolean literals *)

    type valuation = bool_lit -> bool
    (** A boolean valuation *)

    val is_active : t -> v:valuation -> bool
    (** [Trail.is_active t ~v] is true iff all boolean literals
        in [t] are satisfied in the boolean valuation [v]. *)

    val pp : Buffer.t -> t -> unit
    val print : Format.formatter -> t -> unit
  end

  val as_bool : t -> Trail.bool_lit option
  (** Boolean atom for this clause (if any) *)

  val as_bool_exn : t -> Trail.bool_lit
  (** Unsafe version of {!as_bool}.
      @raise Failure if the clause doesn't have a boolean name *)

  val set_bool_name : t -> Trail.bool_lit -> unit
  (** Set the boolean name of this clause.
      Basically, [set_bool_name c i; as_bool i = Some i] holds.
      @raise Failure if the clause already has a name *)

  val has_trail : t -> bool
  (** Has a non-empty trail? *)

  val get_trail : t -> Trail.t
  (** Get the clause's trail *)

  val update_trail : (Trail.t -> Trail.t) -> t -> t
  (** Change the trail. The resulting clause has same parents, proof
      and literals as the input one *)

  val trail_subsumes : t -> t -> bool
  (** [trail_subsumes c1 c2 = Trail.subsumes (get_trail c1) (get_trail c2)] *)

  val compact_trail : Trail.t -> CompactClause.bool_lit list
  (** Compact the trail for use with {!CompactClause} *)

  val is_active : t -> v:Trail.valuation -> bool
  (** True if the clause's trail is active in this valuation *)

  (** {2 Constructors} *)

  module CHashcons : Hashcons.S with type elt = clause

  val on_proof : (Literal.t array * Proof.t) Signal.t
  (** signal triggered with [c, p] whenever a proof [p]  is associated with
      the clause [c], even if the proof is dumped *)

  val create : ?parents:t list -> ?selected:CCBV.t -> ?trail:Trail.t ->
               Literal.t list ->
               (CompactClause.t -> Proof.t) -> t
    (** Build a new clause from the given literals.
        @param parents parent clauses (if none, should be a tautology or axiom)
        @param selected selected literals (can be computed later)
        @param trail boolean trail (default [[]])
        also takes a list of literals and a proof builder *)

  val create_a : ?parents:t list -> ?selected:CCBV.t -> ?trail:Trail.t ->
                  Literal.t array ->
                  (CompactClause.t -> Proof.t) -> t
    (** Build a new clause from the given literals. *)

  val of_forms : ?parents:t list -> ?selected:CCBV.t -> ?trail:Trail.t ->
                    Formula.FO.t list ->
                    (CompactClause.t -> Proof.t) -> t
    (** Directly from list of formulas *)

  val of_forms_axiom : ?role:string -> file:string -> name:string ->
                       Formula.FO.t list -> t
    (** Construction from formulas as axiom (initial clause) *)

  val proof : t -> Proof.t
    (** Extract its proof from the clause *)

  val update_proof : t -> (Proof.t -> CompactClause.t -> Proof.t) -> t
    (** [update_proof c f] creates a new clause that is
        similar to [c] in all aspects, but with
        the proof [f (proof c) (compact c)] *)

  val stats : unit -> (int*int*int*int*int*int)
    (** hashconsing stats *)

  val is_empty : t -> bool
    (** Is the clause an empty clause? *)

  val length : t -> int
    (** Number of literals *)

  val descendants : t -> int SmallSet.t
    (** set of ID of descendants of the clause *)

  val apply_subst : renaming:Substs.Renaming.t -> Substs.t -> t -> scope -> t
    (** apply the substitution to the clause *)

  val maxlits : t -> scope -> Substs.t -> CCBV.t
    (** List of maximal literals *)

  val is_maxlit : t -> scope -> Substs.t -> idx:int -> bool
    (** Is the i-th literal maximal in subst(clause)? Equivalent to
        Bitvector.get (maxlits ~ord c subst) i *)

  val eligible_res : t -> scope -> Substs.t -> CCBV.t
    (** Bitvector that indicates which of the literals of [subst(clause)]
        are eligible for resolution. THe literal has to be either maximal
        among selected literals of the same sign, if some literal is selected,
        or maximal if none is selected. *)

  val eligible_param : t -> scope -> Substs.t -> CCBV.t
    (** Bitvector that indicates which of the literals of [subst(clause)]
        are eligible for paramodulation. That means the literal
        is positive, no literal is selecteed, and the literal
        is maximal among literals of [subst(clause)]. *)

  val is_eligible_param : t -> scope -> Substs.t -> idx:int -> bool
    (** Check whether the [idx]-th literal is eligible for paramodulation *)

  val eligible_chaining : t -> scope -> Substs.t -> CCBV.t
    (** Bitvector of literals of [subst(clause)] that are eligible
        for equality chaining or inequality chaining. That amouns to being
        a maximal, positive inequality literal within the clause,
        and assume the clause has no selected literal. *)

  val has_selected_lits : t -> bool
    (** does the clause have some selected literals? *)

  val is_selected : t -> int -> bool
    (** check whether a literal is selected *)

  val selected_lits : t -> (Literal.t * int) list
    (** get the list of selected literals *)

  val is_unit_clause : t -> bool
    (** is the clause a unit clause? *)

  val is_oriented_rule : t -> bool
    (** Is the clause a positive oriented clause? *)

  val symbols : ?init:Symbol.Set.t -> t Sequence.t -> Symbol.Set.t
    (** symbols that occur in the clause *)

  (** {2 Iterators} *)

  module Seq : sig
    val lits : t -> Literal.t Sequence.t
    val terms : t -> FOTerm.t Sequence.t
    val vars : t -> FOTerm.t Sequence.t

    val abstract : t -> (bool * FOTerm.t Sequence.t) Sequence.t
      (** Easy iteration on an abstract view of literals *)
  end

  (** {2 Filter literals} *)

  module Eligible : sig
    type t = int -> Literal.t -> bool
      (** Eligibility criterion for a literal *)

    val res : clause -> t
      (** Only literals that are eligible for resolution *)

    val param : clause -> t
      (** Only literals that are eligible for paramodulation *)

    val chaining : clause -> t
      (** Eligible for chaining *)

    val eq : t
      (** Equations *)

    val ineq : clause -> t
      (** Only literals that are inequations *)

    val ineq_of : clause -> Theories.TotalOrder.t -> t
      (** Only literals that are inequations for the given ordering *)

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
  module ClauseSet : Set.S with type elt = t

  (** Set with access by ID, bookeeping of maximal var... *)
  module CSet : sig
    (** Set of hashconsed clauses. *)
    type t

    val empty : t
      (** the empty set *)

    val is_empty : t -> bool
      (** is the set empty? *)

    val size : t -> int
      (** number of clauses in the set *)

    val add : t -> clause -> t
      (** add the clause to the set *)

    val add_list : t -> clause list -> t
      (** add several clauses to the set *)

    val remove_id : t -> int -> t
      (** remove clause by ID *)

    val remove : t -> clause -> t
      (** remove hclause *)

    val remove_list : t -> clause list -> t
      (** remove hclauses *)

    val get : t -> int -> clause
      (** get a clause by its ID *)

    val mem : t -> clause -> bool
      (** membership test *)

    val mem_id : t -> int -> bool
      (** membership test by t ID *)

    val choose : t -> clause option
      (** Choose a clause in the set *)

    val union : t -> t -> t
      (** Union of sets *)

    val inter : t -> t -> t
      (** Intersection of sets *)

    val iter : t -> (clause -> unit) -> unit
      (** iterate on clauses in the set *)

    val iteri : t -> (int -> clause -> unit) -> unit
      (** iterate on clauses in the set with their ID *)

    val fold : t -> 'b -> ('b -> int -> clause -> 'b) -> 'b
      (** fold on clauses *)

    val to_list : t -> clause list
    val of_list : clause list -> t

    val to_seq : t -> clause Sequence.t
    val of_seq : t -> clause Sequence.t -> t
    val remove_seq : t -> clause Sequence.t -> t
    val remove_id_seq : t -> int Sequence.t -> t
  end

  (** {2 Position} *)

  module Pos : sig
    val at : t -> Position.t -> FOTerm.t
  end

  (** {2 Clauses with more data} *)

  (** Clause within which a subterm (and its position) are hilighted *)
  module WithPos : sig
    type t = {
      clause : clause;
      pos : Position.t;
      term : FOTerm.t;
    }

    val compare : t -> t -> int
    val pp : Buffer.t -> t -> unit
  end

  (** {2 IO} *)

  val pp : Buffer.t -> t -> unit
  val pp_tstp : Buffer.t -> t -> unit
  val pp_tstp_full : Buffer.t -> t -> unit  (** Print in a cnf() statement *)

  val to_string : t -> string               (** Debug printing to a  string *)
  val fmt : Format.formatter -> t -> unit   (** debug printing *)

  val pp_set : Buffer.t -> CSet.t -> unit
  val pp_set_tstp : Buffer.t -> CSet.t -> unit
  val fmt_set : Format.formatter -> CSet.t -> unit
end


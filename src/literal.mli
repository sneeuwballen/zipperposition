
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

(** {1 Equational literals} *)

open Logtk

type scope = Substs.scope
type form = Formula.FO.t

type t = private
  | Equation of FOTerm.t * FOTerm.t * bool
  | Prop of FOTerm.t * bool
  | True
  | False
  (** a literal, that is, a signed equation or a proposition *)

val eq : t -> t -> bool         (** equality of literals *)
val eq_com : t -> t -> bool     (** commutative equality of lits *)
val compare : t -> t -> int     (** lexicographic comparison of literals *)

val hash : t -> int

val variant : ?subst:Substs.t -> t -> scope -> t -> scope -> Substs.t
  (** Are two literals alpha-equivalent? *)

val are_variant : t -> t -> bool

val compare_partial : ord:Ordering.t -> t -> t -> Comparison.t
  (** partial comparison of literals *)

val to_multiset : t -> FOTerm.t Multiset.t  (** literal to multiset of terms *)

val hash : t -> int                   (** hashing of literal *)
val weight : t -> int                 (** weight of the lit *)
val depth : t -> int                  (** depth of literal *)

val is_pos : t -> bool                (** is the literal positive? *)
val is_neg : t -> bool                (** is the literal negative? *)
val equational : t -> bool            (** is the literal a proper equation? *)
val orientation_of : ord:Ordering.t -> t ->
                     Comparison.t     (** get the orientation of the literal *)

val ineq_lit : spec:Theories.TotalOrder.t -> t -> Theories.TotalOrder.lit
  (** Assuming the literal is an inequation, returns the corresponding
      total order literal.
      @raise Not_found if the literal is not an inequality *)

val is_eq : t -> bool (* a = b *)
val is_neq : t -> bool (* a != b *)
val is_ineq : spec:Theories.TotalOrder.t -> t -> bool
val is_strict_ineq : spec:Theories.TotalOrder.t -> t -> bool
val is_nonstrict_ineq : spec:Theories.TotalOrder.t -> t -> bool

val ineq_lit_of : instance:Theories.TotalOrder.instance -> t -> Theories.TotalOrder.lit
  (** Extract a total ordering literal from the literal, only for the
      given ordering instance *)

val is_ineq_of : instance:Theories.TotalOrder.instance -> t -> bool
  (** [true] iff the literal is an inequation for the given total order *)

val ineq_lit : spec:Theories.TotalOrder.t -> t -> Theories.TotalOrder.lit
  (** Assuming the literal is an inequation, returns the corresponding
      total order literal.
      @raise Not_found if the literal is not an inequality *)

(** build literals. If sides so not have the same sort,
    a SortError will be raised. An ordering must be provided *)
val mk_eq : FOTerm.t -> FOTerm.t -> t
val mk_neq : FOTerm.t -> FOTerm.t -> t
val mk_lit : FOTerm.t -> FOTerm.t -> bool -> t
val mk_prop : FOTerm.t -> bool -> t   (* proposition *)
val mk_true : FOTerm.t -> t     (* true proposition *)
val mk_false : FOTerm.t -> t    (* false proposition *)
val mk_tauto : t (* tautological literal *)
val mk_absurd : t (* absurd literal, like ~ true *)

val mk_less : Theories.TotalOrder.instance -> FOTerm.t -> FOTerm.t -> t
val mk_lesseq : Theories.TotalOrder.instance -> FOTerm.t -> FOTerm.t -> t
val lit_of_form : form -> t (** translate eq/not to literal *)
val to_tuple : t -> (FOTerm.t * FOTerm.t * bool)
val form_of_lit : t -> form
val term_of_lit : t -> HOTerm.t                   (** translate lit to term *)

val apply_subst : renaming:Substs.Renaming.t ->
                  Substs.t -> t -> scope -> t

val negate : t -> t                     (** negate literal *)
val fmap : (FOTerm.t -> FOTerm.t) -> t -> t (** fmap in literal *)
val add_vars : unit FOTerm.Tbl.t -> t -> unit  (** Add variables to the set *)
val vars : t -> FOTerm.t list (** gather variables *)
val var_occurs : FOTerm.t -> t -> bool
val is_ground : t -> bool

module Seq : sig
  val terms : t -> FOTerm.t Sequence.t
  val vars : t -> FOTerm.t Sequence.t
  val symbols : t -> Symbol.t Sequence.t
end

val as_eqn : t -> FOTerm.t * FOTerm.t * bool
  (** Equational view of a literal *)

val get_eqn : t -> Position.t -> FOTerm.t * FOTerm.t * bool
  (** Equational view of a literal, oriented by the position *)

module Pos : sig
  val at : t -> Position.t -> FOTerm.t
    (** Subterm at given position, or
        @raise Not_found if the position is invalid *)

  val replace : t -> at:Position.t -> by:FOTerm.t -> t
    (** Replace subterm, or
        @raise Invalid_argument if the position is invalid *)
end

val apply_subst_list : renaming:Substs.Renaming.t ->
                       Substs.t ->
                       t list -> scope -> t list

val symbols : t -> Symbol.Set.t
  (** Symbols occurring in the literal *)

(** {2 IO} *)

val pp_debug : Buffer.t -> t -> unit
val pp_tstp : Buffer.t -> t -> unit
(*val pp_arith : Buffer.t -> t -> unit TODO *)
val pp : Buffer.t -> t -> unit
val set_default_pp : (Buffer.t -> t -> unit) -> unit

val to_string : t -> string
val fmt : Format.formatter -> t -> unit

(** {2 Arrays of literals} *)

module Arr : sig
  val eq : t array -> t array -> bool
  val eq_com : t array -> t array -> bool
  val compare : t array -> t array -> int
  val hash : t array -> int

  val variant : ?subst:Substs.t ->
                t array -> scope -> t array -> scope ->
                Substs.t
  val are_variant : t array -> t array -> bool

  val weight : t array -> int
  val depth : t array -> int
  val vars : t array -> FOTerm.t list
  val is_ground : t array -> bool             (** all the literals are ground? *)

  val terms : t array -> FOTerm.t Sequence.t

  val compact : t array -> CompactClause.t
    (** Make a compact clause *)

  val to_form : t array -> form
    (** Make a 'or' formula from literals *)

  val apply_subst : renaming:Substs.Renaming.t ->
                    Substs.t -> t array -> scope -> t array

  val fmap : (FOTerm.t -> FOTerm.t) -> t array -> t array

  val pos : t array -> BV.t
  val neg : t array -> BV.t
  val maxlits : ord:Ordering.t -> t array -> BV.t

  val is_trivial : t array -> bool
    (** Tautology? (simple syntactic criterion only) *)

  val of_forms : form list -> t array
    (** Convert a list of atoms into literals *)

  val to_forms : t array -> form list
    (** To list of formulas *)

  module Seq : sig
    val terms : t array -> FOTerm.t Sequence.t
    val as_eqns : t array -> (FOTerm.t * FOTerm.t * bool) Sequence.t
      (** Convert the lits into a sequence of equations *)
  end

  (** {3 High order combinators} *)

  module Pos : sig
    val at : t array -> Position.t -> FOTerm.t
      (** Return the subterm at the given position, or
          @raise Not_found if no such position is valid *)

    val replace : t array -> at:Position.t -> by:FOTerm.t -> unit
      (** In-place modification of the array, in which the subterm at given
          position is replaced by the [by] term.
          @raise Invalid_argument if the position is not valid *)
  end

  val get_eqn : t array -> Position.t -> FOTerm.t * FOTerm.t * bool
    (** get the term l at given position in clause, and r such that l ?= r
        is the Literal.t at the given position.
        @raise Invalid_argument if the position is not valid in the array *)

  val get_ineq : spec:Theories.TotalOrder.t ->
                 t array -> Position.t ->
                Theories.TotalOrder.lit
    (** Obtain the l <= r at the given position in the array, plus a
        boolean that is [true] iff the inequality is {b strict}, and
        the corresponding ordering instance (pair of symbols)
        @raise Invalid_argument if the position is not valid in the array
          or if the literal is not an inequation. *)

  val order_instances : spec:Theories.TotalOrder.t ->
                        t array ->
                        Theories.TotalOrder.instance list
    (** Returns a list of all ordering instances present in the array *)

  val terms_under_ineq : instance:Theories.TotalOrder.instance ->
                         t array -> FOTerm.t Sequence.t
    (** All terms that occur under an equation, a predicate,
        or an inequation for the given total ordering. *)

  val fold_lits : eligible:(int -> t -> bool) ->
                  t array -> 'a ->
                  ('a -> t -> int -> 'a) ->
                  'a
    (** Fold over literals who satisfy [eligible]. The folded function
        is given the literal and its index. *)

  val fold_eqn : ?both:bool -> ?sign:bool -> ord:Ordering.t ->
                  eligible:(int -> t -> bool) ->
                  t array -> 'a ->
                  ('a -> FOTerm.t -> FOTerm.t -> bool -> Position.t -> 'a) ->
                  'a
    (** fold f over all literals sides, with their positions.
        f is given (acc, left side, right side, sign, position of left side)
        if [ord] is present, then only the max side of an oriented
          equation will be visited, otherwise they will both be explored.
        if [both = true], then both sides of a non-oriented equation
          will be visited, otherwise only one side.
        if [sign = true], then only positive equations are visited; if it's
          [false], only negative ones; if it's not defined, both. *)

  val fold_ineq : spec:Theories.TotalOrder.t ->
                  eligible:(int -> t -> bool) ->
                  t array -> 'a ->
                  ('a -> Theories.TotalOrder.lit -> Position.t -> 'a) ->
                  'a
    (** Fold on inequalities of the lits. The fold function is given
        the inequation instance, plus its position within the array.
        [eligible] is used to filter which literals to fold over (given
        the literal and its index). *)

  val fold_terms : ?vars:bool -> which:[<`Max|`One|`Both] ->
                   ord:Ordering.t -> subterms:bool ->
                   eligible:(int -> t -> bool) ->
                   t array -> 'a ->
                   ('a -> FOTerm.t -> Position.t -> 'a) ->
                   'a
    (** Fold on terms, maybe subterms, of the literal array.
        Variables are ignored if [vars] is [false]. 

        [vars] decides whether variables are iterated on too (default [false])
        [eligible] decides whether literals are explored.
        [subterms] decides whether subterms are explored.

        [which] is used to decide on equational literals:
        - if [which] is [`Max], only the maximal side is explored (or both if not comparable)
        - if [which] is [`One], the maximal side, or an arbitrary one, is visited
        - if [which] is [`Both], both sides of any equations are visited.
    *)

  val symbols : ?init:Symbol.Set.t -> t array -> Symbol.Set.t

  (** {3 IO} *)

  val pp : Buffer.t -> t array -> unit
  val pp_tstp : Buffer.t -> t array -> unit
  val to_string : t array -> string
  val fmt : Format.formatter -> t array -> unit
end

(** {2 Special kinds of literal arrays} *)

val is_RR_horn_clause : t array -> bool
  (** Recognized whether the clause is a Range-Restricted Horn clause *)

val is_horn : t array -> bool
  (** Recognizes Horn clauses (at most one positive literal) *)

val is_pos_eq : t array -> (FOTerm.t * FOTerm.t) option
  (** Recognize whether the clause is a positive unit equality. *)


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

(** {1 Array of literals} *)

open Logtk

type scope = Substs.scope
type term = FOTerm.t
type form = Formula.FO.t

type t = Literal.t array
  (** Array of literals *)

val eq : t -> t -> bool
val eq_com : t -> t -> bool
val compare : t -> t -> int
val hash : t -> int

val variant : ?subst:Substs.t -> t -> scope -> t -> scope -> Substs.t Sequence.t
val are_variant : t -> t -> bool

val weight : t -> int
val depth : t -> int
val vars : t -> term list
val is_ground : t -> bool       (** all the literals are ground? *)

val to_form : t -> form
  (** Make a 'or' formula from literals *)

val apply_subst : renaming:Substs.Renaming.t ->
                  Substs.t -> t -> scope -> t

val map : (term -> term) -> t -> t

val pos : t -> BV.t
val neg : t -> BV.t

val maxlits : ord:Ordering.t -> t -> BV.t
  (** Bitvector of positions of maximal literals *)

val maxlits_l : ord:Ordering.t -> t -> (Literal.t * int) list
  (** List of maximal literals, with their index, among the array *)

val is_max : ord:Ordering.t -> t -> int -> bool
  (** Is the i-th literal maximal in the ordering? *)

val is_trivial : t -> bool
  (** Tautology? (simple syntactic criterion only) *)

module Seq : sig
  val terms : t -> term Sequence.t
  val abstract : t -> Index.lits
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
  val of_forms : ?hooks:Literal.Conv.hook_from list -> form list -> t
    (** Convert a list of atoms into literals *)

  val to_forms : ?hooks:Literal.Conv.hook_to list -> t -> form list
    (** To list of formulas *)
end

module View : sig
  val get_eqn : t -> Position.t -> (term * term * bool) option
    (** get the term l at given position in clause, and r such that l ?= r
        is the Literal.t at the given position.
        @raise Invalid_argument if the position is not valid in the *)

  val get_ineq : t -> Position.t -> Theories.TotalOrder.lit option
    (** Obtain the l <= r at the given position in the array, plus a
        boolean that is [true] iff the inequality is {b strict}, and
        the corresponding ordering instance (pair of symbols) *)

  val get_arith : t -> Position.t -> ArithLit.Focus.t option

  (** The following functions will raise [Invalid_argument] if the
     position is not valid or if the literal isn't what's asked for *)

  val get_eqn_exn : t -> Position.t -> (term * term * bool)
  val get_ineq_exn : t -> Position.t -> Theories.TotalOrder.lit
  val get_arith_exn : t -> Position.t -> ArithLit.Focus.t
end

val order_instances : t -> Theories.TotalOrder.t list
  (** Returns a list of all ordering instances present in the array *)

val terms_under_ineq : instance:Theories.TotalOrder.t ->
                       t -> term Sequence.t
  (** All terms that occur under an equation, a predicate,
      or an inequation for the given total ordering. *)

val fold_lits : eligible:(int -> Literal.t -> bool) ->
                t -> 'a ->
                ('a -> Literal.t -> int -> 'a) ->
                'a
  (** Fold over literals who satisfy [eligible]. The folded function
      is given the literal and its index. *)

val fold_eqn : ?both:bool -> ?sign:bool -> ord:Ordering.t ->
                eligible:(int -> Literal.t -> bool) ->
                t -> 'a ->
                ('a -> term -> term -> bool -> Position.t -> 'a) ->
                'a
  (** fold f over all literals sides, with their positions.
      f is given (acc, left side, right side, sign, position of left side)
      if [ord] is present, then only the max side of an oriented
        equation will be visited, otherwise they will both be explored.
      if [both = true], then both sides of a non-oriented equation
        will be visited, otherwise only one side.
      if [sign = true], then only positive equations are visited; if it's
        [false], only negative ones; if it's not defined, both. *)

val fold_ineq : eligible:(int -> Literal.t -> bool) ->
                t -> 'a ->
                ('a -> Theories.TotalOrder.lit -> Position.t -> 'a) ->
                'a
  (** Fold on inequalities of the lits. The fold function is given
      the inequation instance, plus its position within the array.
      [eligible] is used to filter which literals to fold over (given
      the literal and its index). *)

val fold_arith : eligible:(int -> Literal.t -> bool) ->
                 t -> 'a ->
                 ('a -> ArithLit.t -> Position.t -> 'a) ->
                'a
  (** Fold over eligible arithmetic literals *)

val fold_arith_terms : eligible:(int -> Literal.t -> bool) ->
                       which:[<`Max|`All] -> ord:Ordering.t ->
                       t -> 'a ->
                       ('a -> term -> ArithLit.Focus.t -> Position.t -> 'a) ->
                       'a
  (** Fold on terms under arithmetic literals, with the focus on
      the current term *)

val fold_terms : ?vars:bool -> which:[<`Max|`All] ->
                 ord:Ordering.t -> subterms:bool ->
                 eligible:(int -> Literal.t -> bool) ->
                 t -> 'a ->
                 ('a -> term -> Position.t -> 'a) ->
                 'a
  (** See {!Literal.fold_terms}, which is the same but for the
      [eligible] argument *)

val symbols : ?init:Symbol.Set.t -> t -> Symbol.Set.t

(** {2 IO} *)

val pp : Buffer.t -> t -> unit
val pp_tstp : Buffer.t -> t -> unit
val to_string : t -> string
val fmt : Format.formatter -> t -> unit

(** {2 Special kinds of literal arrays} *)

val is_RR_horn_clause : t -> bool
  (** Recognized whether the clause is a Range-Restricted Horn clause *)

val is_horn : t -> bool
  (** Recognizes Horn clauses (at most one positive literal) *)

val is_pos_eq : t -> (term * term) option
  (** Recognize whether the clause is a positive unit equality. *)

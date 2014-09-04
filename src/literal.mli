
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
type term = FOTerm.t
type form = Formula.FO.t

(** a literal, that is, a signed atomic formula *)
type t = private
  | True
  | False
  | Equation of term * term * bool
  | Prop of term * bool
  | Ineq of Theories.TotalOrder.lit
  | Arith of ArithLit.t
  | Subseteq of Theories.Sets.t * term list * term list * bool

val eq : t -> t -> bool         (** equality of literals *)
val eq_com : t -> t -> bool     (** commutative equality of lits *)
val compare : t -> t -> int     (** lexicographic comparison of literals *)

include Interfaces.HASH with type t := t

val hash : t -> int               (** hashing of literal *)
val weight : t -> int             (** weight of the lit (sum of weights of terms) *)
val heuristic_weight : (term -> int) -> t -> int   (** heuristic difficulty to eliminate lit *)
val depth : t -> int              (** depth of literal *)
val sign : t -> bool

val is_pos : t -> bool            (** is the literal positive? *)
val is_neg : t -> bool            (** is the literal negative? *)
val is_eqn : t -> bool            (** is the literal a proper (in)equation or prop? *)
val is_prop : t -> bool           (** is the literal a boolean proposition? *)
val is_eq : t -> bool             (** is the literal of the form a = b? *)
val is_neq : t -> bool            (** is the literal of the form a != b? *)
val is_ineq : t -> bool
val is_strict_ineq : t -> bool
val is_nonstrict_ineq : t -> bool
val is_subseteq : t -> bool

val is_ineq_of : instance:Theories.TotalOrder.t -> t -> bool
  (** [true] iff the literal is an inequation for the given total order *)

val is_arith : t -> bool
val is_arith_eqn : t -> bool    (** = or != *)
val is_arith_eq : t -> bool
val is_arith_neq : t -> bool
val is_arith_ineq : t -> bool   (** < or <= *)
val is_arith_less : t -> bool
val is_arith_lesseq : t -> bool
val is_arith_divides : t -> bool

(** build literals. If sides so not have the same sort,
    a SortError will be raised. An ordering must be provided *)
val mk_eq : term -> term -> t
val mk_neq : term -> term -> t
val mk_lit : term -> term -> bool -> t
val mk_prop : term -> bool -> t   (* proposition *)
val mk_true : term -> t     (* true proposition *)
val mk_false : term -> t    (* false proposition *)
val mk_tauto : t (* tautological literal *)
val mk_absurd : t (* absurd literal, like ~ true *)

val mk_less : Theories.TotalOrder.t -> term -> term -> t
val mk_lesseq : Theories.TotalOrder.t -> term -> term -> t

val mk_arith : ArithLit.t -> t
val mk_arith_op : ArithLit.op -> Z.t Monome.t -> Z.t Monome.t -> t
val mk_arith_eq : Z.t Monome.t -> Z.t Monome.t -> t
val mk_arith_neq : Z.t Monome.t -> Z.t Monome.t -> t
val mk_arith_less : Z.t Monome.t -> Z.t Monome.t -> t
val mk_arith_lesseq : Z.t Monome.t -> Z.t Monome.t -> t

val mk_divides : ?sign:bool -> Z.t -> power:int -> Z.t Monome.t -> t
val mk_not_divides : Z.t -> power:int -> Z.t Monome.t -> t

val mk_subseteq : ?sign:bool -> sets:Theories.Sets.t -> term list -> term list -> t
val mk_notsubseteq : sets:Theories.Sets.t -> term list -> term list -> t

val matching : ?subst:Substs.t -> t -> scope -> t -> scope ->
               Substs.t Sequence.t
(** checks whether subst(lit_a) matches lit_b. Returns alternative
    substitutions s such that s(lit_a) = lit_b and s contains subst. *)

val subsumes : ?subst:Substs.t -> t -> scope -> t -> scope ->
               Substs.t Sequence.t
(** More general version of {!matching}, yields [subst]
    such that [subst(lit_a) => lit_b]. *)

val variant : ?subst:Substs.t -> t -> scope -> t -> scope ->
              Substs.t Sequence.t

val unify : ?subst:Substs.t -> t -> scope -> t -> scope -> Substs.t Sequence.t

val are_variant : t -> t -> bool

val apply_subst : renaming:Substs.Renaming.t ->
                  Substs.t -> t -> scope -> t

val apply_subst_no_simp : renaming:Substs.Renaming.t ->
                          Substs.t -> t -> scope -> t

val apply_subst_list : renaming:Substs.Renaming.t ->
                       Substs.t -> t list -> scope -> t list

val negate : t -> t   (** negate literal *)
val map : (term -> term) -> t -> t (** functor *)
val fold : ('a -> term -> 'a) -> 'a -> t -> 'a  (** basic fold *)
val add_vars : unit FOTerm.Tbl.t -> t -> unit  (** Add variables to the set *)
val vars : t -> term list (** gather variables *)
val var_occurs : term -> t -> bool
val is_ground : t -> bool
val symbols : t -> Symbol.Set.t
val root_terms : t -> term list (** all the terms immediatly under the lit *)

(** {2 Basic semantic checks} *)

val is_trivial : t -> bool
val is_absurd : t -> bool

val fold_terms : ?position:Position.t -> ?vars:bool ->
                 which:[<`Max|`All] ->
                 ord:Ordering.t -> subterms:bool ->
                 t -> 'a ->
                 ('a -> term -> Position.t -> 'a) ->
                 'a
  (** Fold on terms, maybe subterms, of the literal.
      Variables are ignored if [vars] is [false].

      [vars] decides whether variables are iterated on too (default [false])
      [subterms] decides whether strict subterms, not only terms that
      occur directly under the literal, are explored.

      [which] is used to decide which terms to visit:
      - if [which] is [`Max], only the maximal terms are explored
      - if [which] is [`All], all root terms are explored *)

(** {2 Comparisons} *)
module Comp : sig
  val max_terms : ord:Ordering.t -> t -> term list
    (** Maximal terms of the literal *)

  val compare : ord:Ordering.t -> t -> t -> Comparison.t
    (** partial comparison of literals under the given term ordering *)
end

module Seq : sig
  val terms : t -> term Sequence.t
  val vars : t -> term Sequence.t
  val symbols : t -> Symbol.t Sequence.t

  val abstract : t -> (bool * term Sequence.t)
    (** Abstract view, suitable for term indexing for instance *)
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

(** {2 Specific views} *)
module View : sig
  val as_eqn : t -> (term * term * bool) option

  val get_eqn : t -> Position.t -> (term * term * bool) option
    (** View of a Prop or Equation literal, oriented by the position. If the
        position selects its left term, return l, r, otherwise r, l.
        for propositions it will always be p, true.
        @return None for other literals
        @raise Invalid_argument if the position doesn't match the literal. *)

  val get_ineq : t -> Theories.TotalOrder.lit option
    (** Assuming the literal is an inequation, returns the corresponding
        total order literal. *)

  val get_ineq_of : instance:Theories.TotalOrder.t ->
                    t -> Theories.TotalOrder.lit option
    (** Extract a total ordering literal from the literal, only for the
        given ordering instance *)

  val get_arith : t -> ArithLit.t option
    (** Extract an arithmetic literal *)

  val focus_arith : t -> Position.t -> ArithLit.Focus.t option
    (** Focus on a specific term in an arithmetic literal. The focused term is
        removed from its monome, and its coefficient is returned. *)

  val unfocus_arith : ArithLit.Focus.t -> t

  val get_subseteq : t -> (Theories.Sets.t * term list * term list * bool) option

  val get_subseteq_exn : t -> Theories.Sets.t * term list * term list * bool
end

(** {2 Conversions} *)
module Conv : sig
  type hook_from = form -> t option
  type hook_to = t -> form option

  val arith_hook_from : hook_from
  val total_order_hook_from : instance:Theories.TotalOrder.t -> hook_from
  val set_hook_from : sets:Theories.Sets.t -> hook_from
  val set_hook_to : hook_to

  val of_form : ?hooks:hook_from list -> form -> t
  (** Conversion from a formula. By default no ordering or arith theory
      is considered.
      @raise Invalid_argument if the formula is not atomic. *)

  val to_form : ?hooks:hook_to list -> t -> form
end

(** {2 IO} *)

type print_hook = Buffer.t -> t -> bool
  (** might print the literal on the given buffer.
      @return true if it printed, false otherwise *)

val add_default_hook : print_hook -> unit

val pp_debug : ?hooks:print_hook list -> Buffer.t -> t -> unit

val pp : Buffer.t -> t -> unit
val pp_tstp : Buffer.t -> t -> unit

val to_string : t -> string
val fmt : Format.formatter -> t -> unit

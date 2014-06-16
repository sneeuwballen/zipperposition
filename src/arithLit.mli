
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

(** {1 Arithmetic Literal} *)

open Logtk

type term = FOTerm.t
type scope = Substs.scope

(** {2 Type Decls} *)

type op =
  | Equal
  | Different
  | Less
  | Lesseq

type 'm divides = {
  num : Z.t;
  power : int;
  monome : 'm;
  sign : bool;
} (** [num^power divides monome] or not. *)

type t = private
  | Binary of op * Z.t Monome.t * Z.t Monome.t
  | Divides of Z.t Monome.t divides
(** Arithmetic literal (on integers) *)

type lit = t

(** {2 Basics} *)

val eq : t -> t -> bool
val eq_com : t -> t -> bool
val hash : t -> int
val cmp : t -> t -> int

val make : op -> Z.t Monome.t -> Z.t Monome.t -> t

val mk_eq : Z.t Monome.t -> Z.t Monome.t -> t
val mk_neq : Z.t Monome.t -> Z.t Monome.t -> t
val mk_less : Z.t Monome.t -> Z.t Monome.t -> t
val mk_lesseq : Z.t Monome.t -> Z.t Monome.t -> t

val mk_divides : ?sign:bool -> Z.t -> power:int -> Z.t Monome.t -> t
val mk_not_divides : Z.t -> power:int -> Z.t Monome.t -> t

val negate : t -> t

val sign : t -> bool
val polarity : t -> bool  (** Used for the literal ordering *)
val is_pos : t -> bool
val is_neg : t -> bool

val is_eq : t -> bool
val is_neq : t -> bool
val is_eqn : t -> bool   (* = or != *)
val is_ineq : t -> bool   (** < or <= *)
val is_less : t -> bool
val is_lesseq : t -> bool
val is_divides : t -> bool

val pp : Buffer.t -> t -> unit
val pp_tstp : Buffer.t -> t -> unit
val to_string : t -> string
val fmt : Format.formatter -> t -> unit

(** {2 Operators} *)

val fold : ('a -> term -> 'a) -> 'a -> t -> 'a

val map : (term -> term) -> t -> t (** functor *)

type 'a unif = subst:Substs.t -> 'a -> scope -> 'a -> scope -> Substs.t Sequence.t

val generic_unif: Z.t Monome.t unif -> t unif
  (** Generic unification/matching/variant, given such an operation on monomes *)

val apply_subst : renaming:Substs.Renaming.t -> Substs.t -> t -> scope -> t

val apply_subst_no_renaming : Substs.t -> t -> scope -> t

val apply_subst_no_simp : renaming:Substs.Renaming.t -> Substs.t -> t -> scope -> t
  (** Same as {!apply_subst} but takes care {B NOT} simplifying the
      literal. In practice, mostly useful for comparison purpose. *)

val matching : ?subst:Substs.t -> t -> scope -> t -> scope ->
               Substs.t Sequence.t
(** checks whether subst(lit_a) is equal to lit_b. Returns alternative
    substitutions s such that s(lit_a) = lit_b and s contains subst. *)

val variant : ?subst:Substs.t -> t -> scope -> t -> scope ->
              Substs.t Sequence.t

val unify : ?subst:Substs.t -> t -> scope -> t -> scope -> Substs.t Sequence.t

val subsumes : ?subst:Substs.t -> t -> scope -> t -> scope -> Substs.t Sequence.t
  (** Find substitutions such that [subst(lit_a)] implies [lit_b]. This is
      more general than matching. *)

val are_variant : t -> t -> bool

val is_trivial : t -> bool
val is_absurd : t -> bool

val fold_terms : ?pos:Position.t -> ?vars:bool ->
                 which:[<`Max|`All] ->
                 ord:Ordering.t -> subterms:bool ->
                 t -> 'a ->
                 ('a -> term -> Position.t -> 'a) ->
                 'a

val max_terms : ord:Ordering.t -> t -> term list
  (** Maximal terms of the literal *)

val to_form : t -> Formula.FO.t
  (** Conversion into a formula *)

(** {2 Iterators} *)

module Seq : sig
  val terms : t -> term Sequence.t
  val vars : t -> term Sequence.t
  val to_multiset : t -> (term * Z.t) Sequence.t
end

(** {2 Focus on a Term} *)

module Focus : sig
  (** focus on a term in one of the two monomes *)
  type t =
    | Left of op * Z.t Monome.Focus.t * Z.t Monome.t
    | Right of op * Z.t Monome.t * Z.t Monome.Focus.t
    | Div of Z.t Monome.Focus.t divides

  val mk_left : op -> Z.t Monome.Focus.t -> Z.t Monome.t -> t
  val mk_right : op -> Z.t Monome.t -> Z.t Monome.Focus.t -> t
  val mk_div : ?sign:bool -> Z.t -> power:int -> Z.t Monome.Focus.t -> t

  val get : lit -> Position.t -> t option

  val get_exn : lit -> Position.t -> t
    (** @raise Invalid_argument if the position doesn't fit *)

  val focus_term : lit -> term -> t option
    (** Attempt to focus on the given atomic term, if it occurs directly
        under the arith literal *)

  val focus_term_exn : lit -> term -> t

  val replace : t -> Z.t Monome.t -> lit
    (** [replace a m] replaces [mf.coeff × mf.term] with [m] where [mf] is the
        focused monome in [a], and return the resulting literal *)

  val term : t -> term
    (** Focused term *)

  val focused_monome : t -> Z.t Monome.Focus.t
    (** The focused monome *)

  val opposite_monome : t -> Z.t Monome.t option
    (** The opposite monome (in [Left] and [Right]), if any. *)

  val opposite_monome_exn : t -> Z.t Monome.t
    (** Unsafe version of {!opposite_monome}.
        @raise Invalid_argument if the literal is a [Div] *)

  val is_max : ord:Ordering.t -> t -> bool
    (** Is the focused term maximal in the literal? *)

  val is_strictly_max : ord:Ordering.t -> t -> bool
    (** Is the focused term maximal in the literal, ie is it greater
        than all the othe terms? *)

  val map_lit :
    f_m:(Z.t Monome.t -> Z.t Monome.t) ->
    f_mf:(Z.t Monome.Focus.t -> Z.t Monome.Focus.t) ->
    t -> t
    (** Apply a function to the monomes and focused monomes *)

  val product : t -> Z.t -> t
    (** Product by a constant *)

  val scale : t -> t -> t * t
    (** Multiply the two literals by some constant so that their focused
        literals have the same coefficient *)

  val scale_power : t -> int -> t
    (** For a "divides" literal, bring it to the given power.
        @raise Invalid_argument if the current power is strictly greater
          than the argument (cannot scale down), or if the literal
          is not a {!Div} *)

  val apply_subst : renaming:Substs.Renaming.t -> Substs.t -> t -> scope -> t
    (** Apply a substitution *)

  val apply_subst_no_renaming : Substs.t -> t -> scope -> t
    (** Apply a substitution with renaming (careful with collisions!) *)

  val unify : ?subst:Substs.t -> t -> scope -> t -> scope ->
              (t * t * Substs.t) Sequence.t
    (** Unify the two focused terms, and possibly other terms of their
        respective focused monomes; yield the new literals accounting for
        the unification along with the unifier *)

  val fold_terms : ?pos:Position.t -> lit -> 'a ->
                  ('a -> t -> Position.t -> 'a) -> 'a
    (** Fold on focused terms in the literal, one by one, with
        the position of the focused term *)

  val op : t ->
    [ `Binary of op | `Divides ]

  val unfocus : t -> lit
    (** Conversion back to a literal *)

  val pp : Buffer.t -> t -> unit
  val to_string : t -> string
  val fmt : Format.formatter -> t -> unit
end

(** {2 Some Utils for arith} *)
module Util : sig
  type divisor = {
    prime : Z.t;
    power : int;
  }

  val is_prime : Z.t -> bool
    (** Is the integer prime? *)

  val prime_decomposition : Z.t -> divisor list
    (** Decompose the number into a product of power-of-primes. Cheap if
        [is_prime n] was called before.
        @raise Invalid_argument if the number is negative *)

  val primes_leq : Z.t -> Z.t Sequence.t
    (** Sequence of prime numbers smaller than (or equal to) the given number *)
end

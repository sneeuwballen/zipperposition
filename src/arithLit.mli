
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


(** {1 Arithmetic-centric views of a literal}.

    Two  views are provided, one that compares a monome to zero,
    and one that has some priviledged term on the side *)

open Logtk

val is_arith : Literal.t -> bool
  (** Is this literal arithmetic (i.e., root predicate is equality or
      inequality, with arithmetic operators just underneath)? *)

val mk_less : FOTerm.t -> FOTerm.t -> Literal.t
  (** Smart constructor for strict inequality (performs simplifications) *)

val mk_lesseq : FOTerm.t -> FOTerm.t -> Literal.t
  (** Smart constructor for non strict inequality (performs simplifications) *)

val mk_eq : ord:Ordering.t -> FOTerm.t -> FOTerm.t -> Literal.t
  (** Smart constructor for equality *)

val mk_neq : ord:Ordering.t -> FOTerm.t -> FOTerm.t -> Literal.t
  (** Smart constructor for inequality *)

(** Comparison operator *)
type op =
  | Eq
  | Neq
  | Lt
  | Leq

(** Side of a particular term w.r.t the comparison operator *)
type side =
  | Left
  | Right

val flip : side -> side

(** {3 Canonical representation}
    An arithmetic literal can always be reduced to exactly one such
    "extracted" literal, by putting all terms with the same sign
    on the same side of the relation.
    
    Invariant: both monomes only have positive coefficients. *)

module Canonical : sig
  type t =
  | True
  | False
  | Compare of op * Monome.t * Monome.t

  val pp : Buffer.t -> t -> unit
  val to_string : t -> string

  val monomes : t -> Monome.t * Monome.t
    (** Return the monomes inside the literal.
        @raise Invalid_argument if the literal is true or false *)

  val op : t -> op
    (** Return the operator, or
        @raise Invalid_argument if the literal is true or false *)

  val extract : Literal.t -> t
    (** Convert a regular literal into a canonical literal.
        @raise Monome.NotLinear if the literal is not a linear expression *)

  val extract_opt : Literal.t -> t option
    (** Same as {!extract}, but doesn't raise *)

  val size : t -> int
    (** Number of distinct non-arithmetic subterms *)

  val to_lit : ord:Ordering.t -> t -> Literal.t
    (** Conversion back to a literal *)

  val apply_subst : renaming:Substs.FO.Renaming.t ->
                    Substs.FO.t -> t -> Substs.scope -> t
    (** Apply substitution to the literal *)

  val factor : t -> Substs.FO.t list
    (** Unify non-arith subterms pairwise, return corresponding
        substitutions *)

  val eliminate : ?fresh_var:(Type.t -> FOTerm.t) -> t ->
                  Monome.Solve.solution list
    (** Attempt to eliminate the literal *)
end

(** {3 Single-term literal}
    This type helps deal with the special case where there is a single
    term in the literal. It can therefore perform many simplifications. *)

module Single : sig
  type t = private
  | True
  | False
  | Compare of op * side * FOTerm.t * Symbol.t  (* side: side of the term w.r.t operator *)

  val pp : Buffer.t -> t -> unit
  val to_string : t -> string

  val of_canonical : Canonical.t -> t option

  val simplify : t -> t

  val to_lit : ord:Ordering.t -> t -> Literal.t
end

(** {3 Focused literal}
    Same as {!Canonical}, but with the focus on a given maximal term of
    the literal *)

module Focused : sig
  (** literal with focus on a single term within. The
      term always has a stricly positive coefficient. *)
  type t = private {
    side : side;      (* which side of the operator is the term? *)
    op : op;          (* comparison operator *)
    coeff : Symbol.t; (* strictly positive coeff of term *)
    term : FOTerm.t;  (* focused term *)
    same_side : Monome.t;   (* monome on the same side of comparison *)
    other_side : Monome.t;  (* monome on the other side of comparison *)
  }

  val term : t -> FOTerm.t
  val op : t -> op
  val coeff : t -> Symbol.t
  val monomes : t -> Monome.t * Monome.t

  val pp : Buffer.t -> t -> unit
  val to_string : t -> string

  val cmp : t -> t -> int

  val to_lit : ord:Ordering.t -> t -> Literal.t

  val ty : t -> Type.t
    (** Type of the literal's expression *)

  val product : Symbol.t -> t -> t
    (** Product by constant *)

  val of_canonical : ord:Ordering.t -> Canonical.t -> t list
    (** Isolate maximal subterms of a {!Canonical.t} representation *)

  val apply_subst : renaming:Substs.FO.Renaming.t ->
                    Substs.FO.t -> t -> Substs.scope -> t
    (** Apply substitution to the focused literal *)

  val scale : t -> t -> t * t
    (** Multiply the two literals by some constants, so that their focused
        term has the same coefficient. For integers, it requires computing
        their lcm. *)
end

(** {3 High level operations} *)

val is_trivial : Literal.t -> bool
  (** Is the literal a tautology in arithmetic? *)

val has_instances : Literal.t -> bool
  (** If the literal is arithmetic, return [true] iff it is compatible
      with the theory of arithmetic (e.g. X+2Y=3 is ok, but 1=2 is not).
      Otherwise return [true] *)

val simplify : ord:Ordering.t -> Literal.t -> Literal.t
  (** Simplify a literal (evaluation) *)

val eliminate : ?elim_var:(FOTerm.t -> bool) ->
                ?fresh_var:(Type.t -> FOTerm.t) ->
                Literal.t ->
                Substs.FO.t list
  (** List of substitutions that make the literal inconsistent.
      [elim_var] is called to check whether eliminating a variable
      in an equation is possible. *)

val heuristic_eliminate : Literal.t -> Substs.FO.t list
  (** Heuristic version of [eliminate] that tries to deal with some
      non-linear, or too hard, cases. For instance, square roots.
      TODO: instantiate inside to_int/ to_rat *)

(** {2 Arrays of literals} *)

module Arr : sig
  val fold_canonical : ?eligible:(int -> Literal.t -> bool) ->
                       Literal.t array -> 'a ->
                       ('a -> int -> Canonical.t -> 'a) -> 'a
    (** Fold on canonical literals, with their index in the array *)

  val view_canonical : Literal.t array ->
                       [ `Ignore of Literal.t | `Canonical of Canonical.t ] array
    (** View literals as canonical literals or regular literals *)

  val fold_focused : ?eligible:(int -> Literal.t -> bool) ->
                      ord:Ordering.t -> 
                      Literal.t array -> 'a ->
                      ('a -> int -> Focused.t -> 'a) -> 'a
    (** Fold on focused literals with their index. An index can occur several
        times (or none) depending on how many focused terms its literal has *)

  val view_focused :  ord:Ordering.t -> 
                      Literal.t array ->
                      [ `Ignore of Literal.t
                      | `Focused of Focused.t list ] array 
    (** View literals as regular literals or list of focused literals *)

  val view_bounds : Literal.t array ->
    [ `Ignore of Literal.t
    | `LowerBound of bool * Symbol.t * FOTerm.t
    | `HigherBound of bool * FOTerm.t * Symbol.t
    ] array
    (** Simple case where terms are bounded with arithmetic constants.
        Booleans indicate strictness of bound *)

  val purify : ord:Ordering.t -> 
               eligible:(int -> Literal.t -> bool) ->
               Literal.t array -> Literal.t array
    (** Purify the literals, by replacing arithmetic terms that occur
        under a non-interpreted predicate of formula, by a fresh variable,
        and adding the constraint variable=arith subterm to the literals.
        
        TODO: purify even constant terms (but not numeric constants) so
              that superposition works on p(a) & 2a = b? *)


  val eliminate : ord:Ordering.t ->
                  eligible:(int -> Literal.t -> bool) ->
                  Literal.t array ->
                  Literal.t array list
    (** Try to eliminate literals by finding relevant instantiations.
        Instantiations must bind variables only to satisfiable terms
        (ie terms that always represent at least one integer).
        Shielded variables (see {!shielded}) are never eliminated.*)

  val shielded : ?filter:(int -> Literal.t -> bool) ->
                  Literal.t array -> FOTerm.t -> bool
    (** Is the given variable shielded (ie occur as a subterm somewhere)?
        [filter] is used to know which literals can shield the variable.
        @raise Invalid_argument if the term is not a var *)

  val naked_vars : ?filter:(int -> Literal.t -> bool) ->
                    Literal.t array -> FOTerm.varlist
    (** Variables occurring in inequations, that are not shielded *)
end


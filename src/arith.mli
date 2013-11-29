
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

(** {6 Arithmetic Manipulations}
  
    This module contains builtin knownledge about (linear) arithmetic
    with the TPTP syntax and semantic. It mainly deals with two topics:

    - simplification (evaluation) of arithmetic terms/propositions
    - extraction of single subterms from (in)equations
*)

open Logtk

(** {2 Terms} *)

module T : sig
  val sum_list : FOTerm.t list -> FOTerm.t
    (** Sum of those terms *)

  val arith_kind : FOTerm.t -> [ `Const | `Var | `Expr | `Not ]
    (** Is the term an arithmetic expression? Includes constants, variables, and
        more complicated expressions built from arithmetic operators *)

  val is_arith : FOTerm.t -> bool
    (** Same as {!arith_kind}, but returns true for any result that is not `Not *)

  val is_compound_arith : FOTerm.t -> bool
    (** Is the term a composite arithmetic expression? *)

  val is_arith_const : FOTerm.t -> bool
    (** Is the term an arithmetic constant? *)

  val mk_sum : FOTerm.t -> FOTerm.t -> FOTerm.t
  val mk_difference : FOTerm.t -> FOTerm.t -> FOTerm.t
  val mk_product : FOTerm.t -> FOTerm.t -> FOTerm.t
  val mk_quotient : FOTerm.t -> FOTerm.t -> FOTerm.t
  val mk_uminus : FOTerm.t -> FOTerm.t

  val mk_quotient_e : FOTerm.t -> Symbol.t -> FOTerm.t
  val mk_remainder_e : FOTerm.t -> Symbol.t -> FOTerm.t

  (** Smart constructors, that perform small simplifications *)

  val mk_less : FOTerm.t -> FOTerm.t -> FOTerm.t
  val mk_lesseq : FOTerm.t -> FOTerm.t -> FOTerm.t
  val mk_greater : FOTerm.t -> FOTerm.t -> FOTerm.t
  val mk_greatereq : FOTerm.t -> FOTerm.t -> FOTerm.t

  val extract_subterms : FOTerm.t -> FOTerm.t list
    (** If the term's root is an arithmetic expression, extract the
        list of outermost terms that occur immediately as parameters
        of the arithmetic expression. Returns [] if the term is not
        arithmetic or if it's a pure arithmetic expression
        (akin to a constant). *)

  val shielded : FOTerm.t -> FOTerm.t -> bool
    (** [shielded v t] is true if [v] is a variable that occurs under a
        non interpreted symbol in [t] *)

  val flag_simplified : int
    (** flag for simplified terms *)

  val simplify : FOTerm.t -> FOTerm.t
    (** Arithmetic simplifications *)
end

(** {2 Formulas} *)

module F : sig
  val simplify : FOFormula.t -> FOFormula.t
    (** Simplify an arithmetic formula. In particular, it eliminates
        $greater and $greatereq, and simplifies subterms. *)
end

(** {2 Arithmetic-centric views of a literal}.

    Two  views are provided, one that compares a monome to zero,
    and one that has some priviledged term on the side.
*)

module Lit : sig
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

  (** {3 Single-term literal}
      This type helps deal with the special case where there is a single
      term in the literal. It can therefore perform many simplifications.
  *)

  module Single : sig
    type t = private
    | True
    | False
    | Eq of FOTerm.t * Symbol.t
    | Neq of FOTerm.t * Symbol.t
    | L_less of FOTerm.t * Symbol.t
    | L_lesseq of FOTerm.t * Symbol.t
    | R_less of Symbol.t * FOTerm.t
    | R_lesseq of Symbol.t * FOTerm. t

    val pp : Buffer.t -> t -> unit
    val to_string : t -> string

    val to_lit : ord:Ordering.t -> t -> Literal.t
  end

  (** {3 Comparison with 0}
      An arithmetic literal can always be reduced to exactly one such
      "extracted" literal, but putting all terms on the same side of the
      relation. *)

  module Extracted : sig
    type t = private
    | True
    | False
    | Eq of Monome.t  (* monome = 0 *)
    | Neq of Monome.t (* monome != 0 *)
    | Lt of Monome.t  (* monome < 0 *)
    | Leq of Monome.t (* monome <= 0 *)

    val pp : Buffer.t -> t -> unit
    val to_string : t -> string

    val extract : Literal.t -> t
      (** Convert a regular literal into an extracted literal.
          @raise Monome.NotLinear if the literal is not a linear expression *)

    val extract_opt : Literal.t -> t option
      (** Same as {!extract}, but doesn't raise *)

    val get_monome : t -> Monome.t
      (** Return the monome inside the literal.
          @raise Invalid_argument if the literal is true or false *)

    val to_lit : ord:Ordering.t -> t -> Literal.t
      (** Conversion back to a literal *)

    val factor : t -> Substs.FO.t list
      (** Unify non-arith subterms pairwise, return corresponding
          substitutions *)

    val eliminate : ?fresh_var:(Type.t -> FOTerm.t) -> t ->
                    Monome.Solve.solution list
      (** Attempt to eliminate the literal *)
  end

  (** {3 Literal with isolated term}
      This form is used for complex literals, that have
      several non-arithmetic subterms. An {!Extracted.t} can
      be transformed in several {!Pivoted.t}, or into one {!Single.t}.

      See the very important {!pivot} operation. *)

  module Pivoted : sig
    type t = private
    | Eq of FOTerm.t * Monome.t
    | Neq of FOTerm.t * Monome.t
    | L_less of FOTerm.t * Monome.t   (* term < monome *)
    | L_lesseq of FOTerm.t * Monome.t
    | R_less of Monome.t * FOTerm.t
    | R_lesseq of Monome.t * FOTerm.t

    val pp : Buffer.t -> t -> unit
    val to_string : t -> string

    val get_term : t -> FOTerm.t
      (** Extract the term part. *)

    val get_monome : t -> Monome.t
      (** Extract the monome part from the lit. *)

    val to_lit : ord:Ordering.t -> t -> Literal.t
      (** Convert back into a regular literal *)

    module List : sig
      val get_terms : t list -> FOTerm.t list
    end
  end

  (** {3 Pivot operation} *)

  type pivot_result =
    | Single of Single.t
    | Multiple of Pivoted.t list
    | True
    | False

  val pivot : Extracted.t -> pivot_result
    (** Pivots of an {!Extracted.t} *)

  (** {3 High level operations} *)

  val is_trivial : Literal.t -> bool
    (** Is the literal a tautology in arithmetic? *)

  val has_instances : Literal.t -> bool
    (** If the literal is arithmetic, return [true] iff it is compatible
        with the theory of arithmetic (e.g. X+2Y=3 is ok, but 1=2 is not).
        Otherwise return [true] *)

  val make_total : ord:Ordering.t -> Literal.t -> Literal.t
    (** be sure that the literal is "total", ie, if it's an equation, that
        replacing one side by the other is always safe.
        In particular:   a = b/3  is {b NOT} total for integers. *)

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
end

(** {2 Arrays of literals} *)

module Lits : sig
  val purify : ord:Ordering.t -> 
               eligible:(int -> Literal.t -> bool) ->
               Literal.t array -> Literal.t array
    (** Purify the literals, by replacing arithmetic terms that occur
        under a non-interpreted predicate of formula, by a fresh variable,
        and adding the constraint variable=arith subterm to the literals. *)

  val pivot : ord:Ordering.t ->
              eligible:(int -> Literal.t -> bool) ->
              Literal.t array ->
              Literal.t array list
    (** [pivot ~ord ~eligible lits] tries to pivot each literal
        which is [eligible] (ie [eligible index lit] returns [true]).
        Pivoting is done by extracting arithmetic literals [t <| monome]
        and replacing the old literal by those new ones (if [t] maximal).
        It returns a list of such pivoted arrays, each pivoted array resulting
        from a single pivoted literal. *)

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



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
  val sum_list : Term.t list -> Term.t
    (** Sum of those terms *)

  val is_arith : Term.t -> bool
    (** Is the term arithmetic? *)

  val is_arith_const : Term.t -> bool
    (** Is the term an arithmetic constant? *)

  val mk_sum : Term.t -> Term.t -> Term.t
  val mk_difference : Term.t -> Term.t -> Term.t
  val mk_product : Term.t -> Term.t -> Term.t
  val mk_quotient : Term.t -> Term.t -> Term.t
  val mk_uminus : Term.t -> Term.t

  val mk_less : Term.t -> Term.t -> Term.t
  val mk_lesseq : Term.t -> Term.t -> Term.t

  val extract_subterms : Term.t -> Term.t list
    (** If the term's root is an arithmetic expression, extract the
        list of outermost terms that occur immediately as parameters
        of the arithmetic expression. Returns [] if the term is not
        arithmetic or if it's a pure arithmetic expression
        (akin to a constant). *)

  val simplify : signature:Signature.t -> Term.t -> Term.t
    (** Arithmetic simplifications *)
end

(** {2 Formulas} *)

module F : sig
  val simplify : signature:Signature.t -> Formula.t -> Formula.t
    (** Simplify an arithmetic formula. In particular, it eliminates
        $greater and $greatereq, and simplifies subterms. *)
end

(** {2 Polynomes of order 1, over several variables}.

    Variables, in this module, are non-arithmetic terms, i.e. non-interpreted
    functions and predicates, that occur immediately under an arithmetic
    operator. For instance, in the term "f(X) + 1 + 3 × a", the variables
    are "f(X)" and "a", with coefficients "1" and "3".

    Monomes of integers {b must} satisfy the property that each
    coefficient is divisible by the [divby] field.
*)

module Monome : sig
  type t = private {
    coeffs : Symbol.t Term.TMap.t;
    constant : Symbol.t;
    divby : Symbol.t;  (* divide everything by this constant (cool for ints) *)
  }

  val const : Symbol.t -> t           (** Empty monomial, from constant (decides type) *)
  val singleton : ?divby:Symbol.t ->
                  Symbol.t ->
                  Term.t -> t         (** One term. *)
  val of_list : Symbol.t -> (Symbol.t * Term.t) list -> t

  val find : t -> Term.t -> Symbol.t  (** @raise Not_found if not present *)
  val mem : t -> Term.t -> bool       (** Is the term in the monome? *)
  val add : t -> Symbol.t -> Term.t -> t  (** Add term with coefficient. Sums coeffs. *)
  val remove : t -> Term.t -> t           (** Remove the term *)

  val terms : t -> Term.t list
    (** List of terms that occur in the monome with non-nul coefficients *)
  
  val to_list : t -> (Symbol.t * Term.t) list
    (** Terms and their coefficients. Ignores the constant and divby! *)

  val var_occurs : Term.t -> t -> bool
    (** Does the variable occur in the monome? *)

  val reduce_same_divby : t -> t -> t * t
    (** Reduce the two monomes to the same denominator *)

  val sum : t -> t -> t
  val difference : t -> t -> t
  val uminus : t -> t
  val product : t -> Symbol.t -> t  (** Product with constant *)
  val divby : t -> Symbol.t -> t    (** Division by constant, must be > 0 *)
  val succ : t -> t                 (** +1 *)
  val pred : t -> t                 (** -1 *)

  exception NotLinear
    
  val of_term : signature:Signature.t -> Term.t -> t
    (** try to get a monome from a term.
        @raise NotLinear if the term is not a proper monome. *)

  val of_term_opt : signature:Signature.t -> Term.t -> t option
    (** Exceptionless versionf of {!of_term} *)

  val to_term : t -> Term.t         (** convert back to a term *)

  val pp : Buffer.t -> t -> unit
  val to_string : t -> string
  val fmt : Format.formatter -> t -> unit
end

(** {2 View a Literal as an arithmetic Literal}.

    This module provides a given view of a literal, where some arith variable
    has been extracted *)

module Lit : sig
  type t = private
  | Eq of Term.t * Monome.t
  | Neq of Term.t * Monome.t
  | L_less of Term.t * Monome.t   (* term < monome *)
  | L_lesseq of Term.t * Monome.t
  | R_less of Monome.t * Term.t
  | R_lesseq of Monome.t * Term.t

  val is_arith : Literal.t -> bool
    (** Is this literal arithmetic (i.e., root predicate is equality or
        inequality, with arithmetic operators just underneath)? *)

  val extract : signature:Signature.t -> Literal.t -> t list
    (** Possible views of a literal *)

  val to_lit : ord:Ordering.t -> t -> Literal.t
    (** Convert back into a regular literal *)

  val simplify : ord:Ordering.t -> signature:Signature.t ->
                 Literal.t -> Literal.t
    (** Simplify a literal (evaluation) *)

  val get_term : t -> Term.t

  val get_monome : t -> Monome.t

  val eliminate : ?elim_var:(Term.t -> bool) -> signature:Signature.t ->
                  t -> Substs.t list
    (** List of substitutions that make the literal inconsistent.
        [elim_var] is called to check whether eliminating a variable
        in an equation is possible. *)

  (** {3 Operations on Lists of literals} *)
  module L : sig
    val get_terms : t list -> Term.t list

    val filter : t list -> (Term.t -> Monome.t -> bool) -> t list
  end
end

(** {2 Arrays of literals} *)

module Lits : sig
  val purify : ord:Ordering.t -> signature:Signature.t ->
               eligible:(int -> Literal.t -> bool) ->
               Literal.t array -> Literal.t array
    (** Purify the literals, by replacing arithmetic terms that occur
        under a non-interpreted predicate of formula, by a fresh variable,
        and adding the constraint variable=arith subterm to the literals. *)

  val pivot : ord:Ordering.t ->
              signature:Signature.t ->
              eligible:(int -> Literal.t -> bool) ->
              Literal.t array ->
              Literal.t array list
    (** [pivot ~ord ~signature ~eligible lits] tries to pivot each literal
        which is [eligible] (ie [eligible index lit] returns [true]).
        Pivoting is done by extracting arithmetic literals [t <| monome]
        and replacing the old literal by those new ones (if [t] maximal).
        It returns a list of such pivoted arrays, each pivoted array resulting
        from a single pivoted literal. *)
end

(** {2 Inference Rules} *)

val rewrite_lit : Env.lit_rewrite_rule
  (** Simplify literals by evaluating them; in the case of integer monomes,
      also reduce them to common denominator. *)

val factor_arith : Env.unary_inf_rule
  (** Try to unify terms of arithmetic literals *)

val pivot_arith : Env.unary_inf_rule
  (** Pivot arithmetic literals *)

val purify_arith : Env.unary_inf_rule
  (** Purification inference *)

val axioms : PFormula.t list
  (** Set of axioms useful to do arithmetic *)

(** {2 Setup} *)

val setup_penv : penv:PEnv.t -> unit

val setup_env : env:Env.t -> unit

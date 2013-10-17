
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

(** {1 Polynomes of order 1, over several variables}.

    Variables, in this module, are non-arithmetic terms, i.e. non-interpreted
    functions and predicates, that occur immediately under an arithmetic
    operator. For instance, in the term "f(X) + 1 + 3 × a", the variables
    are "f(X)" and "a", with coefficients "1" and "3".

    Monomes of integers {b must} satisfy the property that each
    coefficient is divisible by the [divby] field.
*)

open Logtk

type t = private {
  coeffs : Symbol.t FOTerm.Map.t;
  constant : Symbol.t;
  divby : Symbol.t;  (* divide everything by this constant (cool for ints) *)
}

val eq : t -> t -> bool       (* structural equality *)
val compare : t -> t -> int   (* arbitrary total order on monomes *)
val hash : t -> int

val const : Symbol.t -> t           (** Empty monomial, from constant (decides type) *)
val singleton : ?divby:Symbol.t ->
                Symbol.t ->
                FOTerm.t -> t         (** One term. *)
val of_list : Symbol.t -> (Symbol.t * FOTerm.t) list -> t

val find : t -> FOTerm.t -> Symbol.t  (** @raise Not_found if not present *)
val mem : t -> FOTerm.t -> bool       (** Is the term in the monome? *)
val add : t -> Symbol.t -> FOTerm.t -> t  (** Add term with coefficient. Sums coeffs. *)
val add_const : t -> Symbol.t -> t        (** Add given number to constant *)
val remove : t -> FOTerm.t -> t           (** Remove the term *)

val type_of : t -> Type.t           (** type of the monome *)

val is_constant : t -> bool
  (** Returns [true] if the monome is only a constant *)

val sign : t -> int
  (** Assuming [is_constant m], [sign m] returns the sign of [m].
      @raise Invalid_argument if the monome is not a constant *)

val terms : t -> FOTerm.t list
  (** List of terms that occur in the monome with non-nul coefficients *)

val vars : t -> FOTerm.t list
  (** Variables that occur in some term of the monome *)

val to_list : t -> (Symbol.t * FOTerm.t) list
  (** Terms and their coefficients. Ignores the constant and divby! *)

val var_occurs : FOTerm.t -> t -> bool
  (** Does the variable occur in the monome? *)

val reduce_same_divby : t -> t -> t * t
  (** Reduce the two monomes to the same denominator *)

val normalize : t -> t
  (** Normalize the [divby] field. If rat/real, divby will be 1,
      otherwise some coefficient will be irreducible. *)

val sum : t -> t -> t
val difference : t -> t -> t
val uminus : t -> t
val product : t -> Symbol.t -> t  (** Product with constant *)
val divby : t -> Symbol.t -> t    (** Division by constant, must be > 0 *)
val succ : t -> t                 (** +1 *)
val pred : t -> t                 (** -1 *)

val comparison : t -> t -> Comparison.t
  (** Try to compare two monomes. They may not be comparable (ie on some
      points, or in some models, one will be bigger), but some pairs of
      monomes are:
      for instance, 2X + 1 < 2X + 4  is always true
    *)

val dominates : t -> t -> bool
  (** [dominates m1 m2] is true if [m1] is always bigger or equal than
      [m2], in any model or variable valuation.
      if [dominates m1 m2 && dominates m2 m1], then [m1 = m2]. *)

exception NotLinear
  
val of_term : signature:Signature.t -> FOTerm.t -> t
  (** try to get a monome from a term.
      @raise NotLinear if the term is not a proper monome. *)

val of_term_opt : signature:Signature.t -> FOTerm.t -> t option
  (** Exceptionless versionf of {!of_term} *)

val of_term_infer : FOTerm.t -> t
  (** Infer signature from term, and then make a monome. *)

val to_term : t -> FOTerm.t
  (** convert back to a term *)

val apply_subst : ?recursive:bool -> renaming:Substs.FO.Renaming.t ->
                  Substs.FO.t -> t -> Substs.scope -> t
  (** Apply a substitution to the monome's terms *)

val is_ground : t -> bool
  (** Are there no variables in the monome? *)

val pp : Buffer.t -> t -> unit
val to_string : t -> string
val fmt : Format.formatter -> t -> unit

(** {2 Satisfiability} *)

val has_instances : t -> bool
  (** For real or rational, always true. For integers, returns true
      iff g divides [m.constant], where g is the
      GCD of [c] for [c] in [m.coeffs] union [m.divby].

      The intuition is that this returns [true] iff the monome, with its
      denominator [divby], actually has some instances in its type. Trivially
      true in reals or rationals, this is only the case for integers if
      [x * m.divby = m.coeffs + m.constant] is a satisfiable diophantine
      equation.
  *)

val total_expression : t -> bool
  (** For real or rationals, always true. For integers, returns true
      iff the monome evaluates to an integer for any valuation of free
      variables and terms that occur in the right hand side. Most of
      the time, it means that the denominator is 1.
      
      For instance, a/2 is not a total expression, 3 + 5Y is total,
      but (3Y+7/5) has instances without being total. *)

val floor : t -> t
  (** Highest monome that is <= m, and that satisfies [has_instances]. *)

val ceil : t -> t
  (** Same as {!round_low} but rounds high *)

(** {2 Find Solutions} *)

module Solve : sig
  type solution = (FOTerm.t * t) list
    (** List of constraints (term = monome). It means that
        if all those constraints are satisfied, then a solution
        to the given problem has been found *)

  val split_solution : solution -> Substs.FO.t * solution
    (** Split the solution into a variable substitution, and a
        list of constraints on non-variable terms *)

  val diophant2 : Big_int.big_int -> Big_int.big_int -> Big_int.big_int ->
                  Big_int.big_int * Big_int.big_int * Big_int.big_int
    (** Find the solution vector for this diophantine equation, or fails.
        @return a triple [u, v, gcd] such that for all int [k],
        [u + b * k, v - a * k] is solution of equation [a * x + b * y = const].
        @raise Failure if the equation is unsolvable *)

  val eq_zero : ?fresh_var:(Type.t -> FOTerm.t) -> t -> solution list
    (** Returns substitutions that make the monome always equal to zero.
        Fresh variables may be generated using [fresh_var],
        for diophantine equations. Returns the empty list if no solution is
        found.

        For instance, on the monome 2X + 3Y - 7, it may generate a new variable
        Z and return the substitution  [X -> 3Z - 7, Y -> 2Z + 7] *)

  val lt_zero : ?fresh_var:(Type.t -> FOTerm.t) -> t -> solution list
    (** Solve for the monome to be always strictly lower than zero. This
        may not return all solutions, but a subspace of it
        @param fresh_var see {!solve_eq_zero} *)
end

(** {2 Lib} *)

val bij : t Bij.t
val arbitrary_int : t QCheck.Arbitrary.t (* with int coeffs *)
val arbitrary_rat : t QCheck.Arbitrary.t (* with rat coeffs *)
val arbitrary : t QCheck.Arbitrary.t     (* with any coeffs *)

val arbitrary_ty : Type.t -> t QCheck.Arbitrary.t
  (* for the given type. Works only for $int and $rat *)

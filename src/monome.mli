
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
*)

open Logtk

type t = private {
  const : Symbol.t;
  terms : (Symbol.t * FOTerm.t) list;
}

val eq : t -> t -> bool       (* structural equality *)
val compare : t -> t -> int   (* arbitrary total order on monomes *)
val hash : t -> int

val ty : t -> Type.t   (** type of the monome *)

val const : Symbol.t -> t           (** Empty monomial, from constant (decides type) *)
val singleton : Symbol.t -> FOTerm.t -> t         (** One term. *)
val of_list : Symbol.t -> (Symbol.t * FOTerm.t) list -> t

val find : t -> FOTerm.t -> Symbol.t  (** @raise Not_found if not present *)
val mem : t -> FOTerm.t -> bool       (** Is the term in the monome? *)

val add : t -> Symbol.t -> FOTerm.t -> t  (** Add term with coefficient. Sums coeffs. *)
val add_const : t -> Symbol.t -> t        (** Add given number to constant *)
val remove : t -> FOTerm.t -> t           (** Remove the term *)
val remove_const : t -> t   (** Remove constant *)

module Seq : sig
  val terms : t -> FOTerm.t Sequence.t
  val vars : t -> FOTerm.t Sequence.t
  val coeffs : t -> (Symbol.t * FOTerm.t) Sequence.t
end

val is_const : t -> bool
  (** Returns [true] if the monome is only a constant *)

val sign : t -> int
  (** Assuming [is_constant m], [sign m] returns the sign of [m].
      @raise Invalid_argument if the monome is not a constant *)

val size : t -> int
  (** Number of distinct terms. 0 means that the monome is a constant *)

val terms : t -> FOTerm.t list
  (** List of terms that occur in the monome with non-nul coefficients *)

val to_list : t -> (Symbol.t * FOTerm.t) list
  (** Terms and their coefficients. Ignores the constant! *)

val var_occurs : var:FOTerm.t -> t -> bool
  (** Does the variable occur in the monome? *)

val sum : t -> t -> t
val difference : t -> t -> t
val uminus : t -> t
val product : t -> Symbol.t -> t  (** Product with constant *)
val succ : t -> t                 (** +1 *)
val pred : t -> t                 (** -1 *)

val sum_list : t list -> t
  (** Sum of a list. @raise Failure if the list is empty *)

val comparison : t -> t -> Comparison.t
  (** Try to compare two monomes. They may not be comparable (ie on some
      points, or in some models, one will be bigger), but some pairs of
      monomes are:
      for instance, 2X + 1 < 2X + 4  is always true *)

val dominates : t -> t -> bool
  (** [dominates m1 m2] is true if [m1] is always bigger or equal than
      [m2], in any model or variable valuation.
      if [dominates m1 m2 && dominates m2 m1], then [m1 = m2]. *)

val normalize_wrt_zero : t -> t
  (** Allows to multiply or divide by any positive number since we consider
      that the monome is equal to (or compared with) zero.
      For integer monomes, the result will have co-prime coefficients. *)

val split : t -> t * t
  (** [split m] splits into a monome with positive coefficients, and one
      with negative coefficients.
      @return [m1, m2] such that [m = m1 - m2] and [m1,m2] both have positive
        coefficients *)

exception NotLinear
  
val of_term : FOTerm.t -> t
  (** try to get a monome from a term.
      @raise NotLinear if the term is not a proper monome. *)

val of_term_opt : FOTerm.t -> t option
  (** Exceptionless versionf of {!of_term} *)

val to_term : t -> FOTerm.t
  (** convert back to a term *)

val apply_subst : renaming:Substs.FO.Renaming.t ->
                  Substs.FO.t -> t -> Substs.scope -> t
  (** Apply a substitution to the monome's terms *)

val is_ground : t -> bool
  (** Are there no variables in the monome? *)

val reduce_same_factor : t -> t -> FOTerm.t -> t * t
  (** [reduce_same_factor m1 m2 t] multiplies [m1] and [m2] by
      some constants, so that their coefficient for [t] is the
      same.
      @raise Invalid_argument if [t] does not belong to [m1] or [m2] *)

val pp : Buffer.t -> t -> unit
val to_string : t -> string
val fmt : Format.formatter -> t -> unit

(** {2 Specific to Int} *)

val has_instances : t -> bool
  (** For real or rational, always true. For integers, returns true
      iff g divides [m.constant], where g is the
      GCD of [c] for [c] in [m.coeffs].

      The intuition is that this returns [true] iff the monome actually has
      some instances in its type. Trivially true in reals or rationals, this is
      only the case for integers if [m.coeffs + m.constant = 0] is a
      satisfiable diophantine equation. *)

(*
val floor : t -> t
  (** Highest monome that is <= m, and that satisfies [has_instances]. *)

val ceil : t -> t
  (** Same as {!round_low} but rounds high *)
*)

val quotient : t -> Symbol.t -> t option
  (** [quotient e c] tries to divide [e] by [c], returning [e/c] if
      it is still an integer expression.
      For instance, [quotient (2x + 4y) 2] will return [Some (x + 2y)] *)

val divisible : t -> Symbol.t -> bool
  (** [divisible e n] returns true if all coefficients of [e] are
      divisible by [n] and n is an int >= 2 *)

val factorize : t -> (t * Symbol.t) option
  (** Factorize [e] into [Some (e',s)] if [e = e' x s], None
      otherwise (ie if s=1) *)

(** {2 For fields (Q,R)} *)

val exact_quotient : t -> Symbol.t -> t
  (** Division in a field.
      @raise Division_by_zero if the denominator is zero. *)

(** {2 Modular Computations} *)

module Modulo : sig
  val modulo : n:Symbol.t -> Symbol.t -> Symbol.t
    (** Representative of the number in Z/nZ *)

  val sum : n:Symbol.t -> Symbol.t -> Symbol.t -> Symbol.t
    (** Sum in Z/nZ *)

  val uminus : n:Symbol.t -> Symbol.t -> Symbol.t
    (** Additive inverse in Z/nZ *)

  val inverse : n:Symbol.t -> Symbol.t -> Symbol.t
    (** Multiplicative inverse in Z/nZ.
        TODO (only works if [n] prime) *)
end

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

  val diophant_l : Big_int.big_int list -> Big_int.big_int ->
                   Big_int.big_int list * Big_int.big_int
  (** generalize diophantine equation solving to a list of at least two
      coefficients.
      @return a list of Bezout coefficients, and the
        GCD of the input list, or fails
      @raise Failure if the equation is not solvable *)

  val coeffs_n : Big_int.big_int list -> Big_int.big_int ->
                (FOTerm.t list -> t list)
    (** [coeffs_n l gcd], if [length l = n], returns a function that
        takes a list of [n-1] terms [k1, ..., k(n-1)] and returns a list of
        monomes [m1, ..., mn] that depend on [k1, ..., k(n-1)] such that the sum
        [l1 * m1 + l2 * m2 + ... + ln * mn = 0].

        {b Note} that the input list of the solution must have [n-1] elements,
        but that it returns a list of [n] elements!

        @param gcd is the gcd of all members of [l].
        @param l is a list of at least 2 elements, none of which should be 0 *)

  val eq_zero : ?fresh_var:(Type.t -> FOTerm.t) -> t -> solution list
    (** Returns substitutions that make the monome always equal to zero.
        Fresh variables may be generated using [fresh_var],
        for diophantine equations. Returns the empty list if no solution is
        found.

        For instance, on the monome 2X + 3Y - 7, it may generate a new variable
        Z and return the substitution  [X -> 3Z - 7, Y -> 2Z + 7] *)

  val lower_zero : ?fresh_var:(Type.t -> FOTerm.t) -> strict:bool ->
                   t -> solution list
    (** Solve for the monome to be always lower than zero ([strict] determines
        whether the inequality is strict or not). This
        may not return all solutions, but a subspace of it
        @param fresh_var see {!solve_eq_zero} *)

  val lt_zero : ?fresh_var:(Type.t -> FOTerm.t) -> t -> solution list
    (** Shortcut for {!lower_zero} when [strict = true] *)

  val leq_zero : ?fresh_var:(Type.t -> FOTerm.t) -> t -> solution list
    (** Shortcut for {!lower_zero} when [strict = false] *)

  val neq_zero : ?fresh_var:(Type.t -> FOTerm.t) -> t -> solution list
    (** Find some solutions that negate the equation. For now it
        just takes solutions to [m < 0].  *)
end

(** {2 Lib} *)

val bij : t Bij.t

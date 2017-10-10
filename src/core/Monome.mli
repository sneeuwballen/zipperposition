
(* This file is free software, part of Zipperposition. See file "license" for more details. *)

(** {1 Polynomes of order 1, over several variables}. *)

(** A monome is a linear expression on several "variables".

    We parametrize modules over some class of number (typically,
    Z or Q) that need to provide some operations. The ['a] parameter
    is the type of numbers ([Z.t] or [Q.t] from the library Zarith).

    Variables, in this module, are non-arithmetic terms, i.e. non-interpreted
    functions and predicates, that occur immediately under an arithmetic
    operator. For instance, in the term "f(X) + 1 + 3 × a", the variables
    are "f(X)" and "a", with coefficients "1" and "3".
*)

type term = Term.t

type 'a t
(** A monome over terms, with coefficient of type 'a *)

type 'a monome = 'a t

val equal : 'n t -> 'n t -> bool       (* structural equality *)
val compare : 'n t -> 'n t -> int   (* arbitrary total order on monomes *)
val hash : _ t -> int

val ty : _ t -> Type.t   (** type of the monome (int or rat) *)

val const : 'a t -> 'a   (** constant *)
val coeffs : 'a t -> ('a * term) list  (** coefficients *)

val find : 'a t -> term -> 'a option
val find_exn : 'a t -> term -> 'a (** @raise Not_found if not present *)
val mem : _ t -> term -> bool     (** Is the term in the monome? *)

val add : 'a t -> 'a -> term -> 'a t  (** Add term with coefficient. Sums coeffs. *)
val add_const : 'a t -> 'a -> 'a t    (** Add given number to constant *)
val remove : 'a t -> term -> 'a t     (** Remove the term *)
val remove_const : 'a t -> 'a t       (** Remove the constant *)

val add_list : 'a t -> ('a * term) list -> 'a t

val map : (term -> term) -> 'a t -> 'a t
val map_num : ('a -> 'a) -> 'a t -> 'a t

module Seq : sig
  val terms : _ t -> term Sequence.t
  val vars : _ t -> Term.var Sequence.t
  val coeffs : 'a t -> ('a * term) Sequence.t
  val coeffs_swap : 'a t -> (term * 'a) Sequence.t
end

val is_const : _ t -> bool
(** Returns [true] if the monome is only a constant *)

val is_zero : _ t -> bool
(** return [true] if the monome is the constant 0 *)

val sign : _ t -> int
(** Assuming [is_constant m], [sign m] returns the sign of [m].
    @raise Invalid_argument if the monome is not a constant *)

val size : _ t -> int
(** Number of distinct terms. 0 means that the monome is a constant *)

val terms : _ t -> term list
(** List of terms that occur in the monome with non-nul coefficients *)

val var_occurs : var:Term.var ->  _ t -> bool
(** Does the variable occur in the monome? *)

val sum : 'a t -> 'a t -> 'a t
val difference : 'a t -> 'a t -> 'a t
val uminus : 'a t -> 'a t
val product : 'a t -> 'a -> 'a t  (** Product with constant *)
val succ : 'a t -> 'a t           (** +1 *)
val pred : 'a t -> 'a t           (** -1 *)

val sum_list : 'a t list -> 'a t
(** Sum of a list.
    @raise Failure if the list is empty *)

val comparison : 'a t -> 'a t -> Comparison.t
(** Try to compare two monomes. They may not be comparable (ie on some
    points, or in some models, one will be bigger), but some pairs of
    monomes are:
    for instance, 2X + 1 < 2X + 4  is always true *)

val dominates : strict:bool -> 'a t -> 'a t -> bool
(** [dominates ~strict m1 m2] is true if [m1] is always greater than
    [m2], in any model or variable valuation.
    if [dominates ~strict:false m1 m2 && dominates ~strict:false m2 m1],
    then [m1 = m2].
    @argument strict if true, use "greater than", else "greater or equal". *)

val normalize : 'a t -> 'a t
(** Normalize the monome, which means that if some terms are
    rational or integer constants, they are moved to the constant part
    (e.g after apply X->3/4 in 2.X+1, one gets 2×3/4 +1. Normalization
    reduces this to 5/2). *)

val split : 'a t -> 'a t * 'a t
(** [split m] splits into a monome with positive coefficients, and one
    with negative coefficients.
    @return [m1, m2] such that [m = m1 - m2] and [m1,m2] both have positive
      coefficients *)

val apply_subst : Subst.Renaming.t -> Subst.t -> 'a t Scoped.t -> 'a t
(** Apply a substitution to the monome's terms.
    This does not preserve positions in the monome. *)

val apply_subst_no_simp : Subst.Renaming.t -> Subst.t -> 'a t Scoped.t -> 'a t
(** Apply a substitution to the monome's terms, without renormalizing.
    This preserves positions. *)

val variant : ?subst:Subst.t -> 'a t Scoped.t -> 'a t Scoped.t -> Subst.t Sequence.t

(** Matching and unification aren't complete in the presence of variables
    occurring directly under the sum, for this would require the variable
    to be bound to sums (monomes) itself in the general case.
    Instead, such variables are only bound to atomic terms, excluding
    constants (ie X+1 = a+1 will bind X to a without problem, but
    will X+a=a+1 will fail to bind X to 1) *)

val matching : ?subst:Subst.t -> 'a t Scoped.t -> 'a t Scoped.t -> Subst.t Sequence.t

val unify : ?subst:Unif_subst.t -> 'a t Scoped.t -> 'a t Scoped.t ->
  Unif_subst.t Sequence.t

val is_ground : _ t -> bool
(** Are there no variables in the monome? *)

val fold : ('a -> int -> 'b -> term -> 'a) -> 'a -> 'b t -> 'a
(** Fold over terms *)

val fold_max : ord:Ordering.t ->
  ('a -> int -> 'b -> term -> 'a) -> 'a -> 'b t -> 'a
(** Fold over terms that are maximal in the given ordering. *)

val nth : 'a t -> int -> ('a * term)
(** @raise Invalid_argument if the index is invalid *)

val set : 'a t -> int -> ('a * term) -> 'a t
(** @raise Invalid_argument if the index is invalid *)

val set_term : 'a t -> int -> term -> 'a t
(** @raise Invalid_argument if the index is invalid *)

(** Focus on a specific term *)
module Focus : sig
  type 'a t = {
    term : term;
    coeff : 'a;  (** Never 0 *)
    rest : 'a monome;
  }

  val get : 'a monome -> int -> 'a t
  (** @raise Invalid_argument if the index is invalid *)

  val focus_term : 'a monome -> term -> 'a t option
  (** Focus on the given term, if it is one of the members of
      the given monome. *)

  val focus_term_exn : 'a monome -> term -> 'a t
  (** Same as {!focus_term}, but
      @raise Failure on failure *)

  val to_monome : 'a t -> 'a monome
  (** Conversion back to an unfocused monome *)

  val coeff : 'a t -> 'a
  val term : 'a t -> term
  val rest : 'a t -> 'a monome

  val sum : 'a t -> 'a monome -> 'a t
  val difference : 'a t -> 'a monome -> 'a t
  val uminus : 'a t -> 'a t

  val product : 'a t -> 'a -> 'a t
  (** @raise Invalid_argument if the number is 0 *)

  val map : ?term:(term->term) -> ?coeff:('a -> 'a) ->
    ?rest:('a monome -> 'a monome) -> 'a t -> 'a t

  val scale : 'a t -> 'a t -> 'a t * 'a t
  (** Scale to the same coefficient *)

  val is_max : ord:Ordering.t -> _ t -> bool
  (** Is the focused term maximal in the monome? *)

  val fold_m : pos:Position.t -> 'a monome -> 'b ->
    ('b -> 'a t -> Position.t -> 'b) -> 'b
  (** Fold on terms of the given monome, focusing on them one by one,
      along with the position of the focused term *)

  val apply_subst : Subst.Renaming.t -> Subst.t -> 'a t Scoped.t -> 'a t
  (** Apply a substitution. This can modify the set of terms in [rest]
      because some of them may become equal to the focused term. *)

  (** Here we don't unify full (focused) monomes together, but only the
      focused part (and possibly some unfocused terms too) together.
      For instance, unifying
      [f(x)* + 2a] and [f(y)* + f(z) + b] (where focused terms are starred)
      will yield both
      [(1,1,x=y)] and [(1,2,x=y=z)] since [f(z)] becomes focused too.

      Again, arith constants are not unifiable with unshielded variables. *)

  val unify_ff : ?subst:Unif_subst.t ->
    'a t Scoped.t -> 'a t Scoped.t ->
    ('a t * 'a t * Unif_subst.t) Sequence.t
  (** Unify two focused monomes. All returned unifiers are unifiers
      of the focused terms, but maybe also of other unfocused terms;
      Focused monomes are modified by unification because several terms
      might merge with the focused term, so the new ones are
      returned with the unifier itself *)

  val unify_mm : ?subst:Unif_subst.t ->
    'a monome Scoped.t -> 'a monome Scoped.t ->
    ('a t * 'a t * Unif_subst.t) Sequence.t
  (** Unify parts of two monomes [m1] and [m2]. For each such unifier we
      return the versions of [m1] and [m2] where the unified terms
      are focused. *)

  val unify_self : ?subst:Unif_subst.t ->
    'a t Scoped.t -> ('a t * Unif_subst.t) Sequence.t
  (** Extend the substitution to other terms within the focused monome,
      if possible. For instance it might return
      [2f(x)+a, {x=y}] for the monome [f(x)+f(y)+a] where [f(x)] is focused. *)

  val unify_self_monome : ?subst:Unif_subst.t ->
    'a monome Scoped.t -> ('a t * Unif_subst.t) Sequence.t
  (** Unify at least two terms of the monome together *)

  (* TODO
     val unify_fm : ?subst:Subst.t ->
                 'a t Scoped.t -> 'a monome Scoped.t ->
                 ('a * 'a t * Subst.t) Sequence.t
     (** Unify a focused monome and an unfocused monome. All unifiers
        are unifiers of the focused term and of at least one of the
        terms of the opposite monome. Each result is the coeff of the
        left-focused term, the right-focused monome, and the substitution. *)
  *)

  val pp : _ t CCFormat.printer
end

val pp : _ t CCFormat.printer
val to_string : _ t -> string

val pp_tstp : _ t CCFormat.printer
val pp_zf : _ t CCFormat.printer

exception NotLinear

module Int : sig
  type t = Z.t monome

  val const : Z.t -> t (** Empty monomial, from constant (decides type) *)
  val singleton : Z.t -> term -> t  (** One term. *)
  val of_list : Z.t -> (Z.t * term) list -> t

  val of_term : term -> t option

  val of_term_exn : term -> t
  (** try to get a monome from a term.
      @raise NotLinear if the term is not a proper monome. *)

  val to_term : t -> term
  (** convert back to a term *)

  val has_instances : t -> bool
  (** For real or rational, always true. For integers, returns true
      iff g divides [m.constant], where g is the
      GCD of [c] for [c] in [m.coeffs].

      The intuition is that this returns [true] iff the monome actually has
      some instances in its type. Trivially true in reals or rationals, this is
      only the case for integers if [m.coeffs + m.constant = 0] is a
      satisfiable diophantine equation. *)

  val quotient : t -> Z.t -> t option
  (** [quotient e c] tries to divide [e] by [c], returning [e/c] if
      it is still an integer expression.
      For instance, [quotient (2x + 4y) 2] will return [Some (x + 2y)] *)

  val divisible : t -> Z.t -> bool
  (** [divisible e n] returns true if all coefficients of [e] are
      divisible by [n] and n is an int >= 2 *)

  val factorize : t -> (t * Z.t) option
  (** Factorize [e] into [Some (e',s)] if [e = e' x s], None
      otherwise (ie if s=1). In case it returns [Some (e', s)], [s > 1] holds *)

  val normalize_wrt_zero : t -> t
  (** Allows to multiply or divide by any positive number since we consider
      that the monome is equal to (or compared with) zero.
      For integer monomes, the result will have co-prime coefficients. *)

  val reduce_same_factor : t -> t -> term -> t * t
  (** [reduce_same_factor m1 m2 t] multiplies [m1] and [m2] by
      some constants, so that their coefficient for [t] is the same.
      @raise Invalid_argument if [t] does not belong to [m1] or [m2] *)

  val compare : (term -> term -> Comparison.t) -> t -> t -> Comparison.t
  (** Compare monomes as if they were multisets of terms, the coefficient
      in front of a term being its multiplicity. *)

  val to_multiset : t -> Multisets.MT.t
  (** Multiset of terms with multiplicity *)

  (** {2 Modular Computations} *)

  module Modulo : sig
    val modulo : n:Z.t -> Z.t -> Z.t
    (** Representative of the number in Z/nZ *)

    val sum : n:Z.t -> Z.t -> Z.t -> Z.t
    (** Sum in Z/nZ *)

    val uminus : n:Z.t -> Z.t -> Z.t
    (** Additive inverse in Z/nZ *)
  end

  (** {2 Find Solutions} *)

  module Solve : sig
    type solution = (term * t) list
    (** List of constraints (term = monome). It means that
        if all those constraints are satisfied, then a solution
        to the given problem has been found *)

    val split_solution : solution -> Subst.t * solution
    (** Split the solution into a variable substitution, and a
        list of constraints on non-variable terms *)

    val diophant2 : Z.t -> Z.t -> Z.t -> Z.t * Z.t * Z.t
    (** Find the solution vector for this diophantine equation, or fails.
        @return a triple [u, v, gcd] such that for all int [k],
        [u + b * k, v - a * k] is solution of equation [a * x + b * y = const].
        @raise Failure if the equation is unsolvable *)

    val diophant_l : Z.t list -> Z.t -> Z.t list * Z.t
    (** generalize diophantine equation solving to a list of at least two
        coefficients.
        @return a list of Bezout coefficients, and the
          GCD of the input list, or fails
        @raise Failure if the equation is not solvable *)

    val coeffs_n : Z.t list -> Z.t -> (term list -> t list)
    (** [coeffs_n l gcd], if [length l = n], returns a function that
        takes a list of [n-1] terms [k1, ..., k(n-1)] and returns a list of
        monomes [m1, ..., mn] that depend on [k1, ..., k(n-1)] such that the sum
        [l1 * m1 + l2 * m2 + ... + ln * mn = 0].

        {b Note} that the input list of the solution must have [n-1] elements,
        but that it returns a list of [n] elements!

        @param gcd is the gcd of all members of [l].
        @param l is a list of at least 2 elements, none of which should be 0 *)

    val eq_zero : ?fresh_var:(Type.t -> term) -> t -> solution list
    (** Returns substitutions that make the monome always equal to zero.
        Fresh variables may be generated using [fresh_var],
        for diophantine equations. Returns the empty list if no solution is
        found.

        For instance, on the monome 2X + 3Y - 7, it may generate a new variable
        Z and return the substitution  [X -> 3Z - 7, Y -> 2Z + 7] *)

    val lower_zero : ?fresh_var:(Type.t -> term) -> strict:bool ->
      t -> solution list
    (** Solve for the monome to be always lower than zero ([strict] determines
        whether the inequality is strict or not). This
        may not return all solutions, but a subspace of it
        @param fresh_var see {!solve_eq_zero} *)

    val lt_zero : ?fresh_var:(Type.t -> term) -> t -> solution list
    (** Shortcut for {!lower_zero} when [strict = true] *)

    val leq_zero : ?fresh_var:(Type.t -> term) -> t -> solution list
    (** Shortcut for {!lower_zero} when [strict = false] *)

    val neq_zero : ?fresh_var:(Type.t -> term) -> t -> solution list
    (** Find some solutions that negate the equation. For now it
        just takes solutions to [m < 0].  *)
  end
end

module Rat : sig
  type t = Q.t monome

  val const : Q.t -> t (** Empty monomial, from constant (decides type) *)
  val singleton : Q.t -> term -> t  (** One term. *)
  val of_list : Q.t -> (Q.t * term) list -> t

  val divide : t -> Q.t -> t
  (** Divide by non-zero constant *)

  val of_term : term -> t option

  val of_term_exn : term -> t
  (** try to get a monome from a term.
      @raise NotLinear if the term is not a proper monome. *)

  val to_term : t -> term
  (** convert back to a term *)

  val to_multiset : t -> Multisets.MT.t
  (** Multiset of terms *)
end

(** {2 For fields (Q,R)} *)

(*
val floor : t -> t
  (** Highest monome that is <= m, and that satisfies [has_instances]. *)

val ceil : t -> t
  (** Same as {!round_low} but rounds high *)

val exact_quotient : 'a t -> Symbol.t -> 'a t
  (** Division in a field.
      @raise Division_by_zero if the denominator is zero. *)
*)

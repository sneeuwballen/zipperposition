
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

  val is_arith : FOTerm.t -> bool
    (** Is the term arithmetic? *)

  val is_arith_const : FOTerm.t -> bool
    (** Is the term an arithmetic constant? *)

  val mk_sum : FOTerm.t -> FOTerm.t -> FOTerm.t
  val mk_difference : FOTerm.t -> FOTerm.t -> FOTerm.t
  val mk_product : FOTerm.t -> FOTerm.t -> FOTerm.t
  val mk_quotient : FOTerm.t -> FOTerm.t -> FOTerm.t
  val mk_uminus : FOTerm.t -> FOTerm.t

  val mk_less : FOTerm.t -> FOTerm.t -> FOTerm.t
  val mk_lesseq : FOTerm.t -> FOTerm.t -> FOTerm.t

  val extract_subterms : FOTerm.t -> FOTerm.t list
    (** If the term's root is an arithmetic expression, extract the
        list of outermost terms that occur immediately as parameters
        of the arithmetic expression. Returns [] if the term is not
        arithmetic or if it's a pure arithmetic expression
        (akin to a constant). *)

  val shielded : FOTerm.t -> FOTerm.t -> bool
    (** [shielded v t] is true if [v] is a variable that occurs under a
        non interpreted symbol in [t] *)

  val simplify : signature:Signature.t -> FOTerm.t -> FOTerm.t
    (** Arithmetic simplifications *)
end

(** {2 Formulas} *)

module F : sig
  val simplify : signature:Signature.t -> FOFormula.t -> FOFormula.t
    (** Simplify an arithmetic formula. In particular, it eliminates
        $greater and $greatereq, and simplifies subterms. *)
end

(** {2 View a Literal as an arithmetic Literal}.

    This module provides a given view of a literal, where some arith variable
    has been extracted *)

module Lit : sig
  type t = private
  | True   (* arithmetic tautology *)
  | False  (* arithmetic absurdity *)
  | Eq of FOTerm.t * Monome.t
  | Neq of FOTerm.t * Monome.t
  | L_less of FOTerm.t * Monome.t   (* term < monome *)
  | L_lesseq of FOTerm.t * Monome.t
  | R_less of Monome.t * FOTerm.t
  | R_lesseq of Monome.t * FOTerm.t

  val pp : Buffer.t -> t -> unit
  val to_string : t -> string

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

  val is_trivial : signature:Signature.t -> Literal.t -> bool
    (** Is the literal a tautology in arithmetic? *)

  val has_instances : signature:Signature.t -> Literal.t -> bool
    (** If the literal is arithmetic, return [true] iff it is compatible
        with the theory of arithmetic (e.g. X+2Y=3 is ok, but 1=2 is not).
        Otherwise return [true] *)

  val get_term : t -> FOTerm.t
    (** Extract the term.
        @raise Invalid_argument if the literal is [True] or [False] *)

  val get_monome : t -> Monome.t
    (** Extract the monome from the lit.
        @raise Invalid_argument if the literal is [True] or [False] *)

  val factor : t -> Substs.FO.t list
    (** Unify non-arith subterms pairwise, return corresponding substitutions *)

  val eliminate : ?elim_var:(FOTerm.t -> bool) -> signature:Signature.t ->
                  t -> Substs.FO.t list
    (** List of substitutions that make the literal inconsistent.
        [elim_var] is called to check whether eliminating a variable
        in an equation is possible. *)

  (** {3 Operations on Lists of literals} *)
  module L : sig
    val get_terms : t list -> FOTerm.t list

    val filter : t list -> (FOTerm.t -> Monome.t -> bool) -> t list
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

  val shielded : ?filter:(int -> Literal.t -> bool) ->
                  Literal.t array -> FOTerm.t -> bool
    (** Is the given variable shielded (ie occur as a subterm somewhere)?
        [filter] is used to know which literals can shield the variable.
        @raise Invalid_argument if the term is not a var *)

  val naked_vars : ?filter:(int -> Literal.t -> bool) ->
                    Literal.t array -> FOTerm.varlist
    (** Variables occurring in inequations, that are not shielded *)

end


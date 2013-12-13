
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

(** {1 Arithmetic Terms} *)

open Logtk

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

val shielded : var:FOTerm.t -> FOTerm.t -> bool
  (** [shielded ~var t] is true if [var] is a variable that occurs under a
      non interpreted symbol in [t] *)

val flag_simplified : int
  (** flag for simplified terms *)

val simplify : FOTerm.t -> FOTerm.t
  (** Arithmetic simplifications *)

(** {2 Formulas} *)

module Form : sig
  val simplify : FOFormula.t -> FOFormula.t
    (** Simplify an arithmetic formula. In particular, it eliminates
        $greater and $greatereq, and simplifies subterms. *)
end

(** {2 Utils} *)

val int_range : strict_low:bool -> strict_high:bool -> Big_int.big_int ->
                Big_int.big_int list
  (** enumerate integers from 0 to range. Bounds are excluded or included
      depending on params [strict_low] and [strict_high] (if true, bound is
      excluded). Returns an empty list is the range is empty. *)

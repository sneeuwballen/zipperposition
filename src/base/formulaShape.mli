
(*
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

(** {6 Detect some specific formulas} *)

type form = Formula.FO.t
type term = FOTerm.t

val is_definition : form -> (term * term) option
  (** Check whether the clause defines a symbol, e.g.
      subset(X,Y) = \forall Z(Z in X -> Z in Y). It means the LHS
      is a flat symbol with variables, and all variables in RHS
      are also in LHS *)

val is_pred_definition : form -> (term * form) option
  (** Check whether the formula is a predicate definition *)

val is_rewrite_rule : form -> (term * term) list
  (** More general than definition. It means the clause is an
      equality where all variables in RHS are also in LHS. It
      can return two rewrite rules if the clause can be oriented
      in both ways, e.g. associativity axiom. *)

val is_pred_rewrite_rule : form -> (term * form) option
  (** Rewriting rule for predicates *)

val is_const_definition : form -> (Symbol.t * term) option
  (** Checks whether the clause is "const = ground composite term", e.g.
      a clause "aIbUc = inter(a, union(b, c))". In this case it returns
      Some(constant, definition of constant) *)

val is_const_pred_definition : form -> (Symbol.t * form) option
  (** Definition of constant predicate *)

(** {2 Interface to Transform} *)

(** See module {! Transform}. By detecting some shapes in the input,
    for instance rewrite rules or term definitions, one can obtain
    interesting transformations *)

val detect : form Sequence.t -> Transform.t list
  (** Detect shapes in the given sequence, and convert them into
      transformations over formulas *)

val detect_list : form list -> Transform.t list

val detect_def : ?only:[`Pred|`Term] -> ?arity:[`Zero|`Nonzero] ->
                 form Sequence.t -> Transform.t list
  (** Detect definitions.
      @param only if present, restrict detection to predicates or terms
      @param arity if present, restrict to constant or non-constant symbols *)

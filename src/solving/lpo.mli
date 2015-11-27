
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

(** {1 Constraint Solving for LPO} *)

open Logtk

(** {6 Constraints} *)

module Constraint : sig
  type expr = Symbol.t

  type t =
    | EQ of expr * expr
    | LE of expr * expr
    | LT of expr * expr
    | And of t list
    | Or of t list
    | Not of t
    | True   (* tautology *)
    | False  (* impossible constraint *)

  val eq : expr -> expr -> t
  val neq : expr -> expr -> t
  val le : expr -> expr -> t
  val lt : expr -> expr -> t
  val gt : expr -> expr -> t
  val ge : expr -> expr -> t
  val and_ : t list -> t
  val or_ : t list -> t
  val not_ : t -> t
  val imply : t -> t -> t
  val true_ : t
  val false_ : t

  module Seq : sig
    val exprs : t -> expr Sequence.t
      (** Expressions that occur in the constraint *)
  end

  include LogtkInterfaces.PRINT with type t := t

  val simplify : t -> t
    (** Basic simplifications *)
end

(** {2 Solutions to constraint problems} *)

module Solution : sig
  type t = (Symbol.t * Symbol.t) list
    (** A precedence on symbol. Each pair means that thG
       first symbol is bigger than the second one. *)

  val neg_to_constraint : t -> Constraint.t
    (** Constraint that explicitely eliminate this solution *)

  include LogtkInterfaces.PRINT with type t := t
end

val solve_multiple : Constraint.t list -> Solution.t LazyList.t
  (** A lazy list of partial orders over symbols, that satisfy the given
      list of constraints *)

(** {6 Search for a LPO ordering} *)

module FO : sig
  type term = FOTerm.t

  val orient_lpo : term -> term -> Constraint.t
    (** [orient a b] generates a constraint that is sufficient for [a]
        to be bigger than [b] in LPO orderings satisfying the
        constraints *)

  val orient_lpo_list : (term * term) list -> Constraint.t list
    (** Orient a list of pairs *)
end

module STerm : sig
  type term = STerm.t

  val orient_lpo : term -> term -> Constraint.t
    (** [orient a b] generates a constraint that is sufficient for [a]
        to be bigger than [b] in LPO orderings satisfying the
        constraints *)

  val orient_lpo_list : (term * term) list -> Constraint.t list
    (** Orient a list of pairs *)
end

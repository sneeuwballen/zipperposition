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

(** {1 Types} *)

(** {2 Main Type representation}

Common representation of types, including higher-order
and polymorphic types. Types are hashconsed and all type variables
are assumed to be universally quantified in the outermost possible
scope (outside any other quantifier).

See {!TypeInference} for inferring types from terms and formulas,
and {!Signature} to associate types with symbols.
*)

exception Error of string
  (** Generic error on types. *)

(** {2 Main signature} *)

type symbol = Symbol.t

type t = private ScopedTerm.Std.t
(** Type is a subtype of the general structure ScopedTerm.t,
    with explicit conversion *)

type ty = t

type view = private
  | Var of int              (** Type variable *)
  | BVar of int             (** Bound variable (De Bruijn index) *)
  | App of symbol * t list  (** parametrized type *)
  | Fun of t * t list       (** Function type *)
  | Forall of t             (** explicit quantification using De Bruijn index *)

val view : t -> view
  (** Type-centric view of the head of this type.
      @raise Invalid_argument if the argument is not a type. *)

include Interfaces.HASH with type t := t
include Interfaces.ORD with type t := t

val is_var : t -> bool
val is_bvar : t -> bool
val is_app : t -> bool
val is_fun : t -> bool
val is_forall : t -> bool

(** {2 Constructors} *)

val tType : t
  (** Pseudo-type of types *)

val var : int -> t
  (** Build a type variable. The integer must be >= 0 *)

val app : symbol -> t list -> t
  (** Parametrized type *)

val const : symbol -> t
  (** Constant sort *)

val mk_fun : t -> t list -> t
  (** Function type. The first argument is the return type.
      see {!(<==)}. *)

val forall : t list -> t -> t
  (** [forall vars ty] quantifies [ty] over [vars].
      If [vars] is the empty list, returns [ty].
      @raise Invalid_argument if some element of [vars] is not a variable *)

val __forall : t -> t
  (** not documented. *)

val (@@) : string -> t list -> t
  (** [s @@ args] applies the sort [s] to arguments [args]. *)

val (<==) : t -> t list -> t
  (** General function type. [x <== l] is the same as [x] if [l]
      is empty. Invariant: the return type is never a function type. *)

val (<=.) : t -> t -> t
  (** Unary function type. [x <=. y] is the same as [x <== [y]]. *)

val of_term : ScopedTerm.Std.t -> t option
  (** Conversion from a term, if structure matches *)

(** {2 Containers} *)

module Set : Sequence.Set.S with type elt = t
module Map : Sequence.Map.S with type key = t
module Tbl : Hashtbl.S with type key = t

module Seq : sig
  val vars : t -> t Sequence.t
  val sub : t -> t Sequence.t
  val add_set : Set.t -> t Sequence.t -> Set.t
end

(** {2 Utils} *)

val vars_set : Set.t -> t -> Set.t
  (** Add the free variables to the given set *)

val vars : t -> t list
  (** List of free variables ({!Var}) *)

val close_forall : t -> t
  (** bind free variables *)

val arity : t -> int * int
  (** Number of arguments the type expects.
     If [arity ty] returns [a, b] that means that it
     expects [a] arguments to be used as arguments of Forall, and
     [b] arguments to be used for function application. *)

val expected_args : t -> t list
  (** Types expected as function argument by [ty]. The length of the
      list [expected_args ty] is the same as [snd (arity ty)]. *)

val is_ground : t -> bool
  (** Is the type ground? (means that no {!Var} not {!BVar} occurs in it) *)

val size : t -> int
  (** Size of type, in number of "nodes" *)

val apply : t -> t list -> t
  (** Given a function/forall type, and a list of arguments, return the
      type that results from applying the function/forall to the arguments.
      No unification is done, types must check exactly.
      @raise Error if the types do not match *)

(** {2 IO} *)

include Interfaces.PRINT with type t := t
(*
include Interfaces.SERIALIZABLE with type t := t
*)

(** {2 TPTP} specific printer and types *)

module TPTP : sig
  include Interfaces.PRINT with type t := t

  (** {2 Basic types} *)

  val i : t       (* individuals *)
  val o : t       (* propositions *)
  val tType : t   (* "type" of types *)

  val int : t
  val rat : t
  val real : t

  include Interfaces.PRINT with type t := t
end

(** {2 Misc} *)

val __var : int -> t
  (** Escape hatch to generate fresh variables with negative indexes.
      Use at your own risk... *)

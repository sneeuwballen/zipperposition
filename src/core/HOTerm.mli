
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

(** {1 Higher Order Terms}

    Both higher order formulas and terms are represented by terms. *)

(** {2 Term} *)

type t = private InnerTerm.t

type term = t

type var = Type.t HVar.t

type view = private
  | AppBuiltin of Builtin.t * t list
  | Var of var (** variable *)
  | DB of int (** bound variable (De Bruijn index) *)
  | Lambda of Type.t * t (** lambda abstraction over one variable. *)
  | Forall of Type.t * t (** Forall quantifier (commutes with other forall) *)
  | Exists of Type.t * t (** Exists quantifier (commutes with other exists) *)
  | Const of ID.t (** Typed constant *)
  | App of t * t list (** curried application *)
  | Multiset of Type.t * t list (** a multiset of terms, and their common type *)
  | Record of (string*t) list * var option (** Record of terms *)

val view : t -> view
val ty : t -> Type.t

val of_term_unsafe : InnerTerm.t -> t
(** {b NOTE}: caution, this can break invariants! Use only if you know what
    you are doing. *)

(** {2 Comparison, equality, containers} *)

val subterm : sub:t -> t -> bool
(** checks whether [sub] is a (non-strict) subterm of [t] *)

include Interfaces.HASH with type t := t
include Interfaces.ORD with type t := t

module Set : CCSet.S with type elt = t
module Map : CCMap.S with type key = t
module Tbl : CCHashtbl.S with type key = t

(** {2 Constructors}

    The constructors take care of type-checking.
    They may raise {!Type.ApplyError} in case of type error.
*)

val var : var -> t

val var_of_int : ty:Type.t -> int -> t
(** Create a variable. Providing a type is mandatory.
    The index must be non-negative,
    @raise Invalid_argument otherwise. *)

val bvar : ty:Type.t -> int -> t
(** Create a bound variable. Providing a type is mandatory.
    {b Warning}: be careful and try not to use this function directly*)

val app : t -> t list -> t
(** Curried application.
    @raise Type.ApplyError if types do not match. *)

val app_ty : t -> Type.t list -> t

val app_full : t -> Type.t list -> t list -> t
(** Combination of {!app} and {!app_ty} *)

val builtin : ty:Type.t -> Builtin.t -> t

val app_builtin : ty:Type.t -> Builtin.t -> t list -> t

val const : ty:Type.t -> ID.t -> t
(** Create a typed constant. *)

val record : (string*t) list -> rest:var option -> t
(** Build a record. All terms in the list must have the
    same type, and the rest (if present) must have a record() type.
    @param rest if present, must be either a variable, or a record.
    @raise Type.Error if types mismatch *)

val multiset : ty:Type.t -> t list -> t
(** Build a multiset. The [ty] argument is the type of the elements,
    in case the multiset is empty.
    @raise Type.Error if types mismatch *)

val lambda : varty:Type.t -> t -> t

val forall : varty:Type.t -> t -> t

val exists : varty:Type.t -> t -> t

val is_var : t -> bool
val is_bvar : t -> bool
val is_app : t -> bool
val is_const : t -> bool
val is_lambda : t -> bool
val is_forall : t -> bool
val is_exists : t -> bool
val is_multiset : t -> bool
val is_record : t -> bool

val of_term_unsafe : InnerTerm.t -> t
(** {b NOTE}: this can break the invariants and make {!view} fail. Only
    apply with caution. *)

module VarSet : CCSet.S with type elt = var
module VarMap : CCMap.S with type key = var
module VarTbl : CCHashtbl.S with type key = var

(** {2 Sequences} *)

module Seq : sig
  val vars : t -> var Sequence.t
  val subterms : t -> t Sequence.t
  val subterms_depth : t -> (t * int) Sequence.t  (* subterms with their depth *)
  val symbols : t -> ID.t Sequence.t
  val max_var : var Sequence.t -> int (** max var *)
  val min_var : var Sequence.t -> int (** min var *)
  val ty_vars : t -> var Sequence.t
  val add_set : Set.t -> t Sequence.t -> Set.t
end

val var_occurs : var:var -> t -> bool (** [var_occurs ~var t] true iff [var] in t *)
val is_ground : t -> bool (** is the term ground? (no free vars) *)
val monomorphic : t -> bool (** true if the term contains no type var *)
val max_var : VarSet.t -> int (** find the maximum variable index, or 0 *)
val min_var : VarSet.t -> int (** minimum variable, or 0 if ground *)
val add_vars : unit VarTbl.t -> t -> unit (** add variables of the term to the set *)
val vars : t Sequence.t -> VarSet.t (** compute variables of the terms *)
val vars_prefix_order : t -> var list (** variables in prefix traversal order *)
val depth : t -> int (** depth of the term *)
val head : t -> ID.t option (** head sym *)
val head_exn : t -> ID.t (** head sym (or Not_found) *)
val size : t -> int (** Size (number of nodes) *)

val ty_vars : t -> Type.VarSet.t
(** Set of free type variables *)

(** {2 Subterms and Positions} *)

module Pos : sig
  val at : t -> Position.t -> t
  (** retrieve subterm at pos
      @raise Invalid_argument if the position is invalid *)

  val replace : t -> Position.t -> by:t -> t
  (** [replace t pos ~by] replaces the subterm at position [pos]
      in [t] by the term [by]. The two terms should have the same type.
      @raise Invalid_argument if the position is not valid *)
end

val replace : t -> old:t -> by:t -> t
(** [replace t ~old ~by] syntactically replaces all occurrences of [old]
    in [t] by the term [by]. *)

(** {2 High-level operations} *)

val symbols : ?init:ID.Set.t -> t -> ID.Set.t (** IDs of the term (keys of signature) *)

val contains_symbol : ID.t -> t -> bool
(** Does the term contain this given ID.t? *)

(** {2 Conversion with {!FOTerm}} *)

val of_fo : FOTerm.t -> t
(** Convert from {!FOTerm}. Cannot fail. *)

val to_fo : t -> FOTerm.t option
(** Convert to the {!FOTerm} representation.
    @return None if the term contains untranslatable constructs *)

val is_fo : t -> bool
(** Check whether the term is convertible to a
    first-order term (no binders, no variable applied to something...) *)

(** {2 Various operations} *)

val close_forall : t -> t
(** Universally quantify all free variables
    @since 0.8 *)

val close_exists : t -> t
(** Existentially quantify all free variables
    @since 0.8 *)

val open_forall : ?offset:int -> t -> t
(** [open_forall t] removes all prenex "forall" quantifiers by replacing
    the De Bruijn indices with fresh free variables
    @param offset use this offset to generate fresh free variables
    @since 0.8 *)

(* TODO: move Lambda-calculus operators here *)

(** {2 IO}

    First, full functions with the amount of surrounding binders; then helpers in
    the case this amount is 0 (for instance in clauses)
*)

val print_all_types : bool ref

include Interfaces.PRINT with type t := t
include Interfaces.PRINT_DE_BRUIJN with type t := t
                                    and type term := t

val add_hook : print_hook -> unit

(** {2 TPTP} *)

module TPTP : sig
  include Interfaces.PRINT with type t := t
  include Interfaces.PRINT_DE_BRUIJN
    with type t := t and type term := t and type print_hook := print_hook

  val true_ : t (** tautology term *)
  val false_ : t (** antilogy term *)

  val not_ : t
  val and_ : t
  val or_ : t
  val imply : t
  val equiv : t
  val xor : t
  val forall : t
  val exists : t

  val eq : t
  val neq : t

  val mk_not : t -> t
  val mk_and : t -> t -> t
  val mk_or : t -> t -> t
  val mk_imply : t -> t -> t
  val mk_equiv : t -> t -> t
  val mk_xor : t -> t -> t
  val mk_eq : t -> t -> t
  val mk_neq : t -> t -> t

  val mk_and_list : t list -> t
  val mk_or_list : t list -> t
end

val debugf : Format.formatter -> t -> unit
(** debugf printing, with sorts *)

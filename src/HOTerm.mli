
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

type symbol = Symbol.t

type t = private ScopedTerm.t

type term = t

type view = private
  | Var of int                  (** variable *)
  | BVar of int             (** bound variable (De Bruijn index) *)
  | Lambda of t                 (** lambda abstraction over one variable. *)
  | Const of Symbol.t           (** Typed constant *)
  | App of t * Type.t list * t list
    (** HO function application. Invariant: first term is not a {!App}. *)

type sourced_term =
  t * string * string           (** Term + file,name *)

val view : t -> view

(** {2 Comparison, equality, containers} *)

val subterm : sub:t -> t -> bool
  (** checks whether [sub] is a (non-strict) subterm of [t] *)

include Interfaces.HASH with type t := t
include Interfaces.ORD with type t := t

val lambda_var_ty : t -> Type.t     (** Only on lambda terms. @raise Invalid_argument otherwise. *)

module Tbl : sig
  include Hashtbl.S with type key = t
  val to_list : unit t -> term list
  val from_list : term list -> unit t
  val to_seq : unit t -> term Sequence.t
  val from_seq : term Sequence.t -> unit t
  val add_list : unit t -> term list -> unit
  val add_seq : unit t -> term Sequence.t -> unit
end

module Set : Sequence.Set.S with type elt = t
module Map : Sequence.Map.S with type key = t

module Cache : Cache.S with type key = t

(** {2 Constructors}

The constructors take care of type-checking.
They may raise Type.Error in case of type error.

Use {!lambda} rather than {!__mk_lambda}, and try not to create bound
variables by yourself.
*)

val var : ty:Type.t -> int -> t
  (** Create a variable. Providing a type is mandatory.
      The index must be non-negative,
      @raise Invalid_argument otherwise. *)

val bvar : ty:Type.t -> int -> t
  (** Create a bound variable. Providing a type is mandatory.
      {b Warning}: be careful and try not to use this function directly*)

val app : ?tyargs:Type.t list -> t -> t list -> t
  (** Apply a typed symbol to a list of type arguments (optional) and
      a list of term arguments. Partial application is not
      supported and will raise a type error. The type is automatically
      computed.
      @raise Type.Error if types do not match. *)

val const : ?tyargs:Type.t list -> symbol -> t
  (** Create a constant. *)

val __mk_lambda : varty:Type.t -> t -> t    (** not documented *)
val __mk_forall : varty:Type.t -> t -> t
val __mk_exists : varty:Type.t -> t -> t

(** constructors with free variables. The first argument is the
    list of variables that is bound, then the quantified/abstracted
    term. *)

val mk_lambda : t list -> t -> t   (** (lambda v1,...,vn. t). *)
val mk_forall : t list -> t -> t
val mk_exists : t list -> t -> t

val cast : ty:Type.t -> t -> t
  (** Change the type. Only works for variables and bound variables. *)

val of_term : ScopedTerm.t -> t option
val is_term : ScopedTerm.t -> bool

val is_var : t -> bool
val is_bvar : t -> bool
val is_app : t -> bool
val is_const : t -> bool
val is_lambda : t -> bool

module Base : sig
  val true_ : t   (** tautology term *)
  val false_ : t  (** antilogy term *)

  val not_ : t
  val and_ : t
  val or_ : t
  val imply : t
  val equiv : t

  val eq : t
  val forall : t
  val exists : t

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


(** {2 Sequences} *)

module Seq : sig
  val vars : t -> t Sequence.t
  val subterms : t -> t Sequence.t
  val subterms_depth : t -> (t * int) Sequence.t  (* subterms with their depth *)
  val symbols : t -> Symbol.t Sequence.t
  val max_var : t Sequence.t -> int
  val min_var : t Sequence.t -> int
  val ty_vars : t -> Type.t Sequence.t

  val add_set : Set.t -> t Sequence.t -> Set.t
end

val var_occurs : var:t -> t -> bool     (** [var_occurs ~var t] true iff [var] in t *)
val is_ground : t -> bool               (** is the term ground? (no free vars) *)
val monomorphic : t -> bool             (** true if the term contains no type var *)
val max_var : Set.t -> int              (** find the maximum variable index, or 0 *)
val min_var : Set.t -> int              (** minimum variable, or 0 if ground *)
val add_vars : unit Tbl.t -> t -> unit  (** add variables of the term to the set *)
val vars : t Sequence.t -> Set.t        (** compute variables of the terms *)
val vars_prefix_order : t -> t list     (** variables in prefix traversal order *)
val depth : t -> int                    (** depth of the term *)
val head : t -> Symbol.t                (** head symbol (or Invalid_argument) *)
val size : t -> int                     (** Size (number of nodes) *)

val ty_vars : t -> Type.Set.t
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

  val at_cpos : t -> int -> t
    (** retrieve subterm at the compact pos, or raise Invalid_argument*)

  val max_cpos : t -> int
    (** maximum compact position in the term *)
end

val replace : t -> old:t -> by:t -> t
  (** [replace t ~old ~by] syntactically replaces all occurrences of [old]
      in [t] by the term [by]. *)

(** {2 High-level operations} *)

val symbols : ?init:Symbol.Set.t -> t -> Symbol.Set.t
  (** Symbols of the term (keys of signature) *)

val contains_symbol : Symbol.t -> t -> bool
  (** Does the term contain this given symbol? *)

val close_forall : t -> t
  (** Bind all free variables with 'forall' *)

val close_exists : t -> t
  (** Bind all free variables with 'exists' *)

(** {2 Conversion with {!FOTerm}} *)

val curry : FOTerm.t -> t
  (** Curry all subterms *)

val uncurry : t -> FOTerm.t
  (** Un-curry all subterms *)

val is_fo : t -> bool
  (** Check whether the term is convertible to a
      first-order term (no binders, no variable applied to something...) *)

(** {2 IO}

First, full functions with the amount of surrounding binders; then helpers in
the case this amount is 0 (for instance in clauses)
*)

val print_all_types : bool ref

val pp_depth : int -> Buffer.t -> t -> unit
val pp_tstp_depth : int -> Buffer.t -> t -> unit

val pp_debug : Buffer.t -> t -> unit
val pp_tstp : Buffer.t -> t -> unit


include Interfaces.PRINT with type t := t

(* TODO
include Interfaces.SERIALIZABLE with type t := t
*)

(** {2 TPTP} *)

module TPTP : sig
  include Interfaces.PRINT with type t := t
end

val debug : Format.formatter -> t -> unit
  (** debug printing, with sorts *)

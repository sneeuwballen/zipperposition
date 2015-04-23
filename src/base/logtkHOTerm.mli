
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

type symbol = LogtkSymbol.t

type t = private LogtkScopedTerm.t

type term = t

type view = private
  | Var of int                  (** variable *)
  | BVar of int                 (** bound variable (De Bruijn index) *)
  | Lambda of LogtkType.t * t   (** lambda abstraction over one variable. *)
  | Forall of LogtkType.t * t   (** Forall quantifier (commutes with other forall) *)
  | Exists of LogtkType.t * t   (** Exists quantifier (commutes with other exists) *)
  | Const of symbol             (** LogtkTyped constant *)
  | At of t * t                 (** Curried application *)
  | TyLift of LogtkType.t       (** Lift a type to a term *)
  | Multiset of LogtkType.t * t list (** a multiset of terms, and their common type *)
  | Record of (string*t) list * t option  (** Record of terms *)

val view : t -> view
val ty : t -> LogtkType.t
val kind : LogtkScopedTerm.Kind.t

val of_term : LogtkScopedTerm.t -> t option
val of_term_exn : LogtkScopedTerm.t -> t
val is_term : LogtkScopedTerm.t -> bool

(** {2 LogtkComparison, equality, containers} *)

val open_at : t -> t * LogtkType.t list * t list
  (** Open application recursively so as to gather all type arguments *)

val subterm : sub:t -> t -> bool
  (** checks whether [sub] is a (non-strict) subterm of [t] *)

include LogtkInterfaces.HASH with type t := t
include LogtkInterfaces.ORD with type t := t

val lambda_var_ty : t -> LogtkType.t
  (** Only on lambda terms: returns the type of the function argument.
     @raise Invalid_argument otherwise. *)

val cast : ty:LogtkType.t -> t -> t
  (** Change the type. Only works for variables and bound variables. *)

module Tbl : sig
  include Hashtbl.S with type key = term
  val to_list : 'a t -> (key * 'a) list
  val of_list : ?init:'a t -> (key * 'a) list -> 'a t
  val to_seq : 'a t -> (key * 'a) Sequence.t
  val of_seq : ?init:'a t -> (key * 'a) Sequence.t -> 'a t
end

module Set : Sequence.Set.S with type elt = t
module Map : Sequence.Map.S with type key = t

module LogtkCache : LogtkCache.S with type key = t

(** {2 Constructors}

The constructors take care of type-checking.
They may raise LogtkType.Error in case of type error.

Use {!lambda} rather than {!__mk_lambda}, and try not to create bound
variables by yourself.
*)

val var : ty:LogtkType.t -> int -> t
  (** Create a variable. Providing a type is mandatory.
      The index must be non-negative,
      @raise Invalid_argument otherwise. *)

val bvar : ty:LogtkType.t -> int -> t
  (** Create a bound variable. Providing a type is mandatory.
      {b Warning}: be careful and try not to use this function directly*)

val at : t -> t -> t
  (** Curried application. The first term must have a function type
      with the same argument as the type of the second term. Note that
      [at t1 t2] and [app t1 [t2]] are {b not} the same term.
      @raise LogtkType.Error if types do not match. *)

val at_list : t -> t list -> t
  (** Curried application to several terms, left-parenthesing.
      @raise LogtkType.Error of types do not match. *)

val tylift : LogtkType.t -> t
  (** [tylift ty] makes a term out of [ty]. It has type [Type.type_] *)

val tyat : t -> LogtkType.t -> t
  (** [tyat t ty] is the same as [at t (lifty ty)] *)

val tyat_list : t -> LogtkType.t list -> t
  (** Application to a list of types *)

val at_full : ?tyargs:LogtkType.t list -> t -> t list -> t
  (** Combination of {!at_list} and {!tyat_list} *)

val const : ty:LogtkType.t -> symbol -> t
  (** Create a typed constant. *)

val record : (string*t) list -> rest:t option -> t
  (** Build a record. All terms in the list must have the
      same type, and the rest (if present) must have a record() type.
      @param rest if present, must be either a variable, or a record.
      @raise LogtkType.Error if types mismatch *)

val multiset : ty:LogtkType.t -> t list -> t
  (** Build a multiset. The [ty] argument is the type of the elements,
      in case the multiset is empty.
      @raise LogtkType.Error if types mismatch *)

val __mk_lambda : varty:LogtkType.t -> t -> t    (** not documented *)
val __mk_forall : varty:LogtkType.t -> t -> t
val __mk_exists : varty:LogtkType.t -> t -> t

(** constructors with free variables. The first argument is the
    list of variables that is bound, then the quantified/abstracted
    term. *)

val lambda : t list -> t -> t   (** (lambda v1,...,vn. t). *)

val forall : t list -> t -> t

val exists : t list -> t -> t

val is_var : t -> bool
val is_bvar : t -> bool
val is_at : t -> bool
val is_tylift : t -> bool
val is_const : t -> bool
val is_lambda : t -> bool
val is_forall : t -> bool
val is_exists : t -> bool
val is_multiset : t -> bool
val is_record : t -> bool

(** {2 Sequences} *)

module Seq : sig
  val vars : t -> t Sequence.t
  val subterms : t -> t Sequence.t
  val subterms_depth : t -> (t * int) Sequence.t  (* subterms with their depth *)
  val symbols : t -> LogtkSymbol.t Sequence.t
  val max_var : t Sequence.t -> int
  val min_var : t Sequence.t -> int
  val ty_vars : t -> LogtkType.t Sequence.t

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
val head : t -> LogtkSymbol.t                (** head symbol (or Invalid_argument) *)
val size : t -> int                     (** Size (number of nodes) *)

val ty_vars : t -> LogtkType.Set.t
  (** Set of free type variables *)

(** {2 Subterms and LogtkPositions} *)

module Pos : sig
  val at : t -> LogtkPosition.t -> t
  (** retrieve subterm at pos
      @raise Invalid_argument if the position is invalid *)

  val replace : t -> LogtkPosition.t -> by:t -> t
  (** [replace t pos ~by] replaces the subterm at position [pos]
      in [t] by the term [by]. The two terms should have the same type.
      @raise Invalid_argument if the position is not valid *)
end

val replace : t -> old:t -> by:t -> t
  (** [replace t ~old ~by] syntactically replaces all occurrences of [old]
      in [t] by the term [by]. *)

(** {2 High-level operations} *)

val symbols : ?init:LogtkSymbol.Set.t -> t -> LogtkSymbol.Set.t (** LogtkSymbols of the term (keys of signature) *)

val contains_symbol : LogtkSymbol.t -> t -> bool
  (** Does the term contain this given symbol? *)

(** {2 Visitor} *)

class virtual ['a] any_visitor : object
  method virtual var : LogtkType.t -> int -> 'a
  method virtual bvar : LogtkType.t -> int -> 'a
  method virtual lambda : LogtkType.t -> 'a -> 'a
  method virtual forall : LogtkType.t -> 'a -> 'a
  method virtual exists : LogtkType.t -> 'a -> 'a
  method virtual const : LogtkType.t -> LogtkSymbol.t -> 'a
  method virtual at : 'a -> 'a -> 'a
  method virtual tylift : LogtkType.t -> 'a
  method virtual multiset : LogtkType.t -> 'a list -> 'a
  method virtual record : (string*'a) list -> 'a option -> 'a
  method visit : t -> 'a
end

class id_visitor : object
  method var : LogtkType.t -> int -> t
  method bvar : LogtkType.t -> int -> t
  method lambda : LogtkType.t -> t -> t
  method forall : LogtkType.t -> t -> t
  method exists : LogtkType.t -> t -> t
  method const : LogtkType.t -> LogtkSymbol.t -> t
  method at : t -> t -> t
  method tylift : LogtkType.t -> t
  method multiset : LogtkType.t -> t list -> t
  method record : (string*t) list -> t option -> t
  method visit : t -> t
end (** Visitor that maps the subterms into themselves *)

(** {2 Conversion with {!LogtkFOTerm}} *)

val curry : LogtkFOTerm.t -> t
  (** Curry all subterms *)

val uncurry : t -> LogtkFOTerm.t option
  (** Un-curry all subterms. If some subterms are not convertible to first-order
   * terms then [None] is returned. *)

val is_fo : t -> bool
  (** Check whether the term is convertible to a
      first-order term (no binders, no variable applied to something...) *)

(** {2 Various operations} *)

val close_forall : t -> t
(** Universally quantify all free variables
    @since NEXT_RELEASE *)

val close_exists : t -> t
(** Existentially quantify all free variables
    @since NEXT_RELEASE *)

val open_forall : ?offset:int -> t -> t
(** [open_forall t] removes all prenex "forall" quantifiers with fresh
    variables
    @param offset use this offset to generate fresh free variables
    @since NEXT_RELEASE *)

(* TODO: move Lambda-calculus operators here *)

(** {2 IO}

First, full functions with the amount of surrounding binders; then helpers in
the case this amount is 0 (for instance in clauses)
*)

val print_all_types : bool ref

include LogtkInterfaces.PRINT with type t := t
include LogtkInterfaces.PRINT_DE_BRUIJN with type t := t
    and type term := t

val add_hook : print_hook -> unit

(* TODO
include LogtkInterfaces.SERIALIZABLE with type t := t
*)

(** {2 TPTP} *)

module TPTP : sig
  include LogtkInterfaces.PRINT with type t := t
  include LogtkInterfaces.PRINT_DE_BRUIJN with type t := t
      and type term := t
      and type print_hook := print_hook

  val true_ : t   (** tautology term *)
  val false_ : t  (** antilogy term *)

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

val debug : Format.formatter -> t -> unit
(** debug printing, with sorts *)

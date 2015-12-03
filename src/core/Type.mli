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

    Types are represented using ScopedTerm, with kind Type. Therefore, they
    are hashconsed and scoped.

    Common representation of types, including higher-order
    and polymorphic types. All type variables
    are assumed to be universally quantified in the outermost possible
    scope (outside any other quantifier).

    See {!TypeInference} for inferring types from terms and formulas,
    and {!Signature} to associate types with symbols.

    TODO: think of a good way of representating AC operators (+, ...)
*)

exception Error of string
(** Generic error on types. *)

(** {2 Main signature} *)

type t = private ScopedTerm.t
(** Type is a subtype of the general structure ScopedTerm.t,
    with explicit conversion *)

type ty = t

type builtin = TType | Prop | Term

type view = private
  | Builtin of builtin
  | Var of HVar.t
  | DB of int
  | App of ID.t * t list (** parametrized type *)
  | Fun of t list * t (** Function type (left to right, no left-nesting) *)
  | Record of (string*t) list * HVar.t option (** Record type (+ variable) *)
  | Multiset of t
  | Forall of t (** explicit quantification using De Bruijn index *)

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
val prop : t
val term : t
val int : t
val rat : t

val var : int -> t
(** Build a type variable.
    @raise ScopedTerm.IllFormedTerm if the integer is negative *)

val app : ID.t -> t list -> t
(** Parametrized type *)

val const : ID.t -> t
(** Constant sort *)

val arrow : t -> t -> t
(** [arrow l r] is the type [l -> r]. *)

val arrow_list : t list -> t -> t
(** n-ary version of {!arrow} *)

val record : (string*t) list -> rest:HVar.t option -> t
(** Record type, with an optional extension *)

val __forall : t -> t
(** not documented. *)

val __bvar : int -> t
(** not documented. *)

val (@@) : ID.t -> t list -> t
(** [s @@ args] applies the sort [s] to arguments [args]. *)

val (<==) : t -> t list -> t
(** General function type. [x <== l] is the same as [x] if [l]
    is empty. Invariant: the return type is never a function type. *)

val (<=.) : t -> t -> t
(** Unary function type. [x <=. y] is the same as [x <== [y]]. *)

val multiset : t -> t
(** Type of multiset *)

val of_term_unsafe : ScopedTerm.t -> t
(** {b NOTE}: this can break the invariants and make {!view} fail. Only
    apply with caution. *)

val of_terms_unsafe : ScopedTerm.t list -> t list

(** {2 Containers} *)

module Set : Sequence.Set.S with type elt = t
module Map : Sequence.Map.S with type key = t
module Tbl : Hashtbl.S with type key = t

module Seq : sig
  val vars : t -> t Sequence.t
  val sub : t -> t Sequence.t
  val add_set : Set.t -> t Sequence.t -> Set.t
  val max_var : t Sequence.t -> int
end

(** {2 Utils} *)

val vars_set : Set.t -> t -> Set.t
(** Add the free variables to the given set *)

val vars : t -> t list
(** List of free variables ({!Var}) *)

val close_forall : t -> t
(** bind free variables *)

type arity_result =
  | Arity of int * int
  | NoArity

val arity : t -> arity_result
(** Number of arguments the type expects.
    If [arity ty] returns [Arity (a, b)] that means that it
    expects [a] arguments to be used as arguments of Forall, and
    [b] arguments to be used for function application. If
    it returns [NoArity] then the arity is unknown (variable) *)

val expected_args : t -> t list
(** Types expected as function argument by [ty]. The length of the
    list [expected_args ty] is the same as [snd (arity ty)]. *)

val is_ground : t -> bool
(** Is the type ground? (means that no {!Var} not {!BVar} occurs in it) *)

val size : t -> int
(** Size of type, in number of "nodes" *)

val depth : t -> int
(** Depth of the type (length of the longest path to some leaf)
    @since 0.5.3 *)

val open_fun : t -> (t list * t)
(** [open_fun ty] "unrolls" function arrows from the left, so that
    [open_fun (a -> (b -> (c -> d)))] returns [[a;b;c], d].
    @return the return type and the list of all its arguments *)

val apply : t -> t -> t
(** Given a function/forall type, and an argument, return the
    type that results from applying the function/forall to the arguments.
    No unification is done, types must check exactly.
    @raise Error if the types do not match *)

val apply_list : t -> t list -> t
(** List version of {!apply}
    @raise Error if the types do not match *)

(** {2 IO} *)

include Interfaces.PRINT_DE_BRUIJN with type term := t and type t := t
include Interfaces.PRINT with type t := t
val pp_surrounded : t CCFormat.printer

(*
include Interfaces.SERIALIZABLE with type t := t
*)

(** {2 TPTP} specific printer and types *)

module TPTP : sig
  include Interfaces.PRINT with type t := t
  include Interfaces.PRINT_DE_BRUIJN
    with type term := t and type t := t
                        and type print_hook := print_hook

  (** {2 Basic types} *)

  val i : t       (** individuals *)
  val o : t       (** propositions *)

  val int : t     (** integers *)
  val rat : t     (** rationals *)
  val real : t    (** reals *)
end

(** {2 Conversions} *)

(* TODO: deprecate, at least {!Conv.of_simple_term}
   now the proper way is through {!TypedSTerm} *)

module Conv : sig
  type ctx
  val create : unit -> ctx
  val copy : ctx -> ctx
  val clear : ctx -> unit

  val of_simple_term : ctx:ctx -> STerm.t -> t option
  val to_simple_term : ?curry:bool -> ?depth:int -> t -> STerm.t
  (** convert a type to a prolog term.
      @param depth the number of surrounding De Bruijn binders
      @param curry if true, keep types curried (default true), otherwise uncurry *)
end

(** {2 Misc} *)

val fresh_var : unit -> t
(** Fresh var, with negative index *)

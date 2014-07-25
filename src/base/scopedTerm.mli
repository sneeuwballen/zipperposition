
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

(** {1 Scoped Terms}

Those terms are not designed to be used directly, but rather to provide
a generic backend (implementing De Bruijn indices, subterms, substitutions,
etc.) for other more specific representations like Type, FOTerm, FOFormula...
*)

type symbol = Symbol.t

type t
  (** Abstract type of term *)

type term = t

type view = private
  | Var of int              (** Free variable (integer: mostly useless) *)
  | RigidVar of int         (** Variable that only unifies with other rigid variables *)
  | BVar of int             (** Bound variable (De Bruijn index) *)
  | Bind of symbol * t * t  (** Type, sub-term *)
  | Const of symbol         (** Constant *)
  | Record of (string * t) list * t option (** Extensible record *)
  | RecordGet of t * string       (** [get r name] is [r.name] *)
  | RecordSet of t * string * t   (** [set r name t] is [r.name <- t] *)
  | Multiset of t list      (** Multiset of terms *)
  | App of t * t list       (** Uncurried application *)
  | At of t * t             (** Curried application *)
  | SimpleApp of symbol * t list  (** For representing special constructors *)

val view : t -> view
  (** View on the term's head form *)

module Kind : sig
  (** "kind" of a term, i.e. what is its meaning, in which context is it
      used *)
  type t =
    | Kind
    | Type
    | FOTerm
    | HOTerm
    | Formula of t
    | Untyped
    | Generic  (* other terms *)
end

val kind : t -> Kind.t

type type_result =
  | NoType
  | HasType of t

val ty : t -> type_result
  (** Type of the term, if any *)

val ty_exn : t -> t
  (** Same as {!ty}, but fails if the term has no type
      @raise Invalid_argument if the type is [NoType] *)

include Interfaces.HASH with type t := t
include Interfaces.ORD with type t := t

(** {3 Constructors}

Some constructors, such as {!record}, may raise
{!IllFormedTerm}if the arguments are ill-formed (several occurrences of
a key), or, for variables, if the number is negative *)

exception IllFormedTerm of string
type nat = int

val const : kind:Kind.t -> ty:t -> symbol -> t
val app : kind:Kind.t -> ty:t -> t -> t list -> t
val bind : kind:Kind.t -> ty:t -> varty:t -> symbol -> t -> t
val var : kind:Kind.t -> ty:t -> nat -> t
val rigid_var : kind:Kind.t -> ty:t -> nat -> t
val bvar : kind:Kind.t -> ty:t -> nat -> t
val record : kind:Kind.t -> ty:t -> (string * t) list -> rest:t option -> t
val record_get : kind:Kind.t -> ty:t -> t -> string -> t
val record_set : kind:Kind.t -> ty:t -> t -> string -> t -> t
val multiset : kind:Kind.t -> ty:t -> t list -> t
val at : kind:Kind.t -> ty:t -> t -> t -> t
val simple_app : kind:Kind.t -> ty:t -> symbol -> t list -> t

val mk_at : kind:Kind.t -> ty:t -> t -> t -> t
  (** alias to {!at} *)

val tType : t
  (** The root of the type system. It doesn't have a type.
      It has kind [Kind.Type] *)

val cast : ty:t -> t -> t
  (** Change the type *)

val change_kind : kind:Kind.t -> t -> t
  (** Change the kind *)

val is_var : t -> bool
val is_bvar : t -> bool
val is_rigid_var : t -> bool
val is_const : t -> bool
val is_bind : t -> bool
val is_app : t -> bool
val is_record : t -> bool
val is_record_get : t -> bool
val is_record_set : t -> bool
val is_multiset : t -> bool
val is_at : t -> bool

val hashcons_stats : unit -> int*int*int*int*int*int

(** {3 Containers} *)

module Map : Sequence.Map.S with type key = term
module Set : Sequence.Set.S with type elt = term

module Tbl : sig
  include Hashtbl.S with type key = term
  val to_list : 'a t -> (key * 'a) list
  val of_list : ?init:'a t -> (key * 'a) list -> 'a t
  val to_seq : 'a t -> (key * 'a) Sequence.t
  val of_seq : ?init:'a t -> (key * 'a) Sequence.t -> 'a t
end

(** {3 De Bruijn indices handling} *)

module DB : sig
  type env = t DBEnv.t

  val closed : t -> bool
    (** check whether the term is closed (all DB vars are bound within the
        term). If this returns [true] then the term doesn't depend on
        its environment. *)

  val contains : t -> int -> bool
    (** Does t contains the De Bruijn variable of index n? *)

  val open_vars : t -> t Sequence.t
    (** List of "open" De Bruijn variables (with too high an index)
        @deprecated since 0.5.2, dangerous, use DBEnv functions instead *)

  val shift : int -> t -> t
    (** shift the non-captured De Bruijn indexes in the term by n *)

  val unshift : int -> t -> t
  (** [unshift n t] unshifts the term [t]'s bound variables by [n]. In
      other words it decrements indices of all free De Bruijn variables
      inside by [n]. Variables bound within [t] are left untouched. *)

  val replace : t -> sub:t -> t
    (** [db_from_term t ~sub] replaces [sub] by a fresh De Bruijn index in [t]. *)

  val from_var : t -> var:t -> t
    (** [db_from_var t ~var] replace [var] by a De Bruijn symbol in t.
        Same as {!replace}. *)

  val eval : env -> t -> t
    (** Evaluate the term in the given De Bruijn environment, by
        replacing De Bruijn indices by their value in the environment. *)
end

(** {3 High level constructors} *)

val bind_vars : kind:Kind.t -> ty:t -> symbol -> t list -> t -> t
  (** bind several free variables in the term, transforming it
      to use De Bruijn indices.
      @param ty return type of the term *)

(** {3 Iterators} *)

module Seq : sig
  val vars : t -> t Sequence.t
  val rigid_vars : t -> t Sequence.t
  val subterms : t -> t Sequence.t
  val subterms_depth : t -> (t * int) Sequence.t  (* subterms with their depth *)
  val symbols : t -> symbol Sequence.t
  val types : t -> t Sequence.t
  val max_var : t Sequence.t -> int
  val min_var : t Sequence.t -> int
  val add_set : Set.t -> t Sequence.t -> Set.t
  val add_tbl : unit Tbl.t -> t Sequence.t -> unit
end

(** {3 Positions} *)

module Pos : sig
  val at : t -> Position.t -> t
    (** retrieve subterm at pos, or raise Invalid_argument*)

  val replace : t -> Position.t -> by:t -> t
    (** replace t|_p by the second term *)
end

val replace : t -> old:t -> by:t -> t
  (** [replace t ~old ~by] syntactically replaces all occurrences of [old]
      in [t] by the term [by]. *)

(** {3 Variables} *)

val close_vars : kind:Kind.t -> ty:t -> symbol -> t -> t
  (** Close all free variables of the term using the binding symbol *)

val ground : t -> bool
  (** [true] if the term contains no free variables *)

(** {3 Misc} *)

val size : t -> int

val depth : t -> int

val head : t -> symbol option
  (** Head symbol, or None if the term is a (bound) variable *)

val all_positions : ?vars:bool -> ?pos:Position.t ->
                    t -> 'a ->
                    ('a -> t -> Position.t -> 'a) -> 'a
  (** Fold on all subterms of the given term, with their position.
      @param pos the initial position (prefix). Default: empty.
      @param vars if true, also fold on variables Default: [false].
      @return the accumulator *)

(** {2 IO} *)

include Interfaces.PRINT with type t := t
include Interfaces.PRINT_DE_BRUIJN with type t := t
  and type term := t

val add_default_hook : print_hook -> unit
  (** Add a print hook that will be used from now on *)

(** {2 Misc} *)

val fresh_var : kind:Kind.t -> ty:t -> unit -> t
(** [fresh_var ~kind ~ty ()] returns a fresh, unique, {b NOT HASHCONSED}
    variable that will therefore never be equal to any other variable. *)

val _var : kind:Kind.t -> ty:t -> int -> t
(** Unsafe version of {!var} that accepts negative index *)

(* FIXME
include Interfaces.SERIALIZABLE with type t := t
*)

(* TODO: path-selection operation (for handling general-data in TPTP), see
        XSLT or CSS *)

(* TODO: functor for scoping operation (and inverse) between
        ScopedTerm and NamedTerm *)

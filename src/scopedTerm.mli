
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

TODO: records!
*)

type symbol = Symbol.t

type t
  (** Abstract type of term *)

type term = t

type view = private
  | Var of int
  | BVar of int
  | Bind of symbol * t
  | Const of symbol
  | App of t * t list

val view : t -> view
  (** View on the term's head form *)

module Kind : sig
  (** "kind" of a term, i.e. what is its meaning, in which context is it
      used *)
  type t =
    | Const
    | Type
    | FOTerm
    | HOTerm
    | FOFormula
    | Generic  (* other terms *)
end

val kind : t -> Kind.t

val ty : t -> t
  (** Type of the term *)

include Interfaces.HASH with type t := t
include Interfaces.ORD with type t := t

(** {3 Constructors} *)

val const : kind:Kind.t -> ty:t -> symbol -> t
val app : kind:Kind.t -> ty:t -> t -> t list -> t
val bind : kind:Kind.t -> ty:t -> symbol -> t -> t
val var : kind:Kind.t -> ty:t -> int -> t
val bvar : kind:Kind.t -> ty:t -> int -> t

val tType : t
  (** The root of the type system. It's its own type, i.e.
      [ty tType == tType], and it has kind [`Type] *)

val cast : ty:t -> t -> t
  (** Change the type *)

val change_kind : kind:Kind.t -> t -> t
  (** Change the kind *)

val is_var : t -> bool
val is_bvar : t -> bool
val is_const : t -> bool
val is_bind : t -> bool
val is_app : t -> bool

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
  val closed : ?depth:int -> t -> bool
    (** check whether the term is closed (all DB vars are bound within
        the term) *)

  val contains : t -> int -> bool
    (** Does t contains the De Bruijn variable of index n? *)

  val shift : ?depth:int -> int -> t -> t
    (** shift the non-captured De Bruijn indexes in the term by n *)

  val unshift : ?depth:int -> int -> t -> t
    (** Unshift the term (decrement indices of all free De Bruijn variables
        inside) by [n] *)

  val replace : ?depth:int -> t -> sub:t -> t
    (** [db_from_term t ~sub] replaces [sub] by a fresh De Bruijn index in [t]. *)

  val from_var : ?depth:int -> t -> var:t -> t
    (** [db_from_var t ~var] replace [var] by a De Bruijn symbol in t.
        Same as {!replace}. *)

  val eval : t DBEnv.t -> t -> t
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

(** {3 IO} *)

include Interfaces.PRINT with type t := t

(* FIXME
include Interfaces.SERIALIZABLE with type t := t
*)

(* TODO: path-selection operation (for handling general-data in TPTP), see
        XSLT or CSS *)

(* TODO: functor for scoping operation (and inverse) between
        ScopedTerm and NamedTerm *)

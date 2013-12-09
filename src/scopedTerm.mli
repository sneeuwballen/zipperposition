
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

(** {1 Scoped Terms} *)

module type SYMBOL = sig
  type t

  include Interfaces.HASH with type t := t
  include Interfaces.ORD with type t := t
  include Interfaces.PRINT with type t := t
  include Interfaces.SERIALIZABLE with type t := t
end

module type DATA = sig
  type t

  val create : unit -> t
  val copy : t -> t
end

(** {2 Signature of a Term Representation}

Terms are optionally typed (in which case types are terms) *)

module type S = sig
  module Sym : SYMBOL

  module Data : DATA

  type t
    (** Abstract type of term *)

  type term = t

  type view = private
    | Var of int
    | BVar of int
    | Bind of Sym.t * t
    | Const of Sym.t
    | App of t * t list

  val view : t -> view
    (** View on the term's head form *)

  val ty : t -> t option
    (** Type of the term, if present *)

  val data : t -> Data.t
    (** Additional data for the term (unique to the term) *)

  val set_data : t -> Data.t -> unit
    (** Change data associated with the term *)

  include Interfaces.HASH with type t := t
  include Interfaces.ORD with type t := t

  (** {3 Bool flags} *)

  val flags : t -> int
  val new_flag : unit -> int
  val set_flag : t -> int -> bool -> unit
  val get_flag : t -> int -> bool

  (** {3 Constructors} *)

  val const : ?ty:t -> Sym.t -> t
  val app : ?ty:t -> t -> t list -> t
  val bind : ?ty:t -> Sym.t -> t -> t
  val var : ?ty:t -> int -> t
  val bvar : ?ty:t -> int -> t

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

  val bind_vars : ?ty:t -> Sym.t -> t list -> t -> t
    (** bind several free variables in the term, transforming it
        to use De Bruijn indices *)

  (** {3 Positions} *)

  val at_pos : t -> Position.t -> t 
    (** retrieve subterm at pos, or raise Invalid_argument*)

  val replace_pos : t -> Position.t -> t -> t
    (** replace t|_p by the second term *)

  val replace : t -> old:t -> by:t -> t
    (** [replace t ~old ~by] syntactically replaces all occurrences of [old]
        in [t] by the term [by]. *)

  (** {3 Variables} *)

  val vars_seq : t -> t Sequence.t
    (** Sequence of variables of the term. Each variable may occur several
        times in the sequence. *)

  val vars_set : ?init:unit Tbl.t -> t -> unit Tbl.t
    (** Add the variables of the term to a set (can be provided) *)

  val close_vars : ?ty:t -> Sym.t -> t -> t
    (** Close all free variables of the term using the binding symbol *)

  val ground : t -> bool
    (** true if the term contains no variables (either free or bounds) *)

  (** {3 IO} *)

  include Interfaces.PRINT with type t := t

  include Interfaces.SERIALIZABLE with type t := t
end

(** {2 Functors} *)

module UnitData : DATA with type t = unit
  (** Ignore data (trivial data) *)

module Make(Sym : SYMBOL) :
  S with module Sym = Sym and module Data = UnitData

module MakeHashconsed(Sym : SYMBOL) :
  S with module Sym = Sym and module Data = UnitData

module MakeData(Sym : SYMBOL)(Data : DATA) :
  S with module Sym = Sym and module Data = Data

module MakeHashconsedData(Sym : SYMBOL)(Data : DATA) :
  S with module Sym = Sym and module Data = Data

(* TODO: path-selection operation (for handling general-data in TPTP), see
        XSLT or CSS *)

(* TODO: functor for scoping operation (and inverse) between
        ScopedTerm and NamedTerm *)

(* TODO: functor for translating terms with a type of symbol to terms with
          another type of symbol *)


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

(** {1 Terms without type, typically produced from AST}

This module exports very simple and basic representations of terms
and formulas. Those representations are typically output by parsers
and should be transformed into more powerful representations
(see {!FOTerm}, {!HOTerm}, {!FOFormula}...) before use.
*)

(** {2 Symbols}

Raw, untyped symbols from the AST. *)

module Sym : sig
  type t = private
    | Int of Big_int.big_int
    | Rat of Ratio.ratio
    | Real of float
    | Const of string

  val eq : t -> t -> bool
  val cmp : t -> t -> int
  val hash : t -> int

  val mk_const : string -> t
  val mk_distinct : string -> t
  val mk_bigint : Big_int.big_int -> t
  val mk_int : int -> t
  val mk_rat : int -> int -> t
  val mk_ratio : Ratio.ratio -> t
  val mk_real : float -> t

  val parse_num : string -> t
    (** Parse an Int or a Rat *)

  val true_ : t
  val false_ : t
  val wildcard : t

  module Set : Sequence.Set.S with type elt = t
  module Map : Sequence.Map.S with type key = t

  val pp : Buffer.t -> t -> unit
  val fmt : Format.formatter -> t -> unit
  val to_string : t -> string
  val to_string_tstp : t -> string
end

(** {2 Type representation}

This module exports a very simple representation of types, typically
obtained right after parsing. No hashconsing is performed,
and variables are still strings.
*)

module Ty : sig
  type t = private
    | Var of string
    | App of string * t list
    | Fun of t * t list
    | Forall of t list * t

  val eq : t -> t -> bool
  val cmp : t -> t -> int
  val hash : t -> int

  val var : string -> t
  val app : string -> t list -> t
  val const : string -> t
  val mk_fun : t -> t list -> t
  val (<==) : t -> t list -> t
  val (<=.) : t -> t -> t

  val forall : t list -> t -> t
    (** the list of types must be a list of variables *)

  val is_var : t -> bool
  val is_fun : t -> bool
  val is_app : t -> bool
  val is_forall : t -> bool

  val i : t
  val o : t
  val int : t
  val rat : t
  val real : t
  val tType : t

  val pp : Buffer.t -> t -> unit
  val pp_tstp : Buffer.t -> t -> unit
  val to_string : t -> string
  val fmt : Format.formatter -> t -> unit
end

(** {2 First Order terms} *)

module FO : sig
  type t = private {
    term : tree;
    ty : Ty.t option;
    loc : Location.t option;
  }
  and tree = private
    | App of Sym.t * t list
    | Var of string

  val eq : t -> t -> bool
  val cmp : t -> t -> int
  val hash : t -> int

  val app : ?loc:Location.t -> Sym.t -> t list -> t
  val const : ?loc:Location.t -> Sym.t -> t
  val var : ?loc:Location.t -> ?ty:Ty.t -> string -> t

  val is_var : t -> bool
  val is_app : t -> bool

  val loc : t -> Location.t option
  val cast : t -> Ty.t -> t
  val get_ty : t -> Ty.t   (* obtain type of variables (always present) *)

  exception ExpectedType of t

  val as_ty : t -> Ty.t
    (** Interpret the term as a type.
        @raise ExpectedType if it's not possible (numeric symbols...) *)

  val symbols : t Sequence.t -> Sym.Set.t
  val free_vars : ?init:t list -> t -> t list

  val generalize_vars : t -> t
    (** Each variable gets its own type variable *)
  
  val pp : Buffer.t -> t -> unit
  val pp_tstp : Buffer.t -> t -> unit
  val to_string : t -> string
  val fmt : Format.formatter -> t -> unit
end

(** {2 First Order formulas} *)

module Form : sig
  type b_op =
    | Imply
    | Equiv

  type l_op =
    | And
    | Or

  type q_op =
    | Forall
    | Exists

  type t = private {
    form : tree;
    loc : Location.t option;
  }
  and tree = private
    | Nary of l_op * t list
    | Binary of b_op * t * t
    | Not of t
    | Bool of bool
    | Equal of FO.t * FO.t
    | Atom of FO.t
    | Quant of q_op * FO.t list * t

  type sourced = t * string * string
    (** Sourced formula *)

  val eq : t -> t -> bool
  val cmp : t -> t -> int
  val hash : t -> int

  val mk_and : ?loc:Location.t -> t list -> t
  val mk_or : ?loc:Location.t -> t list -> t
  val mk_not : ?loc:Location.t -> t -> t
  val mk_eq : ?loc:Location.t -> FO.t -> FO.t -> t
  val mk_neq : ?loc:Location.t -> FO.t -> FO.t -> t
  val mk_equiv : ?loc:Location.t -> t -> t -> t
  val mk_xor : ?loc:Location.t -> t -> t -> t
  val mk_imply : ?loc:Location.t -> t -> t -> t
  val atom : ?loc:Location.t -> FO.t -> t
  val forall : ?loc:Location.t -> FO.t list -> t -> t
  val exists : ?loc:Location.t -> FO.t list -> t -> t
  val mk_true : t
  val mk_false : t 

  val free_vars : t -> FO.t list

  val close_forall : t -> t
  val close_exists : t -> t

  val generalize_vars : t -> t
    (** See {!FO.generalize_vars} *)

  val loc : t -> Location.t option

  val pp : Buffer.t -> t -> unit
  val pp_tstp : Buffer.t -> t -> unit
  val to_string : t -> string
  val fmt : Format.formatter -> t -> unit
end

(** {2 Higher order Terms} *)

module HO : sig
  type t = private {
    term : tree;
    ty : Ty.t option;
    loc : Location.t option;
  }
  and tree = private
    | Const of Sym.t
    | App of t * t list
    | Var of string
    | Lambda of t * t

  val eq : t -> t -> bool
  val cmp : t -> t -> int
  val hash : t -> int

  val const : ?loc:Location.t -> Sym.t -> t
  val app : ?loc:Location.t -> t -> t list -> t
  val at : ?loc:Location.t -> t -> t -> t
  val var : ?loc:Location.t -> ?ty:Ty.t -> string -> t

  val cast : t -> Ty.t -> t
  val get_ty : t -> Ty.t   (* obtain type of variables (always present) *)

  exception ExpectedType of t

  val as_ty : t -> Ty.t
    (** Interpret the term as a type.
        @raise ExpectedType if the structure of the term doesn't fit *)

  val true_term : t
  val false_term : t

  val forall : ?loc:Location.t -> var:t -> t -> t
  val exists : ?loc:Location.t -> var:t -> t -> t
  val lambda : ?loc:Location.t -> var:t -> t -> t

  val forall_list : ?loc:Location.t -> t list -> t -> t
  val exists_list : ?loc:Location.t -> t list -> t -> t

  val of_term : FO.t -> t
  val of_form : Form.t -> t
  
  val pp : Buffer.t -> t -> unit
  val pp_tstp : Buffer.t -> t -> unit
  val to_string : t -> string
  val fmt : Format.formatter -> t -> unit
end

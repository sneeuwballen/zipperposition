
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

(** {1 Terms that do not enforce types, typically produced from AST}

This module exports very simple and basic representations of terms
and formulas. Those representations are typically output by parsers
and should be transformed into more powerful representations
(see {!FOTerm}, {!HOTerm}, {!FOFormula}...) before use.
*)

(** {2 First Order terms} *)

module FO : sig
  type t = private ScopedTerm.t
  type term = t

  val kind : ScopedTerm.Kind.t

  type view =
    | Var of int * Type.t
    | BVar of int * Type.t
    | Const of Symbol.t
    | App of t * t list

  val view : t -> view

  (** Since we are before type inference, most types are omitted
      and still unknown. Still, since every term must be typed,
      we use {!Symbol.Base.wildcard} liberally. *)

  val ty : t -> Type.t

  val is_term : ScopedTerm.t -> bool
  val of_term : ScopedTerm.t -> t option
  val of_term_exn : ScopedTerm.t -> t

  include Interfaces.HASH with type t:=t
  include Interfaces.ORD with type t:=t
  include Interfaces.PRINT with type t:=t

  val app : ?ty:Type.t -> t -> t list -> t
  val const : ?ty:Type.t -> Symbol.t -> t
  val var : ty:Type.t -> int -> t
  val bvar : ty:Type.t -> int -> t

  val is_var : t -> bool
  val is_app : t -> bool
  val is_bvar : t -> bool
  val is_const : t -> bool

  val size : t -> int

  exception ExpectedType of t

  val as_ty : t -> Type.t
    (** Interpret the term as a type.
        @raise ExpectedType if it's not possible (numeric symbols...) *)

  val of_ty : Type.t -> t
    (** Converse operation of {!as_ty}
        @raise Invalid_argument if the type has a forall/arrow inside *)

  module Seq : sig
    val symbols : t -> Symbol.t Sequence.t
    val vars : t -> t Sequence.t
  end

  module Set : Sequence.Set.S with type elt = t
end

(** {2 First Order formulas} *)

module Form : Formula.S with type term = FO.t


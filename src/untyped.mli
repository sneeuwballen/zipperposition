
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

XXX: add optional locations to terms/types, to allow better error report?
*)

(** {2 First Order terms} *)

module FO : sig
  type t = private
    | App of Symbol.t * t list
    | Var of string * Type.Parsed.t

  val eq : t -> t -> bool
  val cmp : t -> t -> int
  val hash : t -> int

  val app : Symbol.t -> t list -> t
  val const : Symbol.t -> t
  val var : ty:Type.Parsed.t -> string -> t

  val symbols : t Sequence.t -> Symbol.Set.t
  
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

  type t = private
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

  val mk_and : t list -> t
  val mk_or : t list -> t
  val mk_not : t -> t
  val mk_eq : FO.t -> FO.t -> t
  val mk_neq : FO.t -> FO.t -> t
  val mk_equiv : t -> t -> t
  val mk_xor : t -> t -> t
  val mk_imply : t -> t -> t
  val atom : FO.t -> t
  val forall : FO.t list -> t -> t
  val exists : FO.t list -> t -> t
  val mk_true : t
  val mk_false : t 

  val pp : Buffer.t -> t -> unit
  val pp_tstp : Buffer.t -> t -> unit
  val to_string : t -> string
  val fmt : Format.formatter -> t -> unit
end

(** {2 Higher order Terms} *)

module HO : sig
  type t = private
    | Const of Symbol.t
    | App of t * t list
    | Var of string * Type.Parsed.t
    | Lambda of t * t

  val eq : t -> t -> bool
  val cmp : t -> t -> int
  val hash : t -> int

  val const : Symbol.t -> t
  val app : t -> t list -> t
  val at : t -> t -> t
  val var : ty:Type.Parsed.t -> string -> t

  val forall : var:t -> t -> t
  val exists : var:t -> t -> t
  val lambda : var:t -> t -> t
  
  val pp : Buffer.t -> t -> unit
  val pp_tstp : Buffer.t -> t -> unit
  val to_string : t -> string
  val fmt : Format.formatter -> t -> unit
end

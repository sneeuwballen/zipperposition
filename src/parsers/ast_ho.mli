
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

(** {1 Very Simple AST}

AST that holds Horn Clauses and type declarations, nothing more. *)

open Logtk

type term = STerm.t

type t =
  | Clause of term * term list
  | Type of string * term

type location = ParseLocation.t

include Interfaces.PRINT with type t := t

module Seq : sig
  val terms : t -> term Sequence.t
  val vars : t -> term Sequence.t
end

module Term : sig
  val true_ : term
  val false_ : term
  val wildcard : term

  val var : ?loc:location -> ?ty:term -> string -> term
  val const : ?loc:location -> Symbol.t -> term
  val app : ?loc:location -> term -> term list -> term
  val record : ?loc:location -> (string * term) list -> rest:term option -> term
  val list_ : ?loc:location -> term list -> term

  val and_ : ?loc:location -> term list -> term
  val or_ : ?loc:location -> term list -> term
  val not_ : ?loc:location -> term -> term
  val equiv : ?loc:location -> term -> term -> term
  val xor : ?loc:location -> term -> term -> term
  val imply : ?loc:location -> term -> term -> term
  val eq : ?loc:location -> ?ty:term -> term -> term -> term
  val neq : ?loc:location -> ?ty:term -> term -> term -> term
  val forall : ?loc:location -> term list -> term -> term
  val exists : ?loc:location -> term list -> term -> term
  val lambda : ?loc:location -> term list -> term -> term

  val app_infix : ?loc:location -> string -> term -> term -> term
    (** Ad-hoc infix symbols constructor *)

  val mk_fun_ty : ?loc:location -> term list -> term -> term
  val lift_type : ?loc:location -> term -> term
  val tType : term
  val forall_ty : ?loc:location -> term list -> term -> term
end

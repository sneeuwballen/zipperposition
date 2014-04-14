
(*
Zipperposition: a functional superposition prover for prototyping
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

(** {6 Specifications of Built-in Theories} *)

open Logtk

type scope = Substs.scope
type term = FOTerm.t

(** TODO: theory of inductive types (e.g. lists, or finite domain types
          with only a few constructors);
          then, some case-reasoning inference over those inductive types *)

(** {2 Associativity-Commutativity} *)

module AC : sig
  type t = {
    sym : Symbol.t;
    ty : Type.t;
  }
end

(** {2 Total Ordering} *)

module TotalOrder : sig
  type t = {
    less : Symbol.t;
    lesseq : Symbol.t;
    ty : Type.t;  (** Type of the predicates *)
  } (** A single instance of total ordering. A proof is provided to
        justify why the symbols make a total ordering. *)

  type lit = {
    left : term;
    right : term;
    tyargs : Type.t list;
    strict : bool;
    order : t;
  } (** A literal is an atomic inequality. [strict] is [true] iff the
      literal is a strict inequality, and the ordering itself
      is also provided. *)

  val eq : t -> t -> bool
    (** Are two instances equal? In particular they must share the same type. *)

  val hash : t -> int

  val map : (term -> term) -> lit -> lit
  val apply_subst : renaming:Substs.Renaming.t -> Substs.t -> lit -> scope -> lit

  val less_const : t -> term  (** typed constant *)
  val lesseq_const : t -> term  (** typed constant *)

  val pp : Buffer.t -> t -> unit
  val to_string : t -> string
  val fmt : Format.formatter -> t -> unit
end

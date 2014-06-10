
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
  val neg : lit -> lit

  val less_const : t -> term  (** typed constant *)
  val lesseq_const : t -> term  (** typed constant *)

  val pp : Buffer.t -> t -> unit
  val to_string : t -> string
  val fmt : Format.formatter -> t -> unit
end

(** {2 Set Theory} *)

module Sets : sig
  type t = {
    member : Symbol.t;
    subset : Symbol.t;
    subseteq : Symbol.t;
    union : Symbol.t;
    inter : Symbol.t;
    diff : Symbol.t;
    emptyset : Symbol.t;
    singleton : Symbol.t;
    complement : Symbol.t;
    set_type : Symbol.t;
  }

  val signature : t -> Signature.t
  (** Obtain a signature from a specification (types are fixed) *)

  val default : t
  (** Default naming for TPTP axioms SET002.ax *)

  type view = private
    | Member of term * term
    | Subset of term * term
    | Subseteq of term * term
    | Union of term list
    | Inter of term list
    | Diff of term * term
    | Emptyset of Type.t
    | Singleton of term
    | Complement of term
    | Other of term  (** not a set constructor *)

  val view : sets:t -> term -> view
  (** View of the term as a set operator, if possible *)

  val is_set : sets:t -> term -> bool

  (** {4 constructors} *)
  val mk_member : sets:t -> term -> term -> term
  val mk_subset: sets:t -> term -> term -> term
  val mk_subseteq: sets:t -> term -> term -> term
  val mk_union: sets:t -> term list -> term
  val mk_inter: sets:t -> term list -> term
  val mk_diff: sets:t -> term -> term -> term
  val mk_empty: sets:t -> Type.t -> term
  val mk_singleton : sets:t -> term -> term
  val mk_complement : sets:t -> term -> term
  val mk_set_type : sets:t -> Type.t -> Type.t

  val print_hook : sets:t -> FOTerm.print_hook
end

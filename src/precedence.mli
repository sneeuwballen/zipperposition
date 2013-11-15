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

(** {1 Precedence (total ordering) on symbols} *)

type constr = Symbol.t -> Symbol.t -> int
  (** an ordering constraint (a possibly non-total ordering on symbols) *)

type  clause = FOFormula.t list
  (** Abstraction of a clause. It's only a list of terms. *)

type t = {
  prec_snapshot : Symbol.t list;  (** symbols in decreasing order *)
  prec_compare : Symbol.t -> Symbol.t -> int;       (** Compare symbols *)
  prec_weight : Symbol.t -> int;
  prec_set_weight : (Symbol.t -> int) -> t;
  prec_add_symbols : Symbol.t list -> t * int;
    (** add the given symbols to the precedenc (returns how many are new) *)
} (** A total ordering on symbols *)

val eq : t -> t -> bool
  (** Check whether the two precedences are equal (same snapshot) *)

val snapshot : t -> Symbol.t list
  (** Current list of symbols, in decreasing order *)

val compare : t -> Symbol.t -> Symbol.t -> int
  (** Compare two symbols using the precedence *)

val add_symbols : t -> Symbol.t list -> t
  (** Update the precedence with the given symbols *)

val add_signature : t -> Signature.t -> t
  (** Update the precedence with the symbols that are present in the
      signature *)

val pp_snapshot : Buffer.t -> Symbol.t list -> unit
val pp : Buffer.t -> t -> unit
val fmt : Format.formatter -> t -> unit
val to_string : t -> string

(** {2 Builtin constraints} *)

val cluster_constraint : Symbol.t list list -> constr
  (** ordering constraint by clustering symbols by decreasing order.
      all symbols in the first clusters are bigger than those in the second, etc. *)

val list_constraint : Symbol.t list -> constr
  (** symbols in the given list are in decreasing order *)

val arity_constraint : constr
  (** decreasing arity constraint *)

val invfreq_constraint : FOFormula.t Sequence.t -> constr
  (** symbols with high frequency are smaller *)

val max_constraint : Symbol.t list -> constr
  (** maximal symbols, in decreasing order *)

val min_constraint : Symbol.t list -> constr
  (** minimal symbols, in decreasing order *)

val alpha_constraint : constr
  (** regular (alphabetic) ordering on symbols *)

(** {2 Creation of a precedence from constraints} *)

val create : ?complete:bool -> constr list -> Symbol.t list -> t
  (** make a precedence from the given constraints. First constraints are
      more important than later constraints. Only the very first constraint
      is assured to be totally satisfied.
      
      If [complete] is true (default false) the symbol list is completed using
      special symbols. *)

val default : Symbol.t list -> t
  (** default precedence *)

val default_of_set : Symbol.Set.t -> t
  (** default precedence on the given set of symbols *)

val default_of_signature : Signature.t -> t
  (** default precedence on the given signature *)

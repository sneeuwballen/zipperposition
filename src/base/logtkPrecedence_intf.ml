
(*
Copyright (c) 2013-2014, Simon Cruanes
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

(** {1 Interface of LogtkPrecedence} *)

type symbol_status =
  | Multiset
  | Lexicographic

module type S = sig
  type symbol

  type t
    (** Total LogtkOrdering on a finite number of symbols, plus a few more
        data (weight for KBO, status for RPC) *)

  type precedence = t

  val eq : t -> t -> bool
    (** Check whether the two precedences are equal (same snapshot) *)

  val snapshot : t -> symbol list
    (** Current list of symbols, in decreasing order *)

  val compare : t -> symbol -> symbol -> int
    (** Compare two symbols using the precedence *)

  val mem : t -> symbol -> bool
    (** Is the symbol part of the precedence? *)

  val status : t -> symbol -> symbol_status
    (** Status of the symbol *)

  val weight : t -> symbol -> int
    (** Weight of a symbol (for KBO). Strictly positive int. *)

  val add_list : t -> symbol list -> t
    (** Update the precedence with the given symbols *)

  val add_seq : t -> symbol Sequence.t -> t

  val declare_status : t -> symbol -> symbol_status -> t
    (** Change the status of the given precedence *)

  module Seq : sig
    val symbols : t -> symbol Sequence.t
  end

  val pp_snapshot : Buffer.t -> symbol list -> unit
  val pp_debug : Buffer.t -> t -> unit
  val pp : Buffer.t -> t -> unit
  val fmt : Format.formatter -> t -> unit
  val to_string : t -> string

  (** {2 Builtin constraints} *)

  module Constr : sig
    type t = symbol -> symbol -> LogtkComparison.t
      (** A partial order on symbols, used to make the precedence more
          precise *)

    val cluster : symbol list list -> t
      (** ordering constraint by clustering symbols by decreasing order.
          all symbols in the first clusters are bigger than those in the second, etc. *)

    val of_list : symbol list -> t
      (** symbols in the given list are in decreasing order *)

    val of_precedence : precedence -> t
      (** Copy of another precedence on the common symbols *)

    val arity : (symbol -> int) -> t
      (** decreasing arity constraint (big arity => high in precedence) *)

    val invfreq : symbol Sequence.t -> t
      (** symbols with high frequency are smaller *)

    val max : symbol list -> t
      (** maximal symbols, in decreasing order *)

    val min : symbol list -> t
      (** minimal symbols, in decreasing order *)

    val alpha : t
      (** alphabetic ordering on symbols *)
  end

  type weight_fun = symbol -> int

  val weight_modarity : arity:(symbol -> int) -> weight_fun
  val weight_constant : weight_fun

  val set_weight : t -> weight_fun -> t
  (** Change the weight function of the precedence
      @since 0.5.3 *)

  (** {2 Creation of a precedence from constraints} *)

  val create : ?weight:weight_fun -> Constr.t list -> symbol list -> t
    (** make a precedence from the given constraints. Constraints near
        the head of the list are {b more important} than constraints close
        to the tail. Only the very first constraint is assured to be totally
        satisfied if constraints do not agree with one another. *)

  val create_sort : ?weight:weight_fun -> (int * Constr.t) list -> symbol list -> t
    (** Sort the list of constraints by {b increasing} priority, then
        call {!create} to build a precedence. The constraint with the smallest
        priority will be considered first.
        @since 0.6.1 *)

  val default : symbol list -> t
    (** default precedence. Default status for symbols is {!Lexicographic}. *)

  val default_seq : symbol Sequence.t -> t
    (** default precedence on the given sequence of symbols *)

  val constr_list : t -> Constr.t list
    (** Obtain the list of constraints
        @since 0.6.1 *)

  val with_constr_list : t -> Constr.t list -> t
    (** Update the precedence by replacing its list of constraints.
        Caution, this can be dangerous (change the precedence totally, for instance)
        @since 0.6.1 *)
end


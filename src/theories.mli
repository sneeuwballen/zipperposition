
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

(** {2 Associativity-Commutativity} *)

module AC : sig
  type t

  val create : unit -> t
    (** Create a new specification. *)

  val axioms : Symbol.t -> PFormula.t list
    (** Build axioms of AC for the given symbol *)

  val add : spec:t -> ?proof:Proof.t list -> Symbol.t -> unit
    (** Add the symbol to the list of AC symbols. A proof is needed to
        justify so (so that inference using the AC property will be able
        to justify it) *)

  val is_ac : spec:t -> Symbol.t -> bool
    (** Check whether the symbol is AC *)

  val find_proof : spec:t -> Symbol.t -> Proof.t list
    (** Recover the proof for the AC-property of this symbol.
        @raise Not_found if the symbol is not AC *)

  val exists_ac : spec:t -> bool
    (** Are some symbols AC? *)

  val symbols : spec:t -> Symbol.SSet.t
    (** set of AC symbols *)

  val symbols_of_terms : spec:t -> FOTerm.t Sequence.t -> Symbol.SSet.t
    (** set of AC symbols occurring in the given term *)

  val symbols_of_forms : spec:t -> FOFormula.t Sequence.t -> Symbol.SSet.t
    (** Set of AC symbols occurring in the given formula *)

  val proofs : spec:t -> Proof.t list
    (** All proofs for all AC axioms *)
end

(** {2 Total Ordering} *)

module TotalOrder : sig
  type instance = {
    less : Symbol.t;
    lesseq : Symbol.t;
    proof : Proof.t list;
  } (** A single instance of total ordering. A proof is provided to
        justify why the symbols make a total ordering. *)

  type t 

  type lit = {
    left : FOTerm.t;
    right : FOTerm.t;
    strict : bool;
    instance : instance;
  } (** A literal is an atomic inequality. [strict] is [true] iff the
      literal is a strict inequality, and the ordering itself
      is also provided. *)

  val create : ?base:bool -> unit -> t
    (** New specification. It already contains an instance
        for "$less" and "$lesseq" if [base] is true (default). *)

  val add : spec:t -> ?proof:Proof.t list ->
            less:Symbol.t -> lesseq:Symbol.t -> instance
    (** New instance of ordering.
        @raise Invalid_argument if one of the symbols is already part of an
              instance. *)

  val eq : instance -> instance -> bool

  val is_less : spec:t -> Symbol.t -> bool

  val is_lesseq : spec:t -> Symbol.t -> bool

  val find : spec:t -> Symbol.t -> instance
    (** Find the instance that corresponds to this symbol.
        @raise Not_found if the symbol is not part of any instance. *)

  val is_order_symbol : spec:t -> Symbol.t -> bool
    (** Is less or lesseq of some instance? *)

  val axioms : less:Symbol.t -> lesseq:Symbol.t -> PFormula.t list
    (** Axioms that correspond to the given symbols being a total ordering.
        The proof of the axioms will be "axiom" *)

  val tstp_instance : spec:t -> instance
    (** The specific instance that complies with TSTP signature $less, $lesseq *)

  val exists_order : spec:t -> bool
    (** Are there some known ordering instances? *)

  val pp_instance : Buffer.t -> instance -> unit
  val to_string_instance : instance -> string
  val fmt_instance : Format.formatter -> instance -> unit
end

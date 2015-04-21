
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

(** {1 Meta Prover for zipperposition} *)

open Logtk
open Logtk_meta

type 'a or_error = [`Ok of 'a | `Error of string]

type lemma = CompactClause.t * Proof.t (* a lemma *)
type axiom = string * Type.t list * HOTerm.t
type theory = string * Type.t list * HOTerm.t
type rewrite = (FOTerm.t * FOTerm.t) list (** Rewrite system *)
type pre_rewrite = HORewriting.t

(** {2 Result: Feedback from the meta-prover} *)

module Result : sig
  type t

  val lemmas : t -> lemma list
  (** Discovered lemmas *)

  val theories : t -> theory list
  (** Detected theories *)

  val axioms : t -> axiom list
  (** Additional axioms *)

  val rewrite : t -> rewrite list
  (** List of term rewrite systems *)

  val pre_rewrite : t -> pre_rewrite list
  (** Pre-processing rules *)

  val print : Format.formatter -> t -> unit
end

(** {2 Interface to the Meta-prover} *)

type t

val create : unit -> t
(** Fresh meta-prover *)

val results : t -> Result.t
(** Sum of all results obtained so far *)

val pop_new_results : t -> Result.t
(** Obtain the difference between last call to [pop_new_results p]
    and [results p], and pop this difference.
    [ignore (pop_new_results p); pop_new_results p] always
    returns the empty results *)

val theories : t -> theory Sequence.t
(** List of theories detected so far *)

val reasoner : t -> Logtk_meta.Reasoner.t
(** Meta-level reasoner (inference system) *)

val prover : t -> Logtk_meta.Prover.t
(** meta-prover  *)

val on_theory : t -> theory Signal.t
val on_lemma : t -> lemma Signal.t
val on_axiom : t -> axiom Signal.t
val on_rewrite : t -> rewrite Signal.t
val on_pre_rewrite : t -> pre_rewrite Signal.t

(** {2 Interface to {!Env} *)

val key : (string,t) Mixtbl.injection

val get_global : unit -> t
(** [get_global ()] returns the meta-prover saved in [Const.mixtbl],
    or create one if this is the first call to [get_global ()] *)

val clear_global : unit -> unit
(** Resets the meta-prover accessed using [get_global()] *)

module type S = sig
  module E : Env.S
  module C : module type of E.C

  val parse_theory_file : t -> string -> Result.t or_error
  (** Update prover with the content of this file, returns the new results
      or an error *)

  val parse_theory_files : t -> string list -> Result.t or_error
  (** Parse several files *)

  val scan_formula : t -> PFormula.t -> Result.t
  (** Scan a formula for patterns, and save it *)

  val scan_clause : t -> C.t -> Result.t
  (** Scan a clause for axiom patterns, and save it *)

  (** {2 Inference System} *)

  val setup : unit -> unit
  (** [setup ()] registers some inference rules to [E]
      and adds a meta-prover  *)
end

module Make(E : Env.S) : S with module E = E

val extension : Extensions.t
(** Prover extension *)

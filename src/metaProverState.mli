
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

type result =
  | Deduced of Term.t * Clause.t list
  | Theory of string * Term.t list
  | Expert of Experts.t
  (** Feedback from the meta-prover *)

type t

val create : ctx:Ctx.t -> MetaKB.t -> t
  (** Fresh meta-prover, using the given KB *)

val update_ctx : ctx:Ctx.t -> t -> unit
  (** Change the underlying context of the prover *)

val has_new_patterns : t -> bool
  (** Are there some new patterns that should be lookud up for in
      the active set? *)

val scan_clause : t -> Clause.t -> result list
  (** Scan a clause for patterns *)

val scan_set : t -> Clause.CSet.t -> result list
  (** Scan the set of clauses for patterns that are new. This should
      be called on the active set every time [has_new_patterns prover]
      returns true. After this, [has_new_patterns prover] returns false
      at least until the next call to [scan_clause]. *)

val theories : t -> (string * Term.t list) Sequence.t
  (** List of theories detected so far *)

val experts : t -> Experts.t Sequence.t
  (** Current list of experts that can be used *)

val results : t -> result Sequence.t
  (** All results *)

val reasoner : t -> MetaReasoner.t
  (** Datalog reasoner *)

val kb : t -> MetaKB.t
  (** Current knowledge base *)

val parse_theory_file : t -> string -> unit
  (** Update KB with the content of this file *)

val pp_result : Buffer.t -> result -> unit
val pp_theory : Buffer.t -> (string * Term.t list) -> unit

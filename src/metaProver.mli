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

(** {1 Meta-prover, working on axioms, theories and lemmas} *)

type t

(** {2 Basic operations and accessors} *)

val create : unit -> t
  (** New prover *)

val patterns : t -> MetaPattern.Set.t
  (** Set of active patterns *)

val kb : t -> MetaKB.t
  (** Knowledge base of the prover *)

val reasoner : t -> MetaReasoner.t
  (** Reasoner of the prover *)

val check_formulas : t -> Term.t Sequence.t -> unit
  (** Look for patterns in those formulas *)

val add_pattern : t -> MetaPattern.t -> unit
  (** New pattern to look for *)

val add_kb : t -> MetaKB.t -> unit
  (** Update the KB *)

(** {2 Basic events} *)

val on_lemma : t -> MetaKB.found_lemma Signal.t
val on_theory : t -> MetaKB.found_theory Signal.t
val on_axiom : t -> MetaKB.found_axiom Signal.t

(** {2 IO} *)

val parse_theory_file : t -> string -> unit
  (** Parse a theory file and update the KB *)

val save_kb : t -> string -> unit
val restore_kb : t -> string -> unit
